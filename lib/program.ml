open Core
open Elvm_program
open Elvm_instruction
open Symbolic_instruction

type t = Instruction.t list

let make_data_init program =
  Mov (Elvm_data_addr 0)
  :: List.concat_map program.data ~f:(fun v ->
         [ Swap; Mov (Const v); Swap; Store; Swap; Mov (Const 1); Add ])

let get_label_addr_exn program label =
  let address = Hashtbl.find_exn program.labels label in
  match address.segment with
  | Data -> Elvm_data_addr address.offset
  | Text -> Elvm_text_addr address.offset

(* A = pseudo register value. B is unchanged *)
let read_pseudo register = [ Mov (Pseudo_register_addr register); Load ]

(* store A into pseudo register. B is clobbered. *)
let write_pseudo register = [ Swap; Mov (Pseudo_register_addr register); Store ]

let make_stack_init mem_size =
  let init reg = Mov (Const (mem_size - 1)) :: write_pseudo (Elvm reg) in
  init SP @ init BP

(* A = src. B is unchanged *)
let read program src : Symbolic_instruction.t list =
  match src with
  | Int i -> [ Mov (Const i) ]
  | Register r -> read_pseudo (Elvm r)
  | Label l -> [ Mov (get_label_addr_exn program l) ]

(* dst = A. B is clobbered *)
let write program dst : Symbolic_instruction.t list =
  match dst with
  | Int i -> [ Swap; Mov (Elvm_data_addr i); Store ]
  | Register r -> write_pseudo (Elvm r)
  | Label l -> [ Swap; Mov (get_label_addr_exn program l); Store ]

(* A = comparison of src and dst. B is clobbered. *)
let rec make_comparison program comparison ~src ~dst =
  match comparison with
  | Eq ->
      (* A = src *)
      read program src
      (* B = A *)
      @ [ Swap ]
      (* A = dst *)
      @ read program dst
      @ [
          (* A = A - B *)
          Sub;
          (* if A is 0, jump to set A = 1 *)
          Jmpz (Pc_relative 3);
          (* otherwise, set A = 0 *)
          Mov (Const 0);
          (* jump to end *)
          Jmpz (Pc_relative 2);
          (* A = 1 *)
          Mov (Const 1);
        ]
  | Ne ->
      (* A = src *)
      read program src
      (* B = A *)
      @ [ Swap ]
      (* A = dst *)
      @ read program dst
      @ [
          (* A = A - B *)
          Sub;
          (* if A is 0, continue on *)
          Jmpz (Pc_relative 2);
          (* otherwise, set A = 1 *)
          Mov (Const 1);
        ]
  | Lt ->
      (* A = src *)
      read program src
      (* B = A *)
      @ [ Swap ]
      (* A = dst *)
      @ read program dst
      (* A = (A < B) *)
      @ [ Setlt ]
  | Le ->
      (* A = (src < dst) *)
      make_comparison program Lt ~src ~dst
      (* TEMP = A *)
      @ write_pseudo (Wee TEMP)
      (* A = (src == dst) *)
      @ make_comparison program Eq ~src ~dst
      (* B = A *)
      @ [ Swap ]
      (* A = TEMP *)
      @ read_pseudo (Wee TEMP)
      (* A = A + B *)
      @ [ Add ]
  | Gt -> make_comparison program Le ~src:dst ~dst:src
  | Ge -> make_comparison program Lt ~src:dst ~dst:src

let lower_instruction program (instruction : Elvm_instruction.t) =
  match instruction with
  | Mov { src; dst } ->
      (* A = src *)
      read program src
      (* dst = A *)
      @ write program (Register dst)
  | Add { src; dst } ->
      (* A = src *)
      read program src
      (* B = A *)
      @ [ Swap ]
      (* A = dst *)
      @ read program (Register dst)
      (* A = A + B *)
      @ [ Add ]
      (* dst = A *)
      @ write program (Register dst)
  | Sub { src; dst } ->
      (* A = src *)
      read program src
      (* B = A *)
      @ [ Swap ]
      (* A = dst *)
      @ read program (Register dst)
      (* A = A - B *)
      @ [ Sub ]
      (* dst = A *)
      @ write program (Register dst)
  | Load { src; dst } ->
      (* A = src *)
      read program src
      (* A = mem[A] *)
      @ [ Load ]
      (* dst = A *)
      @ write program (Register dst)
  | Store { src; dst } ->
      (* A = src *)
      read program (Register src)
      (* B = A *)
      @ [ Swap ]
      (* A = dst *)
      @ read program dst
      (* mem[A] = B *)
      @ [ Store ]
  | Putc src ->
      (* A = src *)
      read program src
      (* putc A *)
      @ [ Putc ]
  | Getc dst ->
      (* A = getc *)
      [ Getc ]
      (* dst = A *)
      @ write program (Register dst)
  | Exit -> [ Exit ]
  | Jump { target; condition } ->
      let jump =
        match target with
        | Int i -> [ Mov (Const 0); Jmpz (Elvm_text_addr i) ]
        | Label l -> [ Mov (Const 0); Jmpz (get_label_addr_exn program l) ]
        | Register r ->
            read program (Register r) @ [ Swap; Mov (Const 0); Jmpz Dispatcher ]
      in
      let comparison =
        match condition with
        | Some { comparison; args } ->
            (* A = (src ? dst) *)
            make_comparison program comparison ~src:args.src
              ~dst:(Register args.dst)
            (* skip jump if comparison failed *)
            @ [ Jmpz (Pc_relative (List.length jump + 1)) ]
        | None -> []
      in
      comparison @ jump
  | Set { comparison; args } ->
      (* A = src ? dst *)
      make_comparison program comparison ~src:args.src ~dst:(Register args.dst)
      (* dst = A *)
      @ write program (Register args.dst)
  | Dump -> []

let resolve instructions ~resolve_elvm_text ~resolve_elvm_data ~resolve_register
    ~dispatcher_base =
  let resolve' pc arg =
    match arg with
    | Elvm_text_addr i -> resolve_elvm_text i
    | Elvm_data_addr i -> resolve_elvm_data i
    | Pc_relative i -> pc + i
    | Const i -> i
    | Dispatcher -> dispatcher_base
    | Pseudo_register_addr r -> resolve_register r
  in
  List.mapi instructions ~f:(fun pc instruction : Instruction.t ->
      match instruction with
      | Mov arg -> Mov (resolve' pc arg)
      | Add -> Add
      | Sub -> Sub
      | Load -> Load
      | Store -> Store
      | Putc -> Putc
      | Getc -> Getc
      | Exit -> Exit
      | Jmpz arg -> Jmpz (resolve' pc arg)
      | Setlt -> Setlt
      | Swap -> Swap)

let lower_instructions program base_address =
  let address_mapping = Hashtbl.create (module Int) in
  let wee_pc = ref 0 in
  let lowered =
    List.concat_mapi program.instructions ~f:(fun elvm_pc insn ->
        Hashtbl.add_exn address_mapping ~key:elvm_pc ~data:!wee_pc;
        let lowered = lower_instruction program insn in
        wee_pc := !wee_pc + List.length lowered;
        lowered)
  in
  let resolve_elvm_text elvm_pc =
    let offset =
      Hashtbl.find address_mapping elvm_pc
      |> Option.value_exn
           ~message:
             "referencing a label at the end of the program doesn't make \
              sense. did you forget an exit instruction?"
    in
    List.length base_address + offset
  in
  (lowered, resolve_elvm_text)

let compile program ~mem_size =
  let init = make_stack_init mem_size @ make_data_init program in
  let lowered, resolve_elvm_text = lower_instructions program init in
  let dispatcher =
    Dispatcher.make_routine program ~elvm_to_wee:resolve_elvm_text
  in
  (* we will place the pseudo registers at the first 7 words of memory
     followed by the elvm data *)
  let resolve_register = function
    | Elvm A -> 0
    | Elvm B -> 1
    | Elvm C -> 2
    | Elvm D -> 3
    | Elvm SP -> 4
    | Elvm BP -> 5
    | Wee TEMP -> 6
  in
  let resolve_elvm_data i = i + 7 in
  let dispatcher_base = List.length init + List.length lowered in
  resolve
    (init @ lowered @ dispatcher)
    ~resolve_elvm_text ~resolve_elvm_data ~resolve_register ~dispatcher_base

let to_string t =
  let open Instruction in
  let insn_to_string = function
    | Mov i -> "mov " ^ string_of_int i
    | Swap -> "swap"
    | Add -> "add"
    | Sub -> "sub"
    | Load -> "load"
    | Store -> "store"
    | Setlt -> "setlt"
    | Jmpz i -> "jmpz " ^ string_of_int i
    | Getc -> "getc"
    | Putc -> "putc"
    | Exit -> "exit"
  in
  String.concat ~sep:"\n" (List.map t ~f:insn_to_string)
