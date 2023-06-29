open Core
open Elvm_program
open Symbolic_instruction

module Make (Resolver : Resolver_intf.S) = struct
  type t = Instruction.t list

  let init_data program =
    Mov (Elvm_data_addr 0)
    :: List.concat_map program.data ~f:(fun v ->
           [ Swap; Mov (Const v); Swap; Store; Swap; Mov (Const 1); Add ])

  let get_label_address_exn program label =
    let address = Hashtbl.find_exn program.labels label in
    match address.segment with
    | Data -> Elvm_data_addr address.offset
    | Text -> Elvm_text_addr address.offset

  (* A = pseudo register value. B is unchanged *)
  let read_pseudo register = [ Mov (Pseudo_register_addr register); Load ]

  (* store A into pseudo register. B is clobbered. *)
  let write_pseudo register =
    [ Swap; Mov (Pseudo_register_addr register); Store ]

  let init_regs mem_size =
    (Mov (Const (mem_size - 1)) :: write_pseudo (Elvm SP))
    @ write_pseudo (Elvm BP)

  (* A = src. B is unchanged *)
  let read program src : Symbolic_instruction.t list =
    let open Elvm_instruction in
    match src with
    | Int i -> [ Mov (Const i) ]
    | Register r -> read_pseudo (Elvm r)
    | Label l -> [ Mov (get_label_address_exn program l) ]

  (* dst = A. B is clobbered *)
  let write program dst : Symbolic_instruction.t list =
    let open Elvm_instruction in
    match dst with
    | Int i -> [ Swap; Mov (Elvm_data_addr i); Store ]
    | Register r -> write_pseudo (Elvm r)
    | Label l -> [ Swap; Mov (get_label_address_exn program l); Store ]

  (* A = comparison of src and dst. B is clobbered. *)
  let rec make_comparison program comparison ~src ~dst =
    match comparison with
    | Elvm_instruction.Eq ->
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
    | Elvm_instruction.Ne ->
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
    | Elvm_instruction.Lt ->
        (* A = src *)
        read program src
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ read program dst
        (* A = (A < B) *)
        @ [ Setlt ]
    | Elvm_instruction.Le ->
        (* A = (src < dst) *)
        make_comparison program Elvm_instruction.Lt ~src ~dst
        (* TEMP = A *)
        @ write_pseudo (Tlvm TEMP)
        (* A = (src == dst) *)
        @ make_comparison program Elvm_instruction.Eq ~src ~dst
        (* B = A *)
        @ [ Swap ]
        (* A = TEMP *)
        @ read_pseudo (Tlvm TEMP)
        (* A = A + B *)
        @ [ Add ]
    | Elvm_instruction.Gt ->
        make_comparison program Elvm_instruction.Le ~src:dst ~dst:src
    | Elvm_instruction.Ge ->
        make_comparison program Elvm_instruction.Lt ~src:dst ~dst:src

  let lower_instruction program instruction =
    match instruction with
    | Elvm_instruction.Mov { src; dst } ->
        (* A = src *)
        read program src
        (* dst = A *)
        @ write program (Register dst)
    | Elvm_instruction.Add { src; dst } ->
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
    | Elvm_instruction.Sub { src; dst } ->
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
    | Elvm_instruction.Load { src; dst } ->
        (* A = src *)
        read program src
        (* A = mem[A] *)
        @ [ Load ]
        (* dst = A *)
        @ write program (Register dst)
    | Elvm_instruction.Store { src; dst } ->
        (* A = src *)
        read program (Register src)
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ read program dst
        (* mem[A] = B *)
        @ [ Store ]
    | Elvm_instruction.Putc src ->
        (* A = src *)
        read program src
        (* putc A *)
        @ [ Putc ]
    | Elvm_instruction.Getc dst ->
        (* A = getc *)
        [ Getc ]
        (* dst = A *)
        @ write program (Register dst)
    | Elvm_instruction.Exit -> [ Exit ]
    | Elvm_instruction.Jump { target; condition } ->
        let jump =
          match target with
          | Int i -> [ Mov (Const 0); Jmpz (Elvm_text_addr i) ]
          | Label l -> [ Mov (Const 0); Jmpz (get_label_address_exn program l) ]
          | Register r ->
              read program (Register r)
              @ [ Swap; Mov (Const 0); Jmpz Resolver_base ]
        in
        let comparison =
          match condition with
          | Some { comparison; args } ->
              (* A = (src ? dst) *)
              make_comparison program comparison ~src:args.src
                ~dst:(Register args.dst)
              (* don't jump if comparison failed *)
              @ [ Jmpz (Pc_relative (List.length jump + 1)) ]
          | None -> []
        in
        comparison @ jump
    | Elvm_instruction.Set { comparison; args } ->
        (* A = src ? dst *)
        make_comparison program comparison ~src:args.src
          ~dst:(Register args.dst)
        (* dst = A *)
        @ write program (Register args.dst)
    | Elvm_instruction.Dump -> []

  let relocate instructions ~resolve_elvm_text ~resolve_elvm_data
      ~resolve_register ~resolver_base =
    let resolve pc arg =
      match arg with
      | Elvm_text_addr i -> resolve_elvm_text i
      | Elvm_data_addr i -> resolve_elvm_data i
      | Pc_relative i -> pc + i
      | Const i -> i
      | Resolver_base -> resolver_base
      | Pseudo_register_addr r -> resolve_register r
    in
    List.mapi instructions ~f:(fun pc instruction : Instruction.t ->
        match instruction with
        | Mov arg -> Mov (resolve pc arg)
        | Add -> Add
        | Sub -> Sub
        | Load -> Load
        | Store -> Store
        | Putc -> Putc
        | Getc -> Getc
        | Exit -> Exit
        | Jmpz arg -> Jmpz (resolve pc arg)
        | Setlt -> Setlt
        | Swap -> Swap)

  let compile program resolver ~mem_size =
    let init =
      init_regs mem_size @ init_data program
      @ Resolver.make_initializer resolver program
    in
    let translated, resolve_elvm_text =
      let elvm_text_map = Hashtbl.create (module Int) in
      let tlvm_pc = ref 0 in
      let instructions =
        List.concat_mapi program.instructions ~f:(fun elvm_pc insn ->
            Hashtbl.add_exn elvm_text_map ~key:elvm_pc ~data:!tlvm_pc;
            let lowered = lower_instruction program insn in
            tlvm_pc := !tlvm_pc + List.length lowered;
            lowered)
      in
      (instructions, fun elvm_pc -> Hashtbl.find_exn elvm_text_map elvm_pc)
    in
    let resolver = Resolver.make_resolver resolver program in
    (* We will place the pseudo registers at the first 7 addresses *)
    let resolve_register = function
      | Elvm A -> 0
      | Elvm B -> 1
      | Elvm C -> 2
      | Elvm D -> 3
      | Elvm SP -> 4
      | Elvm BP -> 5
      | Tlvm TEMP -> 6
    in
    let resolve_elvm_data i = i + 7 in
    let resolver_base = List.length init + List.length translated in
    relocate
      (init @ translated @ resolver)
      ~resolve_elvm_text ~resolve_elvm_data ~resolve_register ~resolver_base

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
end