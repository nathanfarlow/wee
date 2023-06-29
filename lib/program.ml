open Core
open Elvm_program

module Make (Resolver : Resolver_intf.S) = struct
  type t = Instruction.t list

  module Pseudo_register = struct
    type tlvm_register = TEMP
    type t = Elvm of Elvm_instruction.register | Tlvm of tlvm_register

    let to_addr = function
      | Elvm A -> 0
      | Elvm B -> 1
      | Elvm C -> 2
      | Elvm D -> 3
      | Elvm SP -> 4
      | Elvm BP -> 5
      | Tlvm TEMP -> 6

    let num_registers () = 7
  end

  type symbolic_arg =
    (* elvm instruction address *)
    | Elvm_text_addr of int
    (* elvm data address *)
    | Elvm_data_addr of int
    (* tlvm address relative to tlvm pc *)
    | Pc_relative of int
    (* constant value *)
    | Const of int
    (* base address of the resolver *)
    | Resolver_base
    (* address of a pseudo register *)
    | Pseudo_register_addr of Pseudo_register.t

  type symbolic_instruction =
    | Mov of symbolic_arg
    | Swap
    | Add
    | Sub
    | Load
    | Store
    | Setlt
    | Jmpz of symbolic_arg
    | Getc
    | Putc
    | Exit

  let init_data elvm =
    Mov (Elvm_data_addr 0)
    :: List.concat_map elvm.data ~f:(fun v ->
           [ Swap; Mov (Const v); Swap; Store; Mov (Const 1); Add ])

  let get_label_address_exn elvm label =
    let address = Hashtbl.find_exn elvm.labels label in
    match address.segment with
    | Data -> Elvm_data_addr address.offset
    | Text -> Elvm_text_addr address.offset

  (* A = pseudo register value. B is unchanged *)
  let load_pseudo register = [ Mov (Pseudo_register_addr register); Load ]

  (* store A into pseudo register. B is clobbered. *)
  let store_pseudo register =
    [ Swap; Mov (Pseudo_register_addr register); Store ]

  (* A = src. B is unchanged *)
  let read elvm src : symbolic_instruction list =
    let open Elvm_instruction in
    match src with
    | Int i -> [ Mov (Const i) ]
    | Register r -> load_pseudo (Elvm r)
    | Label l -> [ Mov (get_label_address_exn elvm l) ]

  (* dst = A. B is clobbered *)
  let write elvm dst : symbolic_instruction list =
    let open Elvm_instruction in
    match dst with
    | Int i -> [ Swap; Mov (Elvm_data_addr i); Store ]
    | Register r -> store_pseudo (Elvm r)
    | Label l -> [ Swap; Mov (get_label_address_exn elvm l); Store ]

  (* A = comparison of src and dst. B is clobbered. *)
  let rec make_comparison elvm comparison ~src ~dst =
    let set_a_lt_one = [ Swap; Mov (Const 1); Setlt ] in
    match comparison with
    | Elvm_instruction.Eq ->
        (* A = src *)
        read elvm src
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ read elvm dst
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
    | Elvm_instruction.Lt ->
        (* A = src *)
        read elvm src
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ read elvm dst
        (* A = (A < B) *)
        @ [ Setlt ]
    | Elvm_instruction.Gt ->
        (* A = (src < dst) *)
        make_comparison elvm Elvm_instruction.Lt ~src ~dst
        (* TEMP = A *)
        @ store_pseudo (Tlvm TEMP)
        (* A = (src == dst) *)
        @ make_comparison elvm Elvm_instruction.Eq ~src ~dst
        (* B = A *)
        @ [ Swap ]
        (* A = TEMP *)
        @ load_pseudo (Tlvm TEMP)
        (* A = A + B *)
        @ [ Add ]
        (* A = (A < 1), effectively A == 0 since A + B is 0, 1, or 2*)
        @ set_a_lt_one
    | Elvm_instruction.Ne ->
        (* A = (src == dst) *)
        make_comparison elvm Elvm_instruction.Eq ~src ~dst
        (* A = (A == 0) *)
        @ set_a_lt_one
    | Elvm_instruction.Le ->
        (* A = (src > dst) *)
        make_comparison elvm Elvm_instruction.Gt ~src ~dst
        (* A = (A == 0) *)
        @ set_a_lt_one
    | Elvm_instruction.Ge ->
        (* A = (src < dst) *)
        make_comparison elvm Elvm_instruction.Lt ~src ~dst
        (* A = (A == 0) *)
        @ set_a_lt_one

  let lower_instruction elvm instruction =
    match instruction with
    | Elvm_instruction.Mov { src; dst } ->
        (* A = src *)
        read elvm src
        (* dst = A *)
        @ write elvm (Register dst)
    | Elvm_instruction.Add { src; dst } ->
        (* A = src *)
        read elvm src
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ read elvm (Register dst)
        (* A = A + B *)
        @ [ Add ]
        (* dst = A *)
        @ write elvm (Register dst)
    | Elvm_instruction.Sub { src; dst } ->
        (* A = src *)
        read elvm src
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ read elvm (Register dst)
        (* A = A - B *)
        @ [ Sub ]
        (* dst = A *)
        @ write elvm (Register dst)
    | Elvm_instruction.Load { src; dst } ->
        (* A = src *)
        read elvm src
        (* A = mem[A] *)
        @ [ Load ]
        (* dst = A *)
        @ write elvm (Register dst)
    | Elvm_instruction.Store { src; dst } ->
        (* A = src *)
        read elvm (Register src)
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ read elvm dst
        (* mem[A] = B *)
        @ [ Store ]
    | Elvm_instruction.Putc src ->
        (* A = src *)
        read elvm src
        (* putc A *)
        @ [ Putc ]
    | Elvm_instruction.Getc dst ->
        (* A = getc *)
        [ Getc ]
        (* dst = A *)
        @ write elvm (Register dst)
    | Elvm_instruction.Exit -> [ Exit ]
    | Elvm_instruction.Jump { target; condition } ->
        let jump =
          match target with
          | Int i -> [ Mov (Const 0); Jmpz (Elvm_text_addr i) ]
          | Label l -> [ Mov (Const 0); Jmpz (get_label_address_exn elvm l) ]
          | Register r ->
              read elvm (Register r)
              @ [ Swap; Mov (Const 0); Jmpz Resolver_base ]
        in
        let comparison =
          match condition with
          | Some { comparison; args } ->
              (* A = (src ? dst) *)
              make_comparison elvm comparison ~src:args.src
                ~dst:(Register args.dst)
              (* don't jump if comparison failed *)
              @ [ Jmpz (Pc_relative (List.length jump + 1)) ]
          | None -> []
        in
        comparison @ jump
    | Elvm_instruction.Set { comparison; args } ->
        (* A = src ? dst *)
        make_comparison elvm comparison ~src:args.src ~dst:(Register args.dst)
        (* dst = A *)
        @ write elvm (Register args.dst)
    | Elvm_instruction.Dump -> []

  let compile elvm _jump_table ~mem_size =
    let num_registers = 6 in
    let init = init_data elvm in
    []

  (* high level strategy:
      mem:
        pseudo registers
        regular elvm data
        <- heap base
        ...
        <- sp

      instructions:
        init sp/bp
        init elvm data
        int 0:
          maybe this is add
          set A = A + B
        inst 1:
          maybe this is a jmp 5
          mov 0
          jmpz inst 5
        inst 2:
          maybe this is a jr
          A = src
          swap
          mov 0
          jmpz resolver
        resolver:
          if B == 0: goto inst 0
          if B == 1: goto inst 1
          ...
  *)

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