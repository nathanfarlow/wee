open Core
open Elvm_program

module Make (Jump_table : Jump_table_intf.S) = struct
  type t = Instruction.t list

  module Pseudo_register = struct
    type tlvm_register = TEMP | PC
    type t = Elvm of Elvm_instruction.register | Tlvm of tlvm_register

    let to_addr = function
      | Elvm A -> 0
      | Elvm B -> 1
      | Elvm C -> 2
      | Elvm D -> 3
      | Elvm SP -> 4
      | Elvm BP -> 5
      | Tlvm TEMP -> 6
      | Tlvm PC -> 7

    let num_registers () = 8
  end

  type immediate =
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

  type relative_instruction =
    | Mov of immediate
    | Swap
    | Add
    | Sub
    | Load
    | Store
    | Setlt
    | Jmpz of immediate
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

  (* A = pseudo register value *)
  let load_pseudo register = [ Mov (Pseudo_register_addr register); Load ]

  (* store A into pseudo register *)
  let store_pseudo register =
    [ Swap; Mov (Pseudo_register_addr register); Store ]

  (* load from src into A. B is unchanged *)
  let load elvm src : relative_instruction list =
    let open Elvm_instruction in
    match src with
    | Int i -> [ Mov (Const i) ]
    | Register r -> load_pseudo (Elvm r)
    | Label l -> [ Mov (get_label_address_exn elvm l); Load ]

  (* store A into dst. B is clobbered *)
  let store elvm dst : relative_instruction list =
    let open Elvm_instruction in
    match dst with
    | Int _ -> failwith "cannot store to immediate"
    | Register r -> store_pseudo (Elvm r)
    | Label l -> [ Swap; Mov (get_label_address_exn elvm l); Store ]

  (* A = comparison of src and dst. B is clobbered. *)
  let rec make_comparison elvm comparison ~src ~dst =
    let set_a_lt_one = [ Swap; Mov (Const 1); Setlt ] in
    match comparison with
    | Elvm_instruction.Eq ->
        (* A = src *)
        load elvm src
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ load elvm dst
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
        load elvm src
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ load elvm dst
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
        load elvm src
        (* dst = A *)
        @ store elvm (Register dst)
    | Elvm_instruction.Add { src; dst } ->
        (* A = src *)
        load elvm src
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ load elvm (Register dst)
        (* A = A + B *)
        @ [ Add ]
        (* dst = A *)
        @ store elvm (Register dst)
    | Elvm_instruction.Sub { src; dst } ->
        (* A = src *)
        load elvm src
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ load elvm (Register dst)
        (* A = A - B *)
        @ [ Sub ]
        (* dst = A *)
        @ store elvm (Register dst)
    | Elvm_instruction.Load { src; dst } ->
        (* A = src *)
        load elvm src
        (* A = mem[A] *)
        @ [ Load ]
        (* dst = A *)
        @ store elvm (Register dst)
    | Elvm_instruction.Store { src; dst } ->
        (* A = src *)
        load elvm (Register src)
        (* B = A *)
        @ [ Swap ]
        (* A = dst *)
        @ load elvm dst
        (* mem[A] = B *)
        @ [ Store ]
    | Elvm_instruction.Putc src ->
        (* A = src *)
        load elvm src
        (* putc A *)
        @ [ Putc ]
    | Elvm_instruction.Getc dst ->
        (* A = getc *)
        [ Getc ]
        (* dst = A *)
        @ store elvm (Register dst)
    | Elvm_instruction.Exit -> [ Exit ]
    | Elvm_instruction.Jump { target; condition } ->
        let jump =
          match target with
          | Int i -> [ Mov (Const 0); Jmpz (Elvm_text_addr i) ]
          | Label l -> [ Mov (Const 0); Jmpz (get_label_address_exn elvm l) ]
          | Register r ->
              load elvm (Register r)
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
        @ store elvm (Register args.dst)
    | Elvm_instruction.Dump -> []

  let compile elvm _jump_table ~mem_size:_ =
    let num_registers = 6 in
    let init = init_data elvm in
    []

  (* high level strategy:
      mem:
        pseudo registers
        regular elvm data
        <- heap base

      instructions:
        init elvm data
        loop:
          if pc == 0: goto inst 0
          if pc == 1: goto inst 1
          ...
        inst 1:
          maybe this is a jmp 5
          mov 0
          jmpz inst 5
        inst 2:
          maybe this is a jr
          set pc = target
          goto resolve
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