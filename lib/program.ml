open Core

module Make (Jump_table : Jump_table_intf.S) = struct
  type t = Instruction.t list

  let init_data (elvm : Elvm_program.t) =
    let open Instruction in
    Mov 0
    :: List.concat_map elvm.data ~f:(fun v ->
           [ Swap; Mov v; Swap; Store; Mov 1; Add ])

  let compile elvm _jump_table ~mem_size:_ =
    let init_routine = init_data elvm in
    init_routine

  (* high level strategy:
      mem:
        regular elvm data
        <- heap base
        ...
        pseudo registers

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