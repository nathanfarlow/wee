open Core

type instruction =
  | Mov of int (* A = imm *)
  | Swap (* swap(A, B) *)
  | Add (* A = A + B *)
  | Sub (* A = A - B *)
  | Load (* A = mem[A] *)
  | Store (* mem[A] = B *)
  | Setlt (* A = (A < B) *)
  | Jmpz of int (* if A == 0 go to imm *)
  | Getc (* A = getchar() *)
  | Putc (* putchar(A) *)
  | Exit (* exit(A) *)

type t = instruction list

let compile _elvm _mem_size = failwith "not implemented"

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