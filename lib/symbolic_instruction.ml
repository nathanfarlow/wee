type tlvm_register = TEMP

type pseudo_register =
  | Elvm of Elvm_instruction.register
  | Tlvm of tlvm_register

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
  | Pseudo_register_addr of pseudo_register

type t =
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
