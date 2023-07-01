type register = A | B | C | D | SP | BP

type immediate_or_register =
  | Int of int
  | Label of string
  | Register of register

type src_dst = { src : immediate_or_register; dst : register }
type comparison = Eq | Ne | Lt | Le | Gt | Ge
type condition = { comparison : comparison; args : src_dst }

type t =
  | Mov of src_dst
  | Add of src_dst
  | Sub of src_dst
  | Load of src_dst
  | Store of { src : register; dst : immediate_or_register }
  | Putc of immediate_or_register
  | Getc of register
  | Exit
  | Jump of { target : immediate_or_register; condition : condition option }
  | Set of condition
  | Dump