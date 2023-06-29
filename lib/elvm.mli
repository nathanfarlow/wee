type t
type register = A | B | C | D | SP | BP

type immediate_or_register =
  | Int of int
  | Label of string
  | Register of register

type src_dst = { src : immediate_or_register; dst : register }
type comparison = Eq | Ne | Lt | Le | Gt | Ge
type condition = { comparison : comparison; args : src_dst }

type instruction =
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

exception Parse_error of string

type segment = Data | Text
type address = { segment : segment; offset : int }

val parse_exn : string -> t
val get_data : t -> int list
val get_instructions : t -> instruction list
val get_num_labels : t -> int
val resolve_label : t -> string -> address option