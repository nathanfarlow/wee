open Core

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

type segment = Data | Text
type address = { segment : segment; offset : int }

type t = {
  data : int list;
  instructions : instruction list;
  labels : (string, address) Hashtbl.t;
}

exception Parse_error of string

val parse_exn : string -> t