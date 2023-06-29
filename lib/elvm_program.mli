open Core

type segment = Data | Text
type address = { segment : segment; offset : int }

type t = {
  data : int list;
  instructions : Elvm_instruction.t list;
  labels : (string, address) Hashtbl.t;
}

exception Parse_error of string

val parse_exn : string -> t