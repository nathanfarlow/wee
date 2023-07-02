type t = Instruction.t list

exception Parse_error of string

val parse_exn : string -> t

val run :
  t -> mem_size:int -> get_char:(unit -> int) -> put_char:(int -> unit) -> unit

val to_string : t -> string