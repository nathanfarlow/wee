type t = Instruction.t list

val compile : Elvm_program.t -> mem_size:int -> t
val to_string : t -> string