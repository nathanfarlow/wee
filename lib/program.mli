module Make (Jump_table : Jump_table_intf.S) : sig
  type t = Instruction.t list

  val compile : Elvm_program.t -> Jump_table.t -> mem_size:int -> t
  val to_string : t -> string
end