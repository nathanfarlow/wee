module Make (Resolver : Resolver_intf.S) : sig
  type t = Instruction.t list

  val compile : Elvm_program.t -> Resolver.t -> mem_size:int -> t
  val to_string : t -> string
end