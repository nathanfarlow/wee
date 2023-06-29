module type S = sig
  type t

  val create : Elvm_program.t -> t
  val build : t -> translate:(int -> int) -> Tlvm_program.instruction list
end