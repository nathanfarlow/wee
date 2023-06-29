module type S = sig
  type t

  val build :
    t ->
    Elvm_program.t ->
    (* maps elvm address to tlvm address *)
    translate:(int -> int) ->
    Instruction.t list
end