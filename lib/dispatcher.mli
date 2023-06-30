(* Generates instructions which jump to the tlvm address in B *)
(* translate maps elvm address -> tlvm address *)
val make_routine :
  Elvm_program.t -> translate:(int -> int) -> Symbolic_instruction.t list
