(* creates a function which, given a wee address in B, jumps to it.
   elvm_to_wee maps elvm address -> wee address *)
val make_routine :
  Elvm_program.t -> elvm_to_wee:(int -> int) -> Symbolic_instruction.t list
