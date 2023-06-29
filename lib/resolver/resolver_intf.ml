module type S = sig
  type t

  (* Outputs code which facilitates the resolver code. Ran once in beginning of program *)
  val make_initializer : t -> Elvm_program.t -> Symbolic_instruction.t list

  (* Outputs code which jumps to tlvm instruction corresponding to elvm address in B *)
  val make_resolver : t -> Elvm_program.t -> Symbolic_instruction.t list
end