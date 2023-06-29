module Make () = struct
  type t = unit

  let create _elvm = ()

  (* val build : t -> translate:(int -> int) -> Tlvm_program.instruction list *)
  let build _ ~translate:_ = []
end
