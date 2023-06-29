open Tlvm
open Core

let parse_elvm filename =
  let contents = In_channel.read_all filename in
  Elvm_program.parse_exn contents

let () =
  let filename =
    match Sys.get_argv () with
    | [| _; filename |] -> filename
    | _ -> failwith "usage: elvm <filename>"
  in
  let program = Program.compile (parse_elvm filename) ~mem_size:1000 in
  print_endline (Program.to_string program)
