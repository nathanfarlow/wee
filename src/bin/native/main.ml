open Wee
open Core

let () =
  let filename, mem_size =
    match Sys.get_argv () with
    | [| _; filename; mem_size |] -> (filename, Int.of_string mem_size)
    | _ -> failwith "usage: wee <file.elvm> <memory size>"
  in
  let elvm = Elvm_program.parse_exn @@ In_channel.read_all filename in
  let program = Compiler.f elvm ~mem_size in
  print_endline @@ Program.to_string program
