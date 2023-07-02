open Core
open Wee
open Js_of_ocaml

let compile elvm mem_size =
  let elvm = Elvm_program.parse_exn elvm in
  Compiler.f elvm ~mem_size

let run wee mem_size =
  let output = ref "" in
  let put_char c =
    let c = Char.of_int_exn (c mod 256) in
    output := !output ^ String.make 1 c
  in
  let get_char () = failwith "getc instruction not supported in web mode" in
  Program.run wee ~mem_size ~get_char ~put_char;
  !output

let () =
  Js.Unsafe.global##.compileElvmToWee := Js.wrap_callback compile;
  Js.Unsafe.global##.weeToString := Js.wrap_callback Program.to_string;
  Js.Unsafe.global##.parseWee := Js.wrap_callback Program.parse_exn;
  Js.Unsafe.global##.runWee := Js.wrap_callback run
