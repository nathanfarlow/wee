open Core
open Instruction

type t = Instruction.t list

exception Parse_error of string

let parse_exn s =
  let parse_int s =
    match int_of_string_opt s with
    | Some i -> i
    | None -> raise @@ Parse_error ("invalid int literal " ^ s)
  in
  let parse_insn s =
    let s = String.strip s in
    match Str.split (Str.regexp "[ \t]+") s with
    | [ "mov"; i ] -> Mov (parse_int i)
    | [ "swap" ] -> Swap
    | [ "add" ] -> Add
    | [ "sub" ] -> Sub
    | [ "load" ] -> Load
    | [ "store" ] -> Store
    | [ "setlt" ] -> Setlt
    | [ "jmpz"; i ] -> Jmpz (parse_int i)
    | [ "getc" ] -> Getc
    | [ "putc" ] -> Putc
    | [ "exit" ] -> Exit
    | _ -> raise @@ Parse_error ("invalid instruction " ^ s)
  in
  List.map (String.split_lines s) ~f:parse_insn

let run t ~mem_size ~get_char ~put_char =
  let instructions = Array.of_list t in
  let mem = Array.create ~len:mem_size 0 in
  let rec run' pc a b =
    match instructions.(pc) with
    | Mov i -> run' (pc + 1) i b
    | Swap -> run' (pc + 1) b a
    | Add -> run' (pc + 1) (a + b) b
    | Sub -> run' (pc + 1) (a - b) b
    | Load -> run' (pc + 1) mem.(a) b
    | Store ->
        mem.(a) <- b;
        run' (pc + 1) a b
    | Setlt -> run' (pc + 1) (if a < b then 1 else 0) b
    | Jmpz i -> if a = 0 then run' i a b else run' (pc + 1) a b
    | Getc ->
        let c = get_char () in
        run' (pc + 1) c b
    | Putc ->
        put_char a;
        run' (pc + 1) a b
    | Exit -> ()
  in
  run' 0 0 0

let to_string t =
  let insn_to_string = function
    | Mov i -> "mov " ^ string_of_int i
    | Swap -> "swap"
    | Add -> "add"
    | Sub -> "sub"
    | Load -> "load"
    | Store -> "store"
    | Setlt -> "setlt"
    | Jmpz i -> "jmpz " ^ string_of_int i
    | Getc -> "getc"
    | Putc -> "putc"
    | Exit -> "exit"
  in
  String.concat ~sep:"\n" (List.map t ~f:insn_to_string)
