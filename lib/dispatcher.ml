open Core

type program =
  | If_lt of { value : int; yes : program; no : program }
  | Select of int

let binary_search sorted_numbers =
  let rec binary_search' left right =
    if left = right then Select sorted_numbers.(left)
    else
      let mid = (left + right) / 2 in
      fprintf stderr "left: %d, right: %d, mid: %d\n" left right mid;
      let no = binary_search' left mid in
      let yes = binary_search' (mid + 1) right in
      If_lt { value = sorted_numbers.(mid); yes; no }
  in
  binary_search' 0 (Array.length sorted_numbers - 1)

let rec compile program : Symbolic_instruction.t list =
  let open Symbolic_instruction in
  match program with
  | If_lt { value; yes; no } ->
      let yes = compile yes in
      let no = compile no in
      let check =
        [ Mov (Const value); Setlt; Jmpz (Pc_relative (List.length yes + 1)) ]
      in
      check @ yes @ no
  | Select pc -> [ Mov (Const 0); Jmpz (Const pc) ]

let _print_program program =
  fprintf stderr "\n";
  let rec print_program' program indent =
    match program with
    | If_lt { value; yes; no } ->
        fprintf stderr "%scheck_lt: %d\n" indent value;
        fprintf stderr "%syes:\n" indent;
        print_program' yes (indent ^ "  ");
        fprintf stderr "%sno:\n" indent;
        print_program' no (indent ^ "  ")
    | Select pc -> fprintf stderr "%sselect %d\n" indent pc
  in
  print_program' program ""

let make_routine (program : Elvm_program.t) ~translate =
  let addresses =
    List.filter_map (Hashtbl.data program.labels) ~f:(function
      | { segment = Text; offset } -> Some offset
      | _ -> None)
    |> List.map ~f:translate
    |> List.sort ~compare:Int.compare
    |> Array.of_list
  in
  (* fprintf stderr "addresses:\n"; *)
  (* Array.iter addresses ~f:(fun addr -> fprintf stderr "%d\n" addr); *)
  let program = binary_search addresses in
  _print_program program;
  compile program
