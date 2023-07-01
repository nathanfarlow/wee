open Core

type dispatcher =
  | Check_lt of { value : int; yes : dispatcher; no : dispatcher }
  | Select of int

let binary_search numbers =
  let rec binary_search' left right =
    if left = right then Select numbers.(left)
    else
      let mid = (left + right) / 2 in
      let no = binary_search' left mid in
      let yes = binary_search' (mid + 1) right in
      Check_lt { value = numbers.(mid); yes; no }
  in
  binary_search' 0 (Array.length numbers - 1)

let rec compile program =
  let open Symbolic_instruction in
  match program with
  | Check_lt { value; yes; no } ->
      let yes = compile yes in
      let no = compile no in
      let check =
        [ Mov (Const value); Setlt; Jmpz (Pc_relative (List.length yes + 1)) ]
      in
      check @ yes @ no
  | Select pc -> [ Mov (Const 0); Jmpz (Const pc) ]

let make_routine program ~elvm_to_wee =
  let open Elvm_program in
  let addresses =
    List.filter_map (Hashtbl.data program.labels) ~f:(function
      | { segment = Text; offset } -> Some offset
      | _ -> None)
    |> List.map ~f:elvm_to_wee
    |> List.sort ~compare:Int.compare
    |> Array.of_list
  in
  compile @@ binary_search addresses
