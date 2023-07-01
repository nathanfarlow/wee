open Core

let compile numbers =
  let open Symbolic_instruction in
  let rec binary_search' left right =
    if left = right then [ Mov (Const 0); Jmpz (Const numbers.(left)) ]
    else
      let mid = (left + right) / 2 in
      let no = binary_search' left mid in
      let yes = binary_search' (mid + 1) right in
      let check =
        [
          Mov (Const numbers.(mid));
          Setlt;
          Jmpz (Pc_relative (List.length yes + 1));
        ]
      in
      check @ yes @ no
  in
  binary_search' 0 (Array.length numbers - 1)

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
  compile addresses
