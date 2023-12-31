open Core
open Elvm_instruction

type segment = Data | Text
type address = { segment : segment; offset : int }
type data_entry = Const of int | Address of address

type t = {
  data : data_entry list;
  instructions : Elvm_instruction.t list;
  labels : (string, address) Hashtbl.t;
}

exception Parse_error of string

type long = Label of string | Number of int
type declaration = Long of long | String of string

module Directive = struct
  type t = Text of int | Data of int | Init of declaration
end

type statement =
  | Label of string
  | Directive of Directive.t
  | Instruction of Elvm_instruction.t

module Section = struct
  type t = Text of int | Data of int
end

let parse_label line =
  match String.split line ~on:':' with [ label; "" ] -> Some label | _ -> None

let get_all_labels lines =
  lines
  |> List.filter_map ~f:parse_label
  (* add elvm's magic heap base pointer *)
  |> List.append [ "_edata" ]
  |> Hash_set.of_list (module String)

let parse_number s =
  Caml.int_of_string_opt s
  (* elvm alternates between using signed and unsigned 24 bit integers. *)
  (* we'll fix that here *)
  |> Option.map ~f:(fun n -> if n > 0x7fffff then n - 0x1000000 else n)

let parse_directive labels line =
  let open Directive in
  match Str.bounded_split (Str.regexp "[ \t]+") line 2 with
  | [ ".text" ] -> Some (Text 0)
  | [ ".text"; subsection ] -> Some (Text (Int.of_string subsection))
  | [ ".data" ] -> Some (Data 0)
  | [ ".data"; subsection ] -> Some (Data (Int.of_string subsection))
  | [ ".long"; arg ] -> (
      match parse_number arg with
      | Some n -> Some (Init (Long (Number n)))
      | None ->
          if Hash_set.mem labels arg then Some (Init (Long (Label arg)))
          else raise @@ Parse_error ("unknown argument for .long: " ^ arg))
  | [ ".string"; arg ] ->
      let inside_quotes = Str.regexp {|"\(.*\)"|} in
      if Str.string_match inside_quotes arg 0 then
        let unescaped = Scanf.unescaped @@ Str.matched_group 1 arg in
        let with_null_terminator = unescaped ^ "\x00" in
        Some (Init (String with_null_terminator))
      else raise @@ Parse_error ("invalid argument for .string: " ^ arg)
  | _ -> None

let parse_register s =
  match s with
  | "A" -> Some A
  | "B" -> Some B
  | "C" -> Some C
  | "D" -> Some D
  | "SP" -> Some SP
  | "BP" -> Some BP
  | _ -> None

let parse_register_exn s =
  match parse_register s with
  | Some r -> r
  | None -> raise @@ Parse_error (s ^ " is not a register")

let parse_instruction line labels =
  let parse_immediate_or_register arg =
    let arg = List.hd_exn @@ String.split ~on:',' arg in
    match parse_number arg with
    | Some n -> Int n
    | None -> (
        match parse_register arg with
        | Some r -> Register r
        | None ->
            if Hash_set.mem labels arg then Label arg
            else raise @@ Parse_error ("label not found: " ^ arg))
  in

  let parse_src_dst ~src ~dst =
    let src = String.chop_suffix_if_exists src ~suffix:"," in
    let src = parse_immediate_or_register src in
    let dst = String.chop_suffix_if_exists dst ~suffix:"," in
    let dst = parse_register_exn dst in
    { src; dst }
  in

  let parse_conditional_jump comparison target ~src ~dst =
    let target = parse_immediate_or_register target in
    let condition = { comparison; args = parse_src_dst ~src ~dst } in
    Some (Jump { target; condition = Some condition })
  in

  let parse_set comparison ~src ~dst =
    let condition = { comparison; args = parse_src_dst ~src ~dst } in
    Some (Set condition)
  in

  match Str.split (Str.regexp "[ \t]+") line with
  | [ "mov"; dst; src ] -> Some (Mov (parse_src_dst ~src ~dst))
  | [ "add"; dst; src ] -> Some (Add (parse_src_dst ~src ~dst))
  | [ "sub"; dst; src ] -> Some (Sub (parse_src_dst ~src ~dst))
  | [ "load"; dst; src ] -> Some (Load (parse_src_dst ~src ~dst))
  | [ "store"; src; dst ] ->
      let no_comma = String.chop_suffix_if_exists src ~suffix:"," in
      let src = parse_register_exn no_comma in
      let dst = parse_immediate_or_register dst in
      Some (Store { src; dst })
  | [ "putc"; src ] -> Some (Putc (parse_immediate_or_register src))
  | [ "getc"; dst ] -> Some (Getc (parse_register_exn dst))
  | [ "exit" ] -> Some Exit
  | [ "jmp"; target ] ->
      let target = parse_immediate_or_register target in
      (match target with
      | Int _ ->
          raise
          @@ Parse_error
               "jmp [int] isn't supported. use jmp [label] instead. this never \
                worked in elvm anyway..."
      | _ -> ());
      Some (Jump { target; condition = None })
  | [ "jeq"; target; dst; src ] -> parse_conditional_jump Eq target ~src ~dst
  | [ "jne"; target; dst; src ] -> parse_conditional_jump Ne target ~src ~dst
  | [ "jlt"; target; dst; src ] -> parse_conditional_jump Lt target ~src ~dst
  | [ "jle"; target; dst; src ] -> parse_conditional_jump Le target ~src ~dst
  | [ "jgt"; target; dst; src ] -> parse_conditional_jump Gt target ~src ~dst
  | [ "jge"; target; dst; src ] -> parse_conditional_jump Ge target ~src ~dst
  | [ "eq"; dst; src ] -> parse_set Eq ~src ~dst
  | [ "ne"; dst; src ] -> parse_set Ne ~src ~dst
  | [ "lt"; dst; src ] -> parse_set Lt ~src ~dst
  | [ "gt"; dst; src ] -> parse_set Gt ~src ~dst
  | [ "le"; dst; src ] -> parse_set Le ~src ~dst
  | [ "ge"; dst; src ] -> parse_set Ge ~src ~dst
  | [ "dump" ] -> Some Dump
  | _ -> None

let parse_statement labels line =
  match parse_label line with
  | Some label -> Label label
  | None -> (
      match parse_directive labels line with
      | Some directive -> Directive directive
      | None -> (
          match parse_instruction line labels with
          | Some i -> Instruction i
          | None -> raise @@ Parse_error ("unknown statement: " ^ line)))

let make_sections statements resolve_label =
  let labels = Hashtbl.create (module String) in
  let data = Hashtbl.create (module Int) in
  let instructions = Hashtbl.create (module Int) in

  let add table key data =
    let deque =
      Hashtbl.find_or_add table key ~default:(fun () -> Deque.create ())
    in
    Deque.enqueue_back deque data
  in

  let current_section = ref None in
  let open Section in
  List.iter statements ~f:(fun statement ->
      match statement with
      | Directive (Text subsection) -> current_section := Some (Text subsection)
      | Directive (Data subsection) -> current_section := Some (Data subsection)
      | Directive (Init value) -> (
          match !current_section with
          | Some (Data sub) -> (
              match value with
              | Long (Number n) -> add data sub (Const n)
              | Long (Label label) ->
                  add data sub (Address (resolve_label label))
              | String s ->
                  String.iter s ~f:(fun c ->
                      add data sub (Const (Char.to_int c))))
          | _ -> raise @@ Parse_error "cannot declare data outside .data")
      | Label label ->
          let length table sub =
            Hashtbl.find_or_add table sub ~default:(fun () -> Deque.create ())
            |> Deque.length
          in
          let value =
            match !current_section with
            | Some (Data sub) -> (Data sub, length data sub)
            | Some (Text sub) -> (Text sub, length instructions sub)
            | None ->
                raise @@ Parse_error "cannot declare label outside section"
          in
          Hashtbl.add_exn labels ~key:label ~data:value
      | Instruction i -> (
          match !current_section with
          | Some (Text sub) -> add instructions sub i
          | _ -> raise @@ Parse_error "cannot declare instruction outside .text"
          ));
  (labels, data, instructions)

let make_segments statements resolve_label =
  let labels, data, instructions = make_sections statements resolve_label in
  let flatten table =
    let sorted_subsections =
      Hashtbl.to_alist table
      |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    in
    let flattened =
      List.map sorted_subsections ~f:snd
      |> List.concat_map ~f:(fun deque -> Deque.to_list deque)
    in
    let indices =
      List.folding_map sorted_subsections ~init:0 ~f:(fun index (key, deque) ->
          let length = Deque.length deque in
          let next_index = index + length in
          (next_index, (key, index)))
      |> Hashtbl.of_alist_exn (module Int)
    in
    (flattened, indices)
  in

  let data_segment, data_subsection_offsets = flatten data in
  let text_segment, text_subsection_offsets = flatten instructions in

  let make_address table segment ~sub ~offset =
    let offset = offset + Hashtbl.find_exn table sub in
    { segment; offset }
  in

  let segment_labels =
    Hashtbl.map labels ~f:(fun (section, offset) ->
        match section with
        | Data sub -> make_address data_subsection_offsets Data ~sub ~offset
        | Text sub -> make_address text_subsection_offsets Text ~sub ~offset)
  in
  (* add elvm's magic heap base pointer *)
  let data_len = List.length data_segment in
  Hashtbl.add_exn segment_labels ~key:"_edata"
    ~data:{ segment = Data; offset = data_len };
  let data_segment =
    data_segment @ [ Address { segment = Data; offset = data_len + 1 } ]
  in
  { data = data_segment; instructions = text_segment; labels = segment_labels }

let make_program statements =
  (* elvm has peculiar subsection behavior. As a result, we have to
     parse out the .text and .data subsections, and then compile them
     into two contiguous text and data segments. *)
  (* all references to labels are set to 0 as a first pass *)
  let program =
    make_segments statements (fun _ -> { segment = Data; offset = 0 })
  in
  (* parse again now that we know segment label offsets *)
  make_segments statements (fun label -> Hashtbl.find_exn program.labels label)

let parse_exn source =
  let lines =
    let is_noop line =
      let prefixes = [ "#"; ".file"; ".loc" ] in
      String.is_empty line
      || List.exists ~f:(fun prefix -> String.is_prefix line ~prefix) prefixes
    in
    String.split_lines source |> List.map ~f:String.strip
    |> List.filter ~f:(fun line -> not @@ is_noop line)
  in
  let labels = get_all_labels lines in
  (* elvm starts execution in the main function if it's present. this breaks
     jmp int, but that's fine since it never worked with elvm in the first place. *)
  let jump_main =
    if Hash_set.mem labels "main" then
      [
        Directive (Text 0);
        Instruction (Jump { target = Label "main"; condition = None });
      ]
    else []
  in
  let statements = jump_main @ List.map lines ~f:(parse_statement labels) in
  make_program statements
