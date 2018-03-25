type loc = {
  (* filename: string; *)
  line: int;
  line_start: int;
  offset: int;
}

let filename = ref ""

let parse_loc offset a =
  if !filename = "" then (
    let s = a.(offset) in
    filename := String.sub s 1 (String.length s - 2)
  );
  {
    line = int_of_string a.(offset + 1);
    line_start = int_of_string a.(offset + 2);
    offset = int_of_string a.(offset + 3);
  }

let parse_location offset a =
  parse_loc offset a, parse_loc (offset + 4) a

let read_line () =
  (*
  print_endline "l";
  *)
  read_line ()

let read_block () =
  let rec f acc =
    let l = read_line () in
    if l = ")" then
      acc
    else
      let s = String.sub l 2 (String.length l - 2) in
      if acc = "" then
        f s
      else
        (* FIXME hack to avoid replacing newlines afterwards *)
        (* this works because types are directly output as attributes *)
        f (acc ^ "&#xA;" ^ s)
  in
  f ""

type ident =
  | Definition of string * (loc * loc)
  | Internal of string * (loc * loc)
  | External of string

let split s =
  Str.split (Str.regexp_string " ") s |>
  Array.of_list

let parse_ident loc_start s =
  let a = split s in
  if a.(0) = "def" then
    (* let's replace the start position *)
    (* FIXME how bad is the original one? *)
    let _, loc_end = parse_location 2 a in
    Definition (a.(1), (loc_start, loc_end))
  else if a.(0) = "ext_ref" then
    (* don't break with Pervasives.( * ) *)
    External (String.sub s 8 (String.length s - 8))
  else if a.(0) = "int_ref" then
    Internal (a.(1), parse_location 2 a)
  else
    failwith ("unexpected " ^ a.(0))

let link_of_ident id =
  let anchor_of_id id = "#" ^ string_of_int id in
  match id with
  | Internal (_, ({offset; _}, _)) -> anchor_of_id offset
  | Definition (_, ({offset; _}, _)) -> anchor_of_id offset
  | External name ->
    let p = String.index name '.' in
    let modl = String.sub name 0 p in
    let mem = String.sub name (p+1) (String.length name - p - 1) in
    let base = "https://caml.inria.fr/pub/docs/manual-ocaml/libref/" in
    base ^ modl ^ ".html#VAL" ^ mem

type t = {
  location: loc * loc;
  typ: string;
  ident: ident;
}

let parse () =
  let rec f l state =
    try
      match read_line (), state with
      | "ident(", `Ident (location, typ) ->
        let loc_start, _ = location in
        let ident = parse_ident loc_start (read_block ()) in
        f ({location; typ; ident} :: l) `Loc
      | "type(", (`Ident _ as id) ->
        ignore (read_block ());
        f l id
      | s, `Ident _
      | s, `Loc ->
        f l (`Type (parse_location 0 (split s)))
      | "type(", `Type location ->
        f l (`Ident (location, read_block ()))
      | "ident(", `Type _ ->
        ignore (read_block ());
        f l `Loc
      | "call(", (`Type _ as t) ->
        ignore (read_block ());
        f l t
      | l, _ ->
        (*
        let s =
          match s with
          | `Loc -> "Loc"
          | `Type _ -> "Type"
          | `Ident _ -> "Ident"
        in
        failwith ("in state " ^ s ^ ", unexpected " ^ l)
        *)
        failwith ("unexpected " ^ l)
    with End_of_file ->
      List.rev l
  in
  !filename, f [] `Loc
