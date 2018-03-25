open Annot

let simple_palette = [|
  (* modified https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/ *)
  "maroon"; "brown"; "olive"; "teal"; "navy"; "black";
  "red"; "orange"; "yellow"; "lime"; "green"; "cyan"; "blue"; "purple";
  "magenta"; "pink"; "coral"; "beige"; "mint"; "lavender"
|]

let palette = [|
  (* generated using http://phrogz.net/css/distinct-colors.html *)
  "#660000"; "#d9c36c"; "#003033"; "#9173e6"; "#403030"; "#d9ff40";
  "#005266"; "#1a0040"; "#d91d00"; "#2e331a"; "#bff2ff"; "#aa00ff";
  "#33120d"; "#669900"; "#3db6f2"; "#554359"; "#d97b6c"; "#7c8c69";
  "#0081f2"; "#b086b3"; "#ffd0bf"; "#00ff00"; "#001b33"; "#ff40f2";
  "#993d00"; "#1a661a"; "#303840"; "#8c2385"; "#4c1f00"; "#36d962";
  "#1d3f73"; "#660036"; "#d98d36"; "#b6f2ce"; "#566273"; "#f23d9d";
  "#664e33"; "#2d5944"; "#bfd0ff"; "#bf6079"; "#d9c7a3"; "#008c70";
  "#0000d9"; "#a67c87"; "#f2c200"; "#3df2ce"; "#0000bf"; "#d9364c";
  "#594700"; "#00b3bf"; "#341d73"
|]


let () =
  let filename, annot = parse () in
  if filename = "" then exit 0; (* don't crash when stdin is empty *)
  let ch = open_in filename in
  let copy len =
    (* FIXME escape HTML characters *)
    (* FIXME use a single buffer (possibly more than once if it's too small) *)
    let arr = Bytes.create len in
    ignore (input ch arr 0 len); (* FIXME handle errors *)
    print_bytes arr
  in
  let f last {location = loc_start, loc_end; typ; ident} =
    (* before the current identifier *)
    let len = loc_start.offset - last in
    copy len;
    (* start tag *)
    let cls, id, title =
      match ident with
      | Internal (_, ({offset; _}, _)) ->
        "ref", Some offset, typ
      | Definition _ ->
        let offset = loc_start.offset in
        "def", Some offset, "New " ^ typ
      | External s ->
        "ext", None, s
    in
    let style =
      match id with
      | None -> ""
      | Some c ->
        let c = palette.(c mod Array.length palette) in
        " style=\"color: " ^ c ^ "\""
    in
    let title =
      match id with
      | None -> title
      | Some id -> title ^ " (" ^ string_of_int id ^ ")"
    in
    let id_a =
      match id with
      | None -> ""
      | Some id -> " id=\"" ^ string_of_int id ^ "\""
    in
    let href = " href=\"" ^ link_of_ident ident ^ "\"" in
    print_string (
      "<a class=\"" ^ cls ^ "\" title=\"" ^ title ^ "\"" ^
      id_a ^ href ^ style ^ ">"
    );
    (* the current identifier *)
    let len = loc_end.offset - loc_start.offset in
    copy len;
    (* end tag *)
    (match id with
    | None -> ()
    | Some i ->
      print_string "<span class=\"id\">@";
      print_int i;
      print_string "</span>"
    );
    print_string "</a>";
    (* remember the position of the last character we output *)
    loc_end.offset
  in
  print_endline "<link rel=\"stylesheet\" href=\"style.css\">";
  print_endline "<pre>";
  ignore (List.fold_left f 0 annot);
  let arr = Bytes.create 2048 in
  let len = ref 0 in
  while (len := input ch arr 0 2048; !len <> 0) do
    output stdout arr 0 !len
  done;
  print_endline "</pre>"
