type loc = {
  (* filename: string; *)
  line: int;
  line_start: int;
  offset: int;
}

type ident =
  | Definition of string * (loc * loc)
  | Internal of string * (loc * loc)
  | External of string

val link_of_ident: ident -> string

type t = {
  location: loc * loc;
  typ: string;
  ident: ident;
}

val parse: unit -> string * t list
