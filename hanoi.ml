type t =
  | A
  | B
  | C

let pp ppf t =
  Printf.fprintf ppf "%s" (match t with
    | A -> "A"
    | B -> "B"
    | C -> "C"
  )

let move from to_ =
  (* Printf.printf "%a -> %a\n" pp from pp to_ *)
  Printf.printf "%a%a\n" pp from pp to_

let rec move_n ~via from to_ = function
  | 0 -> ()
  | n -> (
    move_n from via ~via:to_ (pred n);
    move from to_;
    move_n via to_ ~via:from (pred n)
  )


let () =
  match Sys.argv with
  | [| _; n |] ->
    move_n A C ~via:B (int_of_string n)
  | _ ->
    Printf.eprintf "usage: %s n\n" Sys.argv.(0);
    exit 1
