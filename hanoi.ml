let move from to_ =
  Printf.printf "%c%c\n" from to_

let rec move_n ~via:t x y = function
  | 0 -> ()
  | n -> (
    move_n x t ~via:y (pred n);
    move x y;
    move_n t y ~via:x (pred n)
  )


let () =
  match Sys.argv with
  | [| _; n |] ->
    move_n 'A' 'C' ~via:'B' (int_of_string n)
  | _ ->
    Printf.eprintf "usage: %s n\n" Sys.argv.(0);
    exit 1
