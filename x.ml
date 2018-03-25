let x = 1
let y = 2
let f x =
  x + y
let g y =
  f x + y

let () =
  List.iter print_endline ["a"; "b"]
