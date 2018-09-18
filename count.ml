let rec count x = function
  | [] -> 0
  | h :: t ->
    if h = x then
      1 + count x t
    else
      count x t
