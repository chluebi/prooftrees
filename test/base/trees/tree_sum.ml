open Base.Trees

let () =
  let t : int tree_type = Node (1, [ Node (2, []); Node (3, []) ]) in
  let rec sum = function [] -> 0 | h :: t -> h + sum t in
  let f t l = t + sum l in
  let i = tree_fold f t in
  print_endline (string_of_int i)
