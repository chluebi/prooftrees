open Base.Trees

let () =
  let t : int tree = Node (1, [ Leaf 2; Leaf 3 ]) in
  let f_leaf = Fun.id in
  let rec sum = function [] -> 0 | h :: t -> h + sum t in
  let f_node t l = t + sum l in
  let i = tree_fold f_leaf f_node t in
  print_endline (string_of_int i)
