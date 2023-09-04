let say_hi () = print_endline "Hi, World!"

type 'a tree = Leaf of 'a | Node of 'a * 'a tree list

let rec tree_fold f_leaf f_node = function
  | Leaf t -> f_leaf t
  | Node (t, l) -> f_node t (List.map (tree_fold f_leaf f_node) l)
