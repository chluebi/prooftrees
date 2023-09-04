let say_hi () = print_endline "Hi, World!"

type 'a tree_type = Node of 'a * 'a tree_type list

let rec tree_fold f = function Node (t, l) -> f t (List.map (tree_fold f) l)

class virtual ['a] tree_class =
  object
    method virtual tree_check : 'a tree_type -> unit
  end
