type 'a tree_type = Node of 'a * 'a tree_type list

let rec tree_fold f = function Node (t, l) -> f t (List.map (tree_fold f) l)

module type AssignableTree = sig
  type elt
  type ass
  type t = elt tree_type

  val tree_check : t -> unit
  val tree_to_string : t -> string
  val tree_assign : ass -> t -> t
end

module TreeAssigner (M : AssignableTree) = struct
  let print_tree tree = M.tree_to_string tree
  let assign ass tree = M.tree_assign ass tree
end
