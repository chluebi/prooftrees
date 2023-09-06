type 'a tree_type = Node of 'a * 'a tree_type list

let rec tree_fold f = function Node (t, l) -> f t (List.map (tree_fold f) l)

module type PrintableTree = sig
  type t

  val tree_check : t tree_type -> unit
  val tree_to_string : t tree_type -> string
end

module TreePrinter (M : PrintableTree) = struct
  let print_tree tree = M.tree_to_string tree
end
