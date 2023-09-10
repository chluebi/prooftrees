open Base.Trees

module LogicExpression = struct
  module Key = String

  type elt = Or | And | Not | Var of Key.t
  type t = elt tree_type

  let element_to_string = function
    | Or -> "Or"
    | And -> "And"
    | Not -> "Not"
    | Var s -> s

  let child_count = function Or -> 2 | And -> 2 | Not -> 1 | Var _ -> 0

  let var_opt (op : elt) : string option =
    match op with Var s -> Some s | _ -> None
end

module Examples = struct
  open LogicExpression
  open AssignableTree (LogicExpression)

  let treeA = Node (Var "A", [])
  let treeB = Node (Var "B", [])
  let tree1 = Node (Or, [ Node (Var "A", []); Node (Var "B", []) ])
  let tree2 = Node (And, [ Node (Var "C", []); Node (Var "D", []) ])
  let tree3 = Node (Or, [ Node (Var "A", []); Node (Var "A", []) ])
  let tree4 = Node (Not, [ Node (Var "A", []) ])
  let ass0 = Assignment.empty
  let ass1 = Assignment.add "B" tree2 Assignment.empty
  let ass2 = Assignment.add "A" tree1 ass1
  let ass3 = Assignment.add "A" tree3 Assignment.empty
end
