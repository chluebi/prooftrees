open Base.Trees

module LogicExpression = struct
  module Key = String

  type elt = Or | And | Not | Var of Key.t | Top | Bot
  type t = elt tree_type

  let element_to_string = function
    | Or -> "Or"
    | And -> "And"
    | Not -> "Not"
    | Var s -> s
    | Top -> "Top"
    | Bot -> "Bot"

  let key_to_string s = s

  let child_count = function
    | Or -> 2
    | And -> 2
    | Not -> 1
    | Var _ -> 0
    | Top -> 0
    | Bot -> 0

  let var_opt (op : elt) : string option =
    match op with Var s -> Some s | _ -> None

  let subtree_var_opt = var_opt
  let op_var_opt _ = None
  let assign_op _ (op : elt) = op

  let match_op (op1 : elt) (op2 : elt) : bool * (Key.t * Key.t) option =
    (op1 = op2, None)
end

module Examples = struct
  open LogicExpression
  open AssignableTree (LogicExpression)

  let list_to_set (l : 'a list) : KeySet.t =
    List.fold_left (fun acc elt -> KeySet.add elt acc) KeySet.empty l

  let treeA = Node (Var "A", [])
  let treeB = Node (Var "B", [])
  let tree1 = Node (Or, [ Node (Var "A", []); Node (Var "B", []) ])
  let tree2 = Node (And, [ Node (Var "C", []); Node (Var "D", []) ])
  let tree3 = Node (Or, [ Node (Var "A", []); Node (Var "A", []) ])
  let tree4 = Node (Not, [ Node (Var "A", []) ])

  (* *)
  let subtree_ass0 = SubtreeAssignment.empty
  let subtree_ass1 = SubtreeAssignment.add "B" tree2 SubtreeAssignment.empty
  let subtree_ass2 = SubtreeAssignment.add "A" tree1 subtree_ass1
  let subtree_ass3 = SubtreeAssignment.add "A" tree3 SubtreeAssignment.empty

  (* *)
  let ass0 = (subtree_ass0, VariableAssignment.empty)
  let ass1 = (subtree_ass1, VariableAssignment.empty)
  let ass2 = (subtree_ass2, VariableAssignment.empty)
  let ass3 = (subtree_ass3, VariableAssignment.empty)

  (* *)
  let keysetA = list_to_set [ "A" ]
  let keysetB = list_to_set [ "B" ]
  let keysetAB = list_to_set [ "A"; "B" ]
  let keysetCD = list_to_set [ "C"; "D" ]
end
