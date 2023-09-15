open Base.Trees
open Logic_exp

module Proplogic = struct
  module RHS = AssignableTree (LogicExpression)
  module LHS = TreeSet (RHS)
  module Key = RHS.Key
  module KeySet = Set.Make (Key)

  type rule_name_type = string
  type side_condition_name_type = string
  type ass = LHS.ass
  type statement = LHS.t * RHS.t
  type pattern = LHS.pattern * RHS.pattern

  type rule =
    rule_name_type
    * pattern
    * pattern list
    * (side_condition_name_type * (ass -> bool))
    * (ass -> KeySet.t -> ass list)

  type t = (rule_name_type * statement) tree_type

  let g = (Some "G", LHS.TreeSet.empty)

  (* *)
  let sc_trivial _ = true

  let sc_contains (s : string) (((subtree_ass, _), treeset_ass) : ass) =
    LHS.TreeSet.mem
      (RHS.SubtreeAssignment.find s subtree_ass)
      (LHS.Assignment.find "G" treeset_ass)

  (* *)
  let guess_empty (assignment : ass) (_ : KeySet.t) = [ assignment ]

  let guess_gamma (((subtree_assignment, _), set_assignment) : ass)
      (free_variables : KeySet.t) =
    let gamma = LHS.Assignment.find "G" set_assignment in
    let f free_var assignments =
      let g set_elt acc =
        let h acc ass = RHS.SubtreeAssignment.add free_var set_elt ass :: acc in
        List.append (List.fold_left h [] assignments) acc
      in
      LHS.TreeSet.fold g gamma []
    in
    KeySet.fold f free_variables [ subtree_assignment ]

  let guess_match_gamma (pattern : RHS.pattern)
      ((tree_assignment, set_assignment) : ass) (_ : KeySet.t) =
    let gamma = LHS.Assignment.find "G" set_assignment in
    let f set_elt acc =
      let new_list =
        match RHS.match_with set_elt pattern with
        | Some ass -> (
            match RHS.merge ass tree_assignment with
            | Some ass -> [ (ass, set_assignment) ]
            | None -> [])
        | None -> []
      in
      List.append new_list acc
    in
    LHS.TreeSet.fold f gamma []

  let rules : rule list =
    [
      ( "Axiom",
        (g, Node (Var "X", [])),
        [],
        ("Gamma contains formula", sc_contains "X"),
        guess_empty );
      ( "AND-I",
        (g, Node (And, [ Node (Var "X", []); Node (Var "Y", []) ])),
        [ (g, Node (Var "X", [])); (g, Node (Var "Y", [])) ],
        ("", sc_trivial),
        guess_empty );
      ( "AND-EL",
        (g, Node (Var "X", [])),
        [ (g, Node (And, [ Node (Var "X", []); Node (Var "Y", []) ])) ],
        ("", sc_trivial),
        guess_match_gamma
          (Node (And, [ Node (Var "X", []); Node (Var "Y", []) ])) );
      ( "AND-ER",
        (g, Node (Var "Y", [])),
        [ (g, Node (And, [ Node (Var "X", []); Node (Var "Y", []) ])) ],
        ("", sc_trivial),
        guess_match_gamma
          (Node (And, [ Node (Var "X", []); Node (Var "Y", []) ])) );
    ]

  let rule_name_to_string s = s
  let side_condition_name_to_string s = s

  let statement_to_string ((lhs, rhs) : statement) =
    LHS.to_string lhs ^ " |- " ^ RHS.to_string rhs

  let pattern_to_string ((lhs, rhs) : pattern) =
    LHS.pattern_to_string lhs ^ " |- " ^ RHS.pattern_to_string rhs

  let assignment_to_string = LHS.ass_to_string

  let match_with ((lhs, rhs) : statement) ((lhs_p, rhs_p) : pattern) :
      ass option =
    match (LHS.match_with lhs lhs_p, RHS.match_with rhs rhs_p) with
    | Some (lhs_ass, lhs_set_ass), Some rhs_ass -> (
        match RHS.merge lhs_ass rhs_ass with
        | Some ass -> Some (ass, lhs_set_ass)
        | None -> None)
    | _ -> None

  let assign ((ass, set_ass) : ass) ((lhs_p, rhs_p) : pattern) : statement =
    (LHS.assign (ass, set_ass) lhs_p, RHS.assign ass rhs_p)

  let merge = LHS.merge

  let free_variables_ass ((lhs_p, rhs_p) : pattern) ((ass, set_ass) : ass) :
      KeySet.t =
    KeySet.diff
      (KeySet.union (LHS.free_variables lhs_p) (RHS.free_variables rhs_p))
      (KeySet.union
         (LHS.assigned_variables (ass, set_ass))
         (RHS.assigned_variables ass))

  let free_variables ((lhs_p, rhs_p) : pattern) =
    KeySet.union (LHS.free_variables lhs_p) (RHS.free_variables rhs_p)
end

module Examples = struct
  open LogicExpression

  let list_to_tree_set (l : LogicExpression.t list) : Proplogic.LHS.t =
    List.fold_left
      (fun acc elt -> Proplogic.LHS.TreeSet.add elt acc)
      Proplogic.LHS.TreeSet.empty l

  let treeA = Node (Var "A", [])
  let treeB = Node (Var "B", [])
  let treeAB = Node (And, [ Node (Var "A", []); Node (Var "B", []) ])
  let treeBA = Node (And, [ Node (Var "B", []); Node (Var "A", []) ])

  (* *)
  let treesetA = list_to_tree_set [ treeA ]
  let treesetB = list_to_tree_set [ treeB ]
  let treesetAB = list_to_tree_set [ treeA; treeB ]

  let prooftreeA =
    Node
      ( ( "Axiom",
          ( Proplogic.LHS.TreeSet.singleton (Node (Var "A", [])),
            Node (Var "A", []) ) ),
        [] )

  let prooftree1 =
    Node
      ( ( "AND-I",
          (treesetAB, Node (And, [ Node (Var "A", []); Node (Var "B", []) ])) ),
        [
          Node (("Axiom", (treesetAB, Node (Var "A", []))), []);
          Node (("Axiom", (treesetAB, Node (Var "B", []))), []);
        ] )

  let statement1 =
    (treesetAB, Node (And, [ Node (Var "A", []); Node (Var "B", []) ]))

  let statement2 = (list_to_tree_set [ treeAB ], treeBA)
end
