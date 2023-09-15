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
  let g_tree tree = (Some "G", LHS.TreeSet.singleton tree)

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

  let guess_match_gamma_list (pattern_list : RHS.pattern list)
      (assignment : ass) (ks : KeySet.t) =
    List.concat_map (fun x -> guess_match_gamma x assignment ks) pattern_list

  let guess_match_tnd (pattern : RHS.pattern) (var : string)
      ((tree_assignment, set_assignment) : ass) (_ : KeySet.t) =
    let subtree_assignment, _ = tree_assignment in
    let free_variables =
      RHS.free_variables (RHS.SubtreeAssignment.find var subtree_assignment)
    in
    let tnd_list : RHS.t list =
      let f x =
        Node
          ( LogicExpression.Or,
            [ Node (Var x, []); Node (Not, [ Node (Var x, []) ]) ] )
      in
      List.map f (List.of_seq (KeySet.to_seq free_variables))
    in
    let tnd_set = LHS.TreeSet.of_seq (List.to_seq tnd_list) in
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
    LHS.TreeSet.fold f tnd_set []

  let guess_combine guess1 guess2 (assignment : ass) (free_variables : KeySet.t)
      =
    List.append
      (guess1 assignment free_variables)
      (guess2 assignment free_variables)

  let rules : rule list =
    [
      ( "Axiom",
        (g, Node (Var "X", [])),
        [],
        ("Gamma contains formula", sc_contains "X"),
        guess_empty );
      ( "TND",
        ( g,
          Node (Or, [ Node (Var "X", []); Node (Not, [ Node (Var "X", []) ]) ])
        ),
        [],
        ("", sc_trivial),
        guess_empty );
      ( "Arrow-I",
        (g, Node (Arrow, [ Node (Var "X", []); Node (Var "Y", []) ])),
        [ (g_tree (Node (Var "X", [])), Node (Var "Y", [])) ],
        ("", sc_trivial),
        guess_empty );
      ( "Arrow-E",
        (g, Node (Var "Y", [])),
        [
          (g, Node (Arrow, [ Node (Var "X", []); Node (Var "Y", []) ]));
          (g, Node (Var "X", []));
        ],
        ("", sc_trivial),
        guess_match_gamma
          (Node (Arrow, [ Node (Var "X", []); Node (Var "Y", []) ])) );
      ( "Bot-E",
        (g, Node (Var "X", [])),
        [ (g, Node (Bot, [])) ],
        ("", sc_trivial),
        guess_empty );
      ( "Not-I",
        (g, Node (Not, [ Node (Var "X", []) ])),
        [ (g_tree (Node (Var "X", [])), Node (Bot, [])) ],
        ("", sc_trivial),
        guess_empty );
      ( "Not-E",
        (g, Node (Var "Y", [])),
        [ (g, Node (Var "X", [])); (g, Node (Not, [ Node (Var "X", []) ])) ],
        ("", sc_trivial),
        guess_match_gamma_list
          [ Node (Var "X", []); Node (Not, [ Node (Var "X", []) ]) ] );
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
      ( "OR-IL",
        (g, Node (Or, [ Node (Var "X", []); Node (Var "Y", []) ])),
        [ (g, Node (Var "X", [])) ],
        ("", sc_trivial),
        guess_empty );
      ( "OR-IR",
        (g, Node (Or, [ Node (Var "X", []); Node (Var "Y", []) ])),
        [ (g, Node (Var "Y", [])) ],
        ("", sc_trivial),
        guess_empty );
      ( "OR-E",
        (g, Node (Var "Z", [])),
        [
          (g, Node (Or, [ Node (Var "X", []); Node (Var "Y", []) ]));
          (g_tree (Node (Var "X", [])), Node (Var "Z", []));
          (g_tree (Node (Var "Y", [])), Node (Var "Z", []));
        ],
        ("", sc_trivial),
        guess_combine
          (guess_match_gamma
             (Node (Or, [ Node (Var "X", []); Node (Var "Y", []) ])))
          (guess_match_tnd
             (Node (Or, [ Node (Var "X", []); Node (Var "Y", []) ]))
             "Z") );
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

  let treeOr =
    Node
      ( Arrow,
        [
          Node (Or, [ Node (Var "A", []); Node (Var "B", []) ]);
          Node (Or, [ Node (Var "B", []); Node (Var "A", []) ]);
        ] )

  let tree1 =
    Node
      ( Arrow,
        [
          Node (Or, [ Node (Var "A", []); Node (Var "B", []) ]);
          Node
            ( Arrow,
              [
                Node (Var "C", []);
                Node
                  ( Or,
                    [
                      Node (And, [ Node (Var "A", []); Node (Var "C", []) ]);
                      Node (And, [ Node (Var "B", []); Node (Var "C", []) ]);
                    ] );
              ] );
        ] )

  let tree2 =
    Node
      ( Arrow,
        [
          Node
            (Arrow, [ Node (Not, [ Node (Var "A", []) ]); Node (Var "A", []) ]);
          Node (Var "A", []);
        ] )

  let tree3 =
    Node (Or, [ Node (Not, [ Node (Var "A", []) ]); Node (Var "A", []) ])

  let tree4 =
    Node
      ( Arrow,
        [
          Node
            ( Arrow,
              [
                Node (Arrow, [ Node (Var "A", []); Node (Var "B", []) ]);
                Node (Var "A", []);
              ] );
          Node (Var "A", []);
        ] )

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
  let statement3 = (list_to_tree_set [], Node (Arrow, [ treeAB; treeBA ]))
  let statement4 = (list_to_tree_set [], treeOr)
  let statement5 = (list_to_tree_set [], tree1)
  let statement6 = (list_to_tree_set [], tree2)
  let statement7 = (list_to_tree_set [], tree3)
  let statement8 = (list_to_tree_set [], tree4)
end
