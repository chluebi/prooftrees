open Base.Trees
open Logic_exp

module Proplogic = struct
  module Key = String
  module KeySet = Set.Make (String)
  module RHS = AssignableTree (LogicExpression)
  module LHS = TreeSet (RHS)

  type rule_name_type = string
  type ass = LHS.ass
  type statement = LHS.t * RHS.t
  type pattern = LHS.pattern * RHS.pattern

  type rule =
    rule_name_type * pattern * pattern list * (ass -> KeySet.t -> ass list)

  type t = (rule_name_type * statement) tree_type

  let g = (Some "S", LHS.TreeSet.empty)
  let guess_empty (assignment : ass) (_ : KeySet.t) = [ assignment ]

  let rules : rule list =
    [
      ( "AND-I",
        (g, Node (And, [ Node (Var "A", []); Node (Var "B", []) ])),
        [ (g, Node (Var "A", [])); (g, Node (Var "B", [])) ],
        guess_empty );
    ]

  let rule_name_to_string s = s

  let statement_to_string ((lhs, rhs) : statement) =
    LHS.to_string lhs ^ " |- " ^ RHS.to_string rhs

  let pattern_to_string ((lhs, rhs) : pattern) =
    LHS.pattern_to_string lhs ^ " |- " ^ RHS.pattern_to_string rhs

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
end
