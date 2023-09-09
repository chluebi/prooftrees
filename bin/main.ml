open Base.Trees
open Logic

let () =
  let tree1 : operator tree_type =
    Node (Or, [ Node (Var "A", []); Node (Var "B", []) ])
  in
  let tree2 : operator tree_type =
    Node (And, [ Node (Var "C", []); Node (Var "D", []) ])
  in
  let assignment = Assignment.add "B" tree2 Assignment.empty in
  let module ResultModule = TreeAssigner (LogicExpression) in
  print_endline (ResultModule.print_tree (ResultModule.assign assignment tree1))
