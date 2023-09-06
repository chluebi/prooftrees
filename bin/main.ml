open Base.Trees
open Logic

let () =
  let tree : operator tree_type =
    Node (Or, [ Node (Var "A", []); Node (Var "B", []) ])
  in
  let module ResultModule = TreePrinter (LogicExpression) in
  print_endline (ResultModule.print_tree tree)
