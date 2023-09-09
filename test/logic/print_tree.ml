open Base.Trees
open Logic
open Logic.Examples

let () =
  let module ResultModule = TreeAssigner (LogicExpression) in
  print_endline (ResultModule.print_tree tree1)
