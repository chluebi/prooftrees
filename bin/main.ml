open Base.Trees
open Logic
open Logic.Examples

let () =
  let module ResultModule = TreeAssigner (AssignableTree (LogicExpression)) in
  match ResultModule.match_with tree1 treeA with
  | Some ass ->
      print_endline (ResultModule.print_tree (ResultModule.assign ass tree3))
  | None -> ()
