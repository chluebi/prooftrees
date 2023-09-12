open Base.Trees
open Logic
open Logic.Examples

let () =
  let module ResultModule = AssignableTree (LogicExpression) in
  match ResultModule.match_with tree1 treeA with
  | Some ass ->
      print_endline (ResultModule.to_string (ResultModule.assign ass tree3))
  | None -> ()
