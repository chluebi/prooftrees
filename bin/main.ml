open Base.Trees
open Logic.Logic_exp
open Logic.Logic_exp.Examples

let () =
  let module ResultModule = AssignableTree (LogicExpression) in
  match ResultModule.match_with tree1 treeA with
  | Some ass ->
      print_endline (ResultModule.to_string (ResultModule.assign ass tree3))
  | None -> ()
