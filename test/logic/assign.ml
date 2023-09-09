open Base.Trees
open Logic
open Logic.Examples

let () =
  let inputs =
    [
      (tree1, ass0); (tree1, ass1); (tree1, ass2); (tree3, ass3); (tree4, ass3);
    ]
  in
  let module ResultModule = TreeAssigner (LogicExpression) in
  List.iter
    (fun (t, a) ->
      print_endline (ResultModule.print_tree (ResultModule.assign a t)))
    inputs
