open Base.Trees
open Logic
open Logic.Examples

let () =
  let inputs =
    [
      (tree1, treeA, tree3);
      (tree1, tree3, tree1);
      (treeA, treeB, treeB);
      (tree4, treeA, treeA);
      (tree4, treeA, tree3);
      (tree1, tree1, tree1);
    ]
  in
  let module ResultModule = TreeAssigner (AssignableTree (LogicExpression)) in
  let f (tree, structure, ass_tree) =
    match ResultModule.match_with tree structure with
    | Some ass ->
        print_endline
          (ResultModule.print_tree (ResultModule.assign ass ass_tree))
    | None -> print_endline "No assignment"
  in
  List.iter f inputs
