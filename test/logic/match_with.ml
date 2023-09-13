open Logic.Logic_exp.Examples

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
  let f (tree, structure, ass_tree) =
    match T.match_with tree structure with
    | Some ass -> print_endline (T.to_string (T.assign ass ass_tree))
    | None -> print_endline "No assignment"
  in
  List.iter f inputs
