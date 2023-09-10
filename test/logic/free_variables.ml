open Base.Trees
open Logic
open Logic.Examples

let () =
  let inputs =
    [
      (treeA, keysetA);
      (treeB, keysetB);
      (treeA, keysetB);
      (tree1, keysetAB);
      (tree2, keysetCD);
      (tree3, keysetA);
      (tree3, keysetAB);
    ]
  in
  let module ResultModule = TreeAssigner (AssignableTree (LogicExpression)) in
  let f (tree, expected) =
    match ResultModule.free_variables tree = expected with
    | true -> print_endline "true"
    | false -> print_endline "false"
  in
  List.iter f inputs
