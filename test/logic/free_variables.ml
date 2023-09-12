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
  let f (tree, expected) =
    match T.free_variables tree = expected with
    | true -> print_endline "true"
    | false -> print_endline "false"
  in
  List.iter f inputs
