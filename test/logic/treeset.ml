open Logic.Logic_exp.Examples

let () =
  let inputs =
    [
      (treesetB, treeset_patternA, treesetAB);
      (treesetA, treeset_patternA, treesetA);
    ]
  in
  let f (tree, pattern, expected_tree) =
    match S.match_with tree pattern with
    | Some ass -> (
        match S.assign ass pattern = expected_tree with
        | true -> print_endline "true"
        | false -> print_endline "false")
    | None -> print_endline "No assignment"
  in
  List.iter f inputs
