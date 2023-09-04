open Base.Trees
open Logic

let () =
  let t : op tree_type =
    Node (Or, [ Node (Var "A", []); Node (Var "B", []) ])
  in
  let l_exp = new Logic.logic_exp in
  print_endline (l_exp#tree_to_string t)
