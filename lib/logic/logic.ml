open Base.Trees

type operator = Or | And | Not | Var of string

let print_operator = function
  | Or -> "Or"
  | And -> "And"
  | Not -> "Not"
  | Var s -> s

module Assignment = Map.Make (String)

module LogicExpression = struct
  type elt = operator
  type t = elt tree_type
  type ass = t Assignment.t

  let tree_check tree : unit =
    let check op l =
      let len = List.length l in
      match op with
      | Or -> if len = 2 then () else failwith "Or has exactly 2 arguments"
      | And -> if len = 2 then () else failwith "And has exactly 2 arguments"
      | Not -> if len = 1 then () else failwith "Not has exactly 1 argument"
      | Var _ -> if len = 0 then () else failwith "Var has exactly 0 arguments"
    in
    tree_fold check tree

  let tree_to_string tree : string =
    let rec join (sep : string) (s : string list) : string =
      match s with
      | [] -> ""
      | head :: [] -> head
      | head :: tail -> head ^ sep ^ join sep tail
    in
    let print_node op l =
      if List.length l = 0 then print_operator op
      else print_operator op ^ "(" ^ join ", " l ^ ")"
    in
    tree_fold print_node tree

  let tree_assign assignment tree : t =
    let assign op l =
      match op with
      | Var s -> (
          match Assignment.find_opt s assignment with
          | Some subtree -> subtree
          | None -> Node (op, []))
      | _ -> Node (op, l)
    in
    tree_fold assign tree
end

module Examples = struct
  let tree1 = Node (Or, [ Node (Var "A", []); Node (Var "B", []) ])
  let tree2 = Node (And, [ Node (Var "C", []); Node (Var "D", []) ])
  let tree3 = Node (Or, [ Node (Var "A", []); Node (Var "A", []) ])
  let tree4 = Node (Not, [ Node (Var "A", []) ])
  let ass0 = Assignment.empty
  let ass1 = Assignment.add "B" tree2 Assignment.empty
  let ass2 = Assignment.add "A" tree1 ass1
  let ass3 = Assignment.add "A" tree3 Assignment.empty
end
