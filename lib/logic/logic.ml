open Base.Trees

type operator = Or | And | Not | Var of string

let print_operator = function
  | Or -> "Or"
  | And -> "And"
  | Not -> "Not"
  | Var s -> s

module LogicExpression = struct
  type t = operator

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
end
