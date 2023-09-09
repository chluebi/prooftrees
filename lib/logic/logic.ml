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

  let merge assignment1 (assignment2 : t Assignment.t) : t Assignment.t option =
    let assignment2 = Some assignment2 in
    let f (key : string) (value : t) (assignment2 : t Assignment.t option) :
        t Assignment.t option =
      match assignment2 with
      | Some assignment2 -> (
          match Assignment.find_opt key assignment2 with
          | Some v when v = value -> Some assignment2
          | Some _ -> None
          | None -> Some (Assignment.add key value assignment2))
      | None -> None
    in
    Assignment.fold f assignment1 assignment2

  let rec tree_match_with tree structure : t Assignment.t option =
    match (tree, structure) with
    | Node (op1, l1), Node (Var s, _) ->
        Some (Assignment.add s (Node (op1, l1)) Assignment.empty)
    | Node (op1, l1), Node (op2, l2) ->
        if op1 = op2 && List.length l1 = List.length l2 then
          let l =
            List.map (fun (a, b) -> tree_match_with a b) (List.combine l1 l2)
          in
          let f r ass =
            match (r, ass) with Some r, Some ass -> merge r ass | _ -> None
          in
          List.fold_left f (Some Assignment.empty) l
        else None
end

module Examples = struct
  let treeA = Node (Var "A", [])
  let treeB = Node (Var "B", [])
  let tree1 = Node (Or, [ Node (Var "A", []); Node (Var "B", []) ])
  let tree2 = Node (And, [ Node (Var "C", []); Node (Var "D", []) ])
  let tree3 = Node (Or, [ Node (Var "A", []); Node (Var "A", []) ])
  let tree4 = Node (Not, [ Node (Var "A", []) ])
  let ass0 = Assignment.empty
  let ass1 = Assignment.add "B" tree2 Assignment.empty
  let ass2 = Assignment.add "A" tree1 ass1
  let ass3 = Assignment.add "A" tree3 Assignment.empty
end
