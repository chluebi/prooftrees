type 'a tree_type = Node of 'a * 'a tree_type list

let rec tree_fold f = function Node (t, l) -> f t (List.map (tree_fold f) l)

module type BaseTree = sig
  type elt
  type t = elt tree_type

  module Key : Map.OrderedType

  val element_to_string : elt -> string
  val child_count : elt -> int
  val var_opt : elt -> Key.t option
end

module type AssignableTree = sig
  type elt
  type t = elt tree_type

  module Assignment : Map.S
  module KeySet : Set.S

  val tree_check : t -> unit
  val tree_to_string : t -> string
  val tree_assign : t Assignment.t -> t -> t
  val tree_match_with : t -> t -> t Assignment.t option
end

module AssignableTree (T : BaseTree) = struct
  type elt = T.elt
  type t = elt tree_type

  module Assignment = Map.Make (T.Key)
  module KeySet = Set.Make (T.Key)

  let tree_check (tree : t) : unit =
    let check (op : elt) (l : unit list) =
      let length = List.length l in
      let expected_length = T.child_count op in
      if expected_length = length then ()
      else
        failwith
          (T.element_to_string op ^ " had " ^ string_of_int length
         ^ " children. (Expected: "
          ^ string_of_int expected_length
          ^ ")")
    in
    tree_fold check tree

  let tree_to_string (tree : t) : string =
    let rec join (sep : string) (s : string list) : string =
      match s with
      | [] -> ""
      | head :: [] -> head
      | head :: tail -> head ^ sep ^ join sep tail
    in
    let print_node (op : elt) (l : string list) =
      if List.length l = 0 then T.element_to_string op
      else T.element_to_string op ^ "(" ^ join ", " l ^ ")"
    in
    tree_fold print_node tree

  let tree_assign (assignment : t Assignment.t) (tree : t) : t =
    let assign (op : elt) (l : t list) =
      match T.var_opt op with
      | Some s -> (
          match Assignment.find_opt s assignment with
          | Some subtree -> subtree
          | None -> Node (op, []))
      | _ -> Node (op, l)
    in
    tree_fold assign tree

  let merge (assignment1 : t Assignment.t) (assignment2 : t Assignment.t) :
      t Assignment.t option =
    let f (key : T.Key.t) (value : t) (assignment2 : t Assignment.t option) :
        t Assignment.t option =
      match assignment2 with
      | Some assignment2 -> (
          match Assignment.find_opt key assignment2 with
          | Some v when v = value -> Some assignment2
          | Some _ -> None
          | None -> Some (Assignment.add key value assignment2))
      | None -> None
    in
    Assignment.fold f assignment1 (Some assignment2)

  let rec tree_match_with (tree : t) (structure : t) : t Assignment.t option =
    match (tree, structure) with
    | Node (op1, l1), Node (op2, l2) -> (
        match T.var_opt op2 with
        | Some s -> Some (Assignment.add s (Node (op1, l1)) Assignment.empty)
        | None ->
            if op1 = op2 && List.length l1 = List.length l2 then
              let l =
                List.map
                  (fun (a, b) -> tree_match_with a b)
                  (List.combine l1 l2)
              in
              let f r ass =
                match (r, ass) with
                | Some r, Some ass -> merge r ass
                | _ -> None
              in
              List.fold_left f (Some Assignment.empty) l
            else None)
end

module TreeAssigner (M : AssignableTree) = struct
  let print_tree tree = M.tree_to_string tree
  let assign ass tree = M.tree_assign ass tree
  let match_with tree structure = M.tree_match_with tree structure
end
