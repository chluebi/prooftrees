type 'a tree_type = Node of 'a * 'a tree_type list

let rec tree_fold f = function Node (t, l) -> f t (List.map (tree_fold f) l)

module type BaseTree = sig
  type elt
  type t = elt tree_type

  module Key : Map.OrderedType

  val element_to_string : elt -> string
  val key_to_string : Key.t -> string
  val child_count : elt -> int
  val var_opt : elt -> Key.t option
end

module type AssignableTree = sig
  type elt
  type t = elt tree_type
  type pattern = t

  module Key : Map.OrderedType
  module Assignment : Map.S
  module KeySet : Set.S

  val check : t -> unit
  val check_pattern : t -> unit
  val to_string : t -> string
  val pattern_to_string : pattern -> string
  val key_to_string : Key.t -> string
  val ass_to_string : t Assignment.t -> string
  val keyset_to_string : KeySet.t -> string
  val compare : t -> t -> int
  val var_opt : elt -> Key.t option
  val assign : t Assignment.t -> t -> t
  val match_with : t -> t -> t Assignment.t option
  val free_variables : t -> KeySet.t
  val assigned_variables : t Assignment.t -> KeySet.t
end

module AssignableTree (T : BaseTree) = struct
  type elt = T.elt
  type t = elt tree_type
  type pattern = t

  module Key = T.Key
  module Assignment = Map.Make (T.Key)
  module KeySet = Set.Make (T.Key)

  let check (tree : t) : unit =
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

  let check_pattern = check

  let to_string (tree : t) : string =
    let print_node (op : elt) (l : string list) =
      if List.length l = 0 then T.element_to_string op
      else T.element_to_string op ^ "(" ^ Util.join ", " l ^ ")"
    in
    tree_fold print_node tree

  let pattern_to_string = to_string
  let key_to_string = T.key_to_string

  let ass_to_string (assignment : t Assignment.t) : string =
    let l =
      Assignment.fold
        (fun key value acc ->
          (key_to_string key ^ " -> " ^ to_string value) :: acc)
        assignment []
    in
    "{" ^ Util.join ", " l ^ "}"

  let keyset_to_string (keyset : KeySet.t) : string =
    let l = KeySet.fold (fun elt acc -> key_to_string elt :: acc) keyset [] in
    "{" ^ Util.join ", " l ^ "}"

  let compare (tree1 : t) (tree2 : t) : int =
    String.compare (to_string tree1) (to_string tree2)

  let var_opt = T.var_opt

  let assign (assignment : t Assignment.t) (tree : t) : t =
    let assign (op : elt) (l : t list) =
      match var_opt op with
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

  let rec match_with (tree : t) (structure : t) : t Assignment.t option =
    match (tree, structure) with
    | Node (op1, l1), Node (op2, l2) -> (
        match T.var_opt op2 with
        | Some s -> Some (Assignment.add s (Node (op1, l1)) Assignment.empty)
        | None ->
            if op1 = op2 && List.length l1 = List.length l2 then
              let l =
                List.map (fun (a, b) -> match_with a b) (List.combine l1 l2)
              in
              let f r ass =
                match (r, ass) with
                | Some r, Some ass -> merge r ass
                | _ -> None
              in
              List.fold_left f (Some Assignment.empty) l
            else None)

  let free_variables (tree : t) : KeySet.t =
    let f (op : elt) (l : KeySet.t list) =
      let new_variables =
        match T.var_opt op with
        | Some s -> KeySet.singleton s
        | None -> KeySet.empty
      in
      List.fold_left
        (fun new_set acc -> KeySet.union new_set acc)
        new_variables l
    in
    tree_fold f tree

  let assigned_variables (assignment : t Assignment.t) =
    Assignment.fold
      (fun key _ acc -> KeySet.add key acc)
      assignment KeySet.empty
end

module TreeAssigner (M : AssignableTree) = struct
  let print_tree tree = M.to_string tree
  let print_assignment ass = M.ass_to_string ass
  let print_keyset keyset = M.keyset_to_string keyset
  let assign ass tree = M.assign ass tree
  let match_with tree structure = M.match_with tree structure
  let free_variables tree = M.free_variables tree
end

module type TreeSet = sig
  module Assignment : Map.S
  module KeySet : Set.S
  module TreeSet : Set.S

  type elt
  type t = TreeSet.t
  type pattern = KeySet.elt option * t

  val check : t -> unit
  val check_pattern : pattern -> unit
  val to_string : t -> string
  val pattern_to_string : pattern -> string
  val ass_to_string : t Assignment.t -> string
  val keyset_to_string : KeySet.t -> string
  val compare : t -> t -> int
  val assign : t Assignment.t -> pattern -> t
  val match_with : t -> pattern -> t Assignment.t option
  val free_variables : pattern -> KeySet.t
  val assigned_variables : t Assignment.t -> KeySet.t
end

module TreeSet (T : AssignableTree) : TreeSet = struct
  module Assignment = Map.Make (T.Key)
  module KeySet = Set.Make (T.Key)
  module TreeSet = Set.Make (T)

  type elt = T.t
  type t = TreeSet.t
  type pattern = T.Key.t option * t

  let check (trees : t) : unit = TreeSet.iter T.check trees
  let check_pattern ((_, trees) : pattern) : unit = check trees

  let to_string (trees : t) : string =
    let l = TreeSet.fold (fun elt acc -> T.to_string elt :: acc) trees [] in
    "{" ^ Util.join ", " l ^ "}"

  let key_to_string = T.key_to_string

  let pattern_to_string ((s, trees) : pattern) : string =
    let s_string = match s with Some s -> key_to_string s | None -> "" in
    let trees_string = to_string trees in
    match (s_string, trees_string) with
    | s, t when s = "" && t = "{}" -> "{}"
    | s, t when s = "" -> t
    | s, t when t = "{}" -> s
    | s, t -> t ^ " u " ^ s

  let ass_to_string (assignment : t Assignment.t) : string =
    let l =
      Assignment.fold
        (fun key value acc ->
          (T.key_to_string key ^ " -> " ^ to_string value) :: acc)
        assignment []
    in
    "{" ^ Util.join ", " l ^ "}"

  let keyset_to_string (keyset : KeySet.t) : string =
    let l = KeySet.fold (fun elt acc -> key_to_string elt :: acc) keyset [] in
    "{" ^ Util.join ", " l ^ "}"

  let compare (treeset1 : t) (treeset2 : t) : int =
    String.compare (to_string treeset1) (to_string treeset2)

  let assign (assignment : t Assignment.t) ((s, trees) : pattern) : t =
    match s with
    | Some s -> (
        match Assignment.find_opt s assignment with
        | Some set -> TreeSet.union trees set
        | None -> trees)
    | None -> trees

  let match_with (treeset1 : t) ((s, _) : pattern) : t Assignment.t option =
    match s with
    | Some s -> Some (Assignment.singleton s treeset1)
    | None -> Some Assignment.empty

  let free_variables ((s, _) : pattern) =
    match s with Some s -> KeySet.singleton s | None -> KeySet.empty

  let assigned_variables (assignment : t Assignment.t) =
    Assignment.fold
      (fun key _ acc -> KeySet.add key acc)
      assignment KeySet.empty
end
