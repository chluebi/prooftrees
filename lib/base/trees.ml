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
  val subtree_var_opt : elt -> Key.t option
  val op_var_opt : elt -> Key.t option
  val assign_op : Key.t -> elt -> elt
  val match_op : elt -> elt -> bool * (Key.t * Key.t) option
end

module type AssignableTree = sig
  module T : BaseTree
  module Key : Map.OrderedType with type t = T.Key.t
  module SubtreeAssignment : Map.S with type key = Key.t
  module VariableAssignment : Map.S with type key = Key.t
  module KeySet : Set.S with type elt = Key.t

  type elt = T.elt
  type t = elt tree_type
  type pattern = t
  type ass = t SubtreeAssignment.t * Key.t VariableAssignment.t

  val check : t -> unit
  val check_pattern : t -> unit
  val to_string : t -> string
  val pattern_to_string : pattern -> string
  val key_to_string : Key.t -> string
  val ass_to_string : ass -> string
  val keyset_to_string : KeySet.t -> string
  val compare : t -> t -> int
  val assign : ass -> t -> t
  val merge : ass -> ass -> ass option
  val match_with : t -> t -> ass option
  val free_variables : t -> KeySet.t
  val assigned_variables : ass -> KeySet.t
end

module AssignableTree (T : BaseTree) = struct
  module T = T
  module Key = T.Key
  module SubtreeAssignment = Map.Make (T.Key)
  module VariableAssignment = Map.Make (T.Key)
  module KeySet = Set.Make (T.Key)

  type elt = T.elt
  type t = elt tree_type
  type pattern = t
  type ass = t SubtreeAssignment.t * Key.t VariableAssignment.t

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

  let ass_to_string ((subtree_assignment, variable_assignment) : ass) : string =
    let l_subtree =
      SubtreeAssignment.fold
        (fun key value acc ->
          (key_to_string key ^ " -> " ^ to_string value) :: acc)
        subtree_assignment []
    in
    let l_variable =
      VariableAssignment.fold
        (fun key value acc ->
          (key_to_string key ^ " -> " ^ key_to_string value) :: acc)
        variable_assignment []
    in
    "{" ^ Util.join ", " (List.append l_subtree l_variable) ^ "}"

  let keyset_to_string (keyset : KeySet.t) : string =
    let l = KeySet.fold (fun elt acc -> key_to_string elt :: acc) keyset [] in
    "{" ^ Util.join ", " l ^ "}"

  let compare (tree1 : t) (tree2 : t) : int =
    String.compare (to_string tree1) (to_string tree2)

  let assign ((subtree_assignment, variable_assignment) : ass) (tree : t) : t =
    let assign (op : elt) (l : t list) =
      let new_op =
        match T.op_var_opt op with
        | Some s -> (
            match VariableAssignment.find_opt s variable_assignment with
            | Some s -> T.assign_op s op
            | None -> op)
        | None -> op
      in
      match T.subtree_var_opt op with
      | Some s -> (
          match SubtreeAssignment.find_opt s subtree_assignment with
          | Some subtree -> subtree
          | None -> Node (new_op, l))
      | _ -> Node (new_op, l)
    in
    tree_fold assign tree

  let merge ((subtree_assignment1, variable_assignment1) : ass)
      ((subtree_assignment2, variable_assignment2) : ass) : ass option =
    let f_subtree (key : T.Key.t) (value : t)
        (assignment2 : t SubtreeAssignment.t option) :
        t SubtreeAssignment.t option =
      match assignment2 with
      | Some assignment2 -> (
          match SubtreeAssignment.find_opt key assignment2 with
          | Some v when v = value -> Some assignment2
          | Some _ -> None
          | None -> Some (SubtreeAssignment.add key value assignment2))
      | None -> None
    in
    let f_variable (key : T.Key.t) (value : T.Key.t)
        (assignment2 : Key.t VariableAssignment.t option) :
        Key.t VariableAssignment.t option =
      match assignment2 with
      | Some assignment2 -> (
          match VariableAssignment.find_opt key assignment2 with
          | Some v when v = value -> Some assignment2
          | Some _ -> None
          | None -> Some (VariableAssignment.add key value assignment2))
      | None -> None
    in
    let subtree_assignment =
      SubtreeAssignment.fold f_subtree subtree_assignment1
        (Some subtree_assignment2)
    in
    let variable_assignment =
      VariableAssignment.fold f_variable variable_assignment1
        (Some variable_assignment2)
    in
    match (subtree_assignment, variable_assignment) with
    | None, None -> None
    | None, _ -> None
    | _, None -> None
    | Some s, Some v -> Some (s, v)

  let rec match_with (tree : t) (structure : t) : ass option =
    match (tree, structure) with
    | Node (op1, l1), Node (op2, l2) -> (
        match T.subtree_var_opt op2 with
        | Some s ->
            Some
              ( SubtreeAssignment.singleton s (Node (op1, l1)),
                VariableAssignment.empty )
        | None ->
            let same_op, variable_assignment_tuple = T.match_op op1 op2 in
            let variable_assignment =
              match variable_assignment_tuple with
              | Some (tree_variable_assignment, structure_variable_assignment)
                ->
                  VariableAssignment.singleton structure_variable_assignment
                    tree_variable_assignment
              | None -> VariableAssignment.empty
            in
            if same_op && List.length l1 = List.length l2 then
              let l =
                List.map (fun (a, b) -> match_with a b) (List.combine l1 l2)
              in
              let f r ass =
                match (r, ass) with
                | Some r, Some ass -> merge r ass
                | _ -> None
              in
              List.fold_left f
                (Some (SubtreeAssignment.empty, variable_assignment))
                l
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

  let assigned_variables ((subtree_assignment, variable_assignment) : ass) :
      KeySet.t =
    KeySet.union
      (SubtreeAssignment.fold
         (fun key _ acc -> KeySet.add key acc)
         subtree_assignment KeySet.empty)
      (VariableAssignment.fold
         (fun key _ acc -> KeySet.add key acc)
         variable_assignment KeySet.empty)
end

module type TreeSet = sig
  module T : AssignableTree
  module Key : Map.OrderedType with type t = T.Key.t
  module Assignment : Map.S with type key = Key.t
  module KeySet : Set.S with type elt = Key.t
  module TreeSet : Set.S with type elt = T.t

  type elt = T.t
  type t = TreeSet.t
  type pattern = KeySet.elt option * t
  type ass

  val check : t -> unit
  val check_pattern : pattern -> unit
  val to_string : t -> string
  val pattern_to_string : pattern -> string
  val key_to_string : Key.t -> string
  val ass_to_string : ass -> string
  val keyset_to_string : KeySet.t -> string
  val compare : t -> t -> int
  val assign : ass -> pattern -> t
  val match_with : t -> pattern -> ass option
  val free_variables : pattern -> KeySet.t
  val assigned_variables : ass -> KeySet.t
end

module TreeSet (T : AssignableTree) = struct
  module T = T
  module Key = T.Key
  module Assignment = Map.Make (Key)
  module KeySet = T.KeySet
  module TreeSet = Set.Make (T)

  type elt = T.t
  type t = TreeSet.t
  type pattern = T.Key.t option * t
  type ass = T.ass * t Assignment.t

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

  let key_to_string = T.key_to_string

  let ass_to_string ((tree_assignment, set_assignment) : ass) : string =
    let l =
      Assignment.fold
        (fun key value acc ->
          (T.key_to_string key ^ " -> " ^ to_string value) :: acc)
        set_assignment []
    in
    T.ass_to_string tree_assignment ^ ", {" ^ Util.join ", " l ^ "}"

  let keyset_to_string (keyset : KeySet.t) : string =
    let l = KeySet.fold (fun elt acc -> key_to_string elt :: acc) keyset [] in
    "{" ^ Util.join ", " l ^ "}"

  let compare (treeset1 : t) (treeset2 : t) : int =
    String.compare (to_string treeset1) (to_string treeset2)

  let assign ((tree_assignment, set_assignment) : ass) ((s, trees) : pattern) :
      t =
    let assigned_trees =
      TreeSet.fold
        (fun elt acc -> TreeSet.add (T.assign tree_assignment elt) acc)
        trees TreeSet.empty
    in
    match s with
    | Some s -> (
        match Assignment.find_opt s set_assignment with
        | Some set -> TreeSet.union assigned_trees set
        | None -> assigned_trees)
    | None -> assigned_trees

  let match_with (treeset1 : t) ((s, _) : pattern) : ass option =
    match s with
    | Some s ->
        Some
          ( (T.SubtreeAssignment.empty, T.VariableAssignment.empty),
            Assignment.singleton s treeset1 )
    | None ->
        Some
          ( (T.SubtreeAssignment.empty, T.VariableAssignment.empty),
            Assignment.empty )

  let free_variables ((s, trees) : pattern) =
    let free_variables_trees =
      TreeSet.fold
        (fun elt acc -> KeySet.union (T.free_variables elt) acc)
        trees KeySet.empty
    in
    let free_set_variables =
      match s with Some s -> KeySet.singleton s | None -> KeySet.empty
    in
    KeySet.union free_variables_trees free_set_variables

  let assigned_variables ((tree_assignment, set_assignment) : ass) =
    let assigned_tree_variables = T.assigned_variables tree_assignment in
    Assignment.fold
      (fun key _ acc -> KeySet.add key acc)
      set_assignment assigned_tree_variables
end
