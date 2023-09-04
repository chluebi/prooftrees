open Base.Trees

type op = Or | And | Not | Var of string

let print_op = function Or -> "Or" | And -> "And" | Not -> "Not" | Var s -> s

class logic_exp =
  object
    (* inherit [op] tree_class as super *)
    method tree_check (t : op tree_type) : unit =
      let check t l =
        let len = List.length l in
        match t with
        | Or -> if len = 2 then () else failwith "Or has exactly 2 arguments"
        | And -> if len = 2 then () else failwith "And has exactly 2 arguments"
        | Not -> if len = 1 then () else failwith "Not has exactly 1 argument"
        | Var _ ->
            if len = 0 then () else failwith "Var has exactly 0 arguments"
      in
      tree_fold check t

    method tree_to_string (t : op tree_type) : string =
      let rec join (sep : string) (s : string list) : string =
        match s with [] -> "" | h :: [] -> h | h :: t -> h ^ sep ^ join sep t
      in
      let print_node t l =
        if List.length l = 0 then print_op t
        else print_op t ^ "(" ^ join ", " l ^ ")"
      in
      tree_fold print_node t
  end
