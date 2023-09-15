open Trees
open Util

module type Proofsystem = sig
  module Key : Map.OrderedType
  module KeySet : Set.S with type elt = Key.t

  type rule_name_type
  type side_condition_name_type
  type ass
  type statement
  type pattern

  type rule =
    rule_name_type
    * pattern
    * pattern list
    * (side_condition_name_type * (ass -> bool))
    * (ass -> KeySet.t -> ass list)

  type t = (rule_name_type * statement) tree_type

  (* *)
  val rules : rule list
  val rule_name_to_string : rule_name_type -> string
  val side_condition_name_to_string : side_condition_name_type -> string
  val statement_to_string : statement -> string
  val pattern_to_string : pattern -> string
  val assignment_to_string : ass -> string

  (* *)
  val match_with : statement -> pattern -> ass option
  val assign : ass -> pattern -> statement
  val merge : ass -> ass -> ass option
  val free_variables : pattern -> KeySet.t
  val free_variables_ass : pattern -> ass -> KeySet.t
end

module ProofAssistant (P : Proofsystem) = struct
  let prooftree_to_string (tree : P.t) : string =
    let f (rule_name, statement) prev =
      let prev_string =
        match List.length prev with 0 -> "" | _ -> "\n- " ^ join "\n- " prev
      in
      let prev_string =
        join "\n"
          (List.map (fun x -> "\t" ^ x) (String.split_on_char '\n' prev_string))
      in
      P.statement_to_string statement
      ^ "\t ("
      ^ P.rule_name_to_string rule_name
      ^ ")" ^ prev_string
    in
    tree_fold f tree

  let rec proofcheck (tree : P.t) : unit =
    match tree with
    | Node ((rule_name, statement), l) -> (
        let ( _,
              rule_pattern,
              rule_pred_patterns,
              (side_condition_name, side_condition),
              _ ) =
          List.find (fun (x, _, _, _, _) -> x = rule_name) P.rules
        in
        let assignment =
          match P.match_with statement rule_pattern with
          | Some assignment -> assignment
          | None ->
              failwith
                (P.statement_to_string statement
                ^ " does not match "
                ^ P.pattern_to_string rule_pattern)
        in
        let combined_list :
            ((P.rule_name_type * P.statement) tree_type * P.pattern) list =
          List.combine l rule_pred_patterns
        in
        let combined_ass =
          let f acc (Node ((_, s), _), p) =
            match (acc, P.match_with s p) with
            | Some ass, Some pred_ass -> (
                match P.merge ass pred_ass with
                | Some ass -> Some ass
                | None ->
                    failwith
                      ("Merge conflict between assignments "
                     ^ P.assignment_to_string ass ^ " and "
                      ^ P.assignment_to_string pred_ass))
            | Some _, None ->
                failwith
                  (P.statement_to_string s ^ " does not match "
                 ^ P.pattern_to_string p)
            | _ -> None
          in
          List.fold_left f (Some assignment) combined_list
        in
        match combined_ass with
        | Some ass -> (
            match side_condition ass with
            | true -> List.iter proofcheck l
            | false ->
                failwith
                  ("Sidecondition not fulfilled: "
                  ^ P.side_condition_name_to_string side_condition_name))
        | None -> failwith "Something went wrong when merging")

  let free_variables ((_, rule_pattern, pred_patterns, _, _) : P.rule) =
    let f acc pattern = P.KeySet.union acc (P.free_variables pattern) in
    List.fold_left f (P.free_variables rule_pattern) pred_patterns

  let free_variables_ass ((_, rule_pattern, pred_patterns, _, _) : P.rule)
      (assignment : P.ass) =
    let f acc pattern =
      P.KeySet.union acc (P.free_variables_ass pattern assignment)
    in
    List.fold_left f
      (P.free_variables_ass rule_pattern assignment)
      pred_patterns

  let rec prove (statement : P.statement) (height_left : int) : P.t list =
    if height_left = 0 then []
    else
      let find_assignments (rule : P.rule) =
        let _, rule_pattern, _, _, guess = rule in
        match P.match_with statement rule_pattern with
        | Some assignment -> (
            let free_vars = free_variables_ass rule assignment in
            match free_vars = P.KeySet.empty with
            | true -> [ (rule, assignment) ]
            | false ->
                List.map (fun x -> (rule, x)) (guess assignment free_vars))
        | None -> []
      in
      let assignments = List.concat_map find_assignments P.rules in
      let assignments =
        List.filter
          (fun (rule, ass) -> free_variables_ass rule ass = P.KeySet.empty)
          assignments
      in
      let assignments =
        List.filter
          (fun ((_, _, _, (_, side_condition), _), ass) -> side_condition ass)
          assignments
      in
      let prove_ass ((rule_name, _, pred_patterns, _, _), ass) =
        match List.length pred_patterns with
        | 0 -> [ (rule_name, []) ]
        | _ ->
            let pred_statements = List.map (P.assign ass) pred_patterns in
            let pred_trees =
              List.map (fun x -> prove x (height_left - 1)) pred_statements
            in
            let pred_combinations = crossproduct pred_trees in
            List.map (fun x -> (rule_name, x)) pred_combinations
      in
      let make_tree (rule_name, pred) = Node ((rule_name, statement), pred) in
      let f acc ass =
        match acc with
        | [] -> (
            match List.map make_tree (prove_ass ass) with
            | [] -> []
            | xs -> [ List.hd xs ])
        | xs -> [ List.hd xs ]
      in
      List.fold_left f [] assignments

  let rec prove_all (statement : P.statement) (height_left : int) : P.t list =
    if height_left = 0 then []
    else
      let find_assignments (rule : P.rule) =
        let _, rule_pattern, _, _, guess = rule in
        match P.match_with statement rule_pattern with
        | Some assignment -> (
            let free_vars = free_variables_ass rule assignment in
            match free_vars = P.KeySet.empty with
            | true -> [ (rule, assignment) ]
            | false ->
                List.map (fun x -> (rule, x)) (guess assignment free_vars))
        | None -> []
      in
      let assignments = List.concat_map find_assignments P.rules in
      let assignments =
        List.filter
          (fun (rule, ass) -> free_variables_ass rule ass = P.KeySet.empty)
          assignments
      in
      let assignments =
        List.filter
          (fun ((_, _, _, (_, side_condition), _), ass) -> side_condition ass)
          assignments
      in
      let prove_ass ((rule_name, _, pred_patterns, _, _), ass) =
        match List.length pred_patterns with
        | 0 -> [ (rule_name, []) ]
        | _ ->
            let pred_statements = List.map (P.assign ass) pred_patterns in
            let pred_trees =
              List.map (fun x -> prove_all x (height_left - 1)) pred_statements
            in
            let pred_combinations = crossproduct pred_trees in
            List.map (fun x -> (rule_name, x)) pred_combinations
      in
      let make_tree (rule_name, pred) = Node ((rule_name, statement), pred) in
      List.concat_map (fun x -> List.map make_tree (prove_ass x)) assignments
end
