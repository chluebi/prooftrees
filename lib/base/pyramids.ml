open Trees

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
end

module ProofAssistant (P : Proofsystem) = struct
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
end
