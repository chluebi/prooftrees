open Trees

module type Proofsystem = sig
  module Key : Map.OrderedType
  module KeySet : Set.S with type elt = Key.t

  type rule_name_type
  type ass
  type statement
  type pattern

  type rule =
    rule_name_type * pattern * pattern list * (ass -> KeySet.t -> ass list)

  type t = (rule_name_type * statement) tree_type

  (* *)
  val rules : rule list
  val rule_name_to_string : rule_name_type -> string
  val statement_to_string : statement -> string
  val pattern_to_string : pattern -> string

  (* *)
  val match_with : statement -> pattern -> ass option
  val assign : ass -> pattern -> statement
end
