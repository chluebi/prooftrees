open Trees

module type Formula = sig
  type t
  type pattern

  module Assignment : Map.S
  module KeySet : Set.S

  val check : t -> unit
  val check_pattern : pattern -> unit
  val to_string : t -> string
  val pattern_to_string : pattern -> string
  val ass_to_string : t Assignment.t -> string
  val keyset_to_string : KeySet.t -> string
  val assign : t Assignment.t -> pattern -> t
  val match_with : t -> pattern -> t Assignment.t option
  val free_variables : pattern -> KeySet.t
  val assigned_variables : t Assignment.t -> KeySet.t
end

module type Pyramid = sig
  module Key : Map.OrderedType
  module LHS : Formula
  module RHS : Formula

  type rule_name_type
  type statement = LHS.t * RHS.t
  type t = (statement * rule_name_type) tree_type
end
