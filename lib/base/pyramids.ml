module type ProofSystem = sig
  module Key : Map.OrderedType

  type lhs
  type rhs
  type rule_name_type
end
