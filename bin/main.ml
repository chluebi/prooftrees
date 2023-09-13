open Base.Pyramids
open Logic.Proplogic
open Logic.Proplogic.Examples

let () =
  let module Prover = ProofAssistant (Proplogic) in
  Prover.proofcheck prooftree1
