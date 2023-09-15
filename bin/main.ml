open Base.Pyramids
open Logic.Proplogic
open Logic.Proplogic.Examples

let () =
  let module Prover = ProofAssistant (Proplogic) in
  List.iter
    (fun x -> print_endline (Prover.prooftree_to_string x))
    (Prover.prove statement2 3)
