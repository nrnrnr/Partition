signature OUTCOME = sig
  datatype outcome
    = PASSED of string
    | NOTPASSED of { outcome : string, witness : string }
    | DNR

  val toString : outcome -> string

  exception DNRComparison

  val compare     : outcome * outcome -> order
  (* weakCompare is like compare, but returns EQUAL for any two PASSED outcomes *)
  val weakCompare : outcome * outcome -> order
  val eq          : outcome * outcome -> bool
  (* weakEq is like eq, but only considers things EQUAL according to weakCompare *)
  val weakEq      : outcome * outcome -> bool

  val comparePartial : outcome * outcome -> order option

end

(*
signature OUTCOME  = sig
  datatype outcome
    = PASSED
    | NOTPASSED of { outcome : string, witness : string }
    | DNR
  
  structure CmpSingle   : OUTCOME_COMPARISON where type outcome = outcome
  structure CmpMultiple : OUTCOME_COMPARISON where type outcome = outcome

  type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : outcome
           }

end
*)
