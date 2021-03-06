structure O = struct
  datatype outcome
    = PASSED of string
    | NOTPASSED of { outcome : string, witness : string }
    | DNR
  exception DNRComparison

  fun toString (PASSED _) = "passed"
    | toString (NOTPASSED { outcome = c, ...}) = c
    | toString DNR = "DNR"

  fun mkWeakCompare compare =
      let fun weakCompare (PASSED _, PASSED _) = EQUAL
            | weakCompare (o1, o2) = compare (o1, o2)
      in  weakCompare
      end
end


structure OutcomeSingle :> OUTCOME = struct
  open O

  fun eprint s = TextIO.output (TextIO.stdErr, s ^ "\n")

  fun compare (PASSED _, NOTPASSED _) = GREATER
    | compare (PASSED _, PASSED _)  = EQUAL
    | compare (NOTPASSED _, PASSED _) = LESS
    | compare (NOTPASSED { outcome = c1, ...}, NOTPASSED { outcome = c2, ... }) =
      let (* not the typical trim function, but equivalent in this context because
             no reason has spaces in the middle *)
          fun trim s = String.implode (List.filter (not o Char.isSpace) (String.explode s))
          val c1 = trim c1
          val c2 = trim c2
      in compareReason c1 c2
      end
    | compare (DNR, DNR)                 = EQUAL
    | compare (_,_)                      = raise DNRComparison
  and compareReason "blewstack" "errored" = LESS
    | compareReason "errored" "blewstack" = GREATER
    | compareReason "errored" "failed" = LESS
    | compareReason "failed" "errored" = GREATER
    | compareReason "blewstack" "failed" = LESS
    | compareReason "failed" "blewstack" = GREATER
    | compareReason r1 r2 =
      ( (* We don't treat the empty string as an unrecognized reason because
           code in the Prop structure compares "" with all of the given
           outcomes. We *could* try to only complain about a particular reason
           the first time we see it, but then we also need to keep a list of what
           reasons we're expecting (need to be able to distinguish a known reason
           from the unknown ones). *)
        if r1 <> r2 andalso r1 <> "" andalso r2 <> "" then
            let val msg = String.concat [ "One or more unrecognized outcome reasons; got "
                                        , "'", r1, "', and "
                                        , "'", r2, "'"
                                        ]
            in eprint msg
            end
        else
            ()
      ; EQUAL
      )
  val weakCompare = mkWeakCompare compare

  fun comparePartial (PASSED _, NOTPASSED _)    = SOME GREATER
    | comparePartial (PASSED _, PASSED _)       = SOME EQUAL
    | comparePartial (NOTPASSED _, PASSED _)    = SOME LESS
    | comparePartial (DNR, DNR)                 = SOME EQUAL
    | comparePartial (NOTPASSED a, NOTPASSED b) =
        if #outcome a = #outcome b then SOME EQUAL else NONE
    | comparePartial (_,_)                      = raise DNRComparison

  fun eq (o1, o2) = 
    (case compare (o1, o2)
      of EQUAL => true
       | _     => false) handle DNRComparison => false
  fun weakEq (o1, o2) =
      (case weakCompare (o1, o2)
        of EQUAL => true
         | _     => false) handle DNRComparison => false
end

structure OutcomeMultiple :> OUTCOME = struct
  open O

  fun compare (PASSED _, NOTPASSED _) = GREATER
    | compare (PASSED _, _) = EQUAL
    | compare (NOTPASSED _, PASSED _) = LESS
    | compare (NOTPASSED {outcome = out1, witness = wit1}, 
               NOTPASSED {outcome = out2, witness = wit2}) = 
            if out1 = out2 then EQUAL
                          else if out1 = "errored" then LESS else GREATER
    | compare (_, _) = EQUAL
  val weakCompare = mkWeakCompare compare

  (* dodgy *)
  fun comparePartial (PASSED _, NOTPASSED _) = SOME GREATER
    | comparePartial (PASSED _, _) = SOME EQUAL
    | comparePartial (NOTPASSED _, PASSED _) = SOME LESS
    | comparePartial (NOTPASSED {outcome = out1, witness = wit1}, 
               NOTPASSED {outcome = out2, witness = wit2}) = 
            if out1 = out2 then SOME EQUAL else NONE
    | comparePartial (DNR, DNR)                 = SOME EQUAL
    | comparePartial (_, _) = NONE

  fun eq (o1, o2) = 
    (case compare (o1, o2)
      of EQUAL => true
       | _     => false)
  fun weakEq (o1, o2) =
      (case weakCompare (o1, o2)
        of EQUAL => true
         | _     => false) handle DNRComparison => false
end

structure Outcome = OutcomeSingle
