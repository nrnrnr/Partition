structure DecisionTreeReport :> sig
              type sid = string
              type tid = string
              type tnum = int

              type tmark = (tid * tnum)
              type report = (tmark * string) list
              val make : TestResultDecisionTree.decisionTree -> DB.db -> (sid * report) list
              val format : (sid * report) list -> string
          end
= struct
  exception Invariant of string
  structure T = TestResultDecisionTree
  infixr 0 $
  fun f $ x = f x
  type sid = string
  type tid = string
  type tnum = int

  type tmark = (tid * tnum)
  type report = (tmark * string) list
  val reportSize = 3

  fun make tree db =
      let fun describe (sid, (tid, tnum)) =
              let val tnum = Int.toString tnum
                  val outcome = DB.lookup (tid, tnum, sid, db)
              in case outcome
                  of Outcome.PASSED => "you passed."
                   | (Outcome.NOTPASSED { witness = w, ...}) => w ^ "."
                   | DNR => raise Invariant $ "Got DNR for " ^ String.concatWith " " [sid, tid, tnum]
              end
      in case tree
           of (T.Leaf sIds) => map (fn sId => (sId, [])) sIds
            | t => nonemptyReport t describe
      end

  and nonemptyReport t describe =
      let fun reportOfTree' (T.Leaf sIds) wDecisions =
              let val wDecisions = ListMergeSort.sort (fn ((e0, _), (e1, _)) => e0 < e1) wDecisions
                  val tmarks = map (fn (_, tmark) => tmark) wDecisions
                  val highest = List.take (tmarks, Int.min (length wDecisions, reportSize))
                  fun reportFor sid = (sid, map (fn tmark => (tmark, describe (sid, tmark))) highest)
              in  map reportFor sIds
              end
            | reportOfTree' (T.Branch (entropy, decision, subs)) wDecisions =
              let val wDecisions' = (entropy, decision) :: wDecisions
                  fun report (_, t) = reportOfTree' t wDecisions'
              in  List.concat $ map report subs
              end
      in  reportOfTree' t []
      end

  fun format reports =
      let fun fmtEntry ((tid, tnum), description) =
              String.concatWith " " ["On", tid, "test", Int.toString tnum ^ ","
                                    , description
                                    ]
          fun fmtReportFor sId report =
              let val entries = map fmtEntry report
              in  String.concatWith "\n" $
                                    sId :: map (fn e => "  " ^ e) entries
                                    @ [""]
              end
      in  String.concatWith "\n" $ map (fn (s, r) => fmtReportFor s r) reports
      end
end
