structure ReportUtil :> sig
              type sid = string
              type tid = string
              type tnum = int

              type tmark = (tid * tnum)
              val describe : DB.db -> (sid * tmark) -> string
              val fmtFeedback : (tmark * string) -> string
          end
= struct
  infixr 0 $
  fun f $ x = f x
  exception Invariant of string
  type sid = string
  type tid = string
  type tnum = int

  type tmark = (tid * tnum)
  type report = { feedback : (tmark * string) list
                , comments : string list
                }

  fun describe db (sid, (tid, tnum)) =
      let val tnum = Int.toString tnum
          val outcome = DB.lookup (tid, tnum, sid, db)
      in case outcome
          of (Outcome.PASSED "") => "you passed."
           | (Outcome.PASSED w) => w
           | (Outcome.NOTPASSED { witness = w, ...}) => w ^ "."
           | DNR => raise Invariant $ "Got DNR for " ^ String.concatWith " " [sid, tid, tnum]
      end

  fun fmtFeedback ((tid, tnum), description) =
      String.concatWith " " ["On", tid, "test", Int.toString tnum ^ ","
                            , description
                            ]
end

structure DecisionTreeReport :> sig
              type sid = string
              type report
              val make : TestResultDecisionTree.decisionTree -> DB.db -> (sid * report) list
              val utlnEntries : (sid * report) list -> Utln.entry list
          end
= struct
  structure T = TestResultDecisionTree
  infixr 0 $
  fun f $ x = f x
  type sid = string
  type tid = string
  type tnum = int

  type tmark = (tid * tnum)
  type report = { feedback : (tmark * string) list }
  val reportSize = 3

  fun make (T.Leaf sIds) _ = map (fn sId => (sId, { feedback = [] })) sIds
    | make t db = nonemptyReport t (ReportUtil.describe db)

  and nonemptyReport t describe =
      let fun reportOfTree' (T.Leaf sIds) wDecisions =
              let val wDecisions = ListMergeSort.sort (fn ((e0, _), (e1, _)) => e0 < e1) wDecisions
                  val tmarks = map (fn (_, tmark) => tmark) wDecisions
                  val highest = List.take (tmarks, Int.min (length wDecisions, reportSize))
                  fun reportFor sid = (sid, { feedback = map (fn tmark => (tmark, describe (sid, tmark))) highest })
              in  map reportFor sIds
              end
            | reportOfTree' (T.Branch (entropy, decision, subs)) wDecisions =
              let val wDecisions' = (entropy, decision) :: wDecisions
                  fun report (_, t) = reportOfTree' t wDecisions'
              in  List.concat $ map report subs
              end
      in  reportOfTree' t []
      end

  fun utlnEntries reports =
      let fun entryFor (sid, {feedback}) =
              { sid = sid
              , grade = Grade.UNKNOWN "I"
              , commentary = map ReportUtil.fmtFeedback feedback
              , internalComments = []
              }
      in  map entryFor reports
      end
end

structure TestWeightOfEvidenceReport :> sig
              type sid = string
              type report
              val make : int -> DB.db -> Grade.grade Map.map -> (sid * report) list
              val mergedUtlnEntries : (sid * (report list)) list -> Utln.entry list
              val utlnEntries : (sid * report) list -> Utln.entry list
          end
= struct
  infixr 0 $
  fun f $ x = f x
  exception Invariant of string
  fun eprint s = TextIO.output(TextIO.stdErr, s)
  fun eprintln s = (eprint $ s ^ "\n")
  structure H = Histogram
  structure U = Util
  val (    combinations,   breakTies,   invertOrdering,   fmtReal,   fmtReal')
      = (U.combinations, U.breakTies, U.invertOrdering, U.fmtReal, U.fmtReal')
  structure SolutionMap = BinaryMapFn(SolutionKey)
  type sid = string
  type tid = string
  type tnum = int

  type tmark = (tid * tnum)
  type outcome = string
  type report = { feedback : (tmark * string) list
                , grade : Grade.grade
                , weight : real
                , ties : int
                , prior : real
                , posterior : real
                , posteriorC : real
                }

  type observation = (tmark * outcome) list
  fun tmarks obs = map (fn (tm, _) => tm) obs
  fun compareWitness db sid (obs0, obs1) =
      let fun lengthWits tmarks =
              foldl op + 0 (map (fn t => String.size $ ReportUtil.describe db (sid, t)) tmarks)
          val tmarks0 = tmarks obs0
          val tmarks1 = tmarks obs1
      in  Int.compare (lengthWits tmarks0, lengthWits tmarks1)
      end

  structure ObservationKey = struct
      type ord_key = observation
      fun compare rows =
          let fun compareRows ((tmark0, outcome0), (tmark1, outcome1)) =
                  case TmarkKey.compare (tmark0, tmark1)
                   of EQUAL => String.compare (outcome0, outcome1)
                    | order => order
          in  List.collate compareRows rows
          end
    end
  structure ObservationMap = BinaryMapFn(ObservationKey)
  type histogram = Grade.grade H.counter
  type map = histogram ObservationMap.map

  fun fobs f obs m =
      let val h = ObservationMap.find (m, obs)
      in  f $ getOpt (h, H.zeroes)
      end
  fun fmap f obs m = ObservationMap.insert (m, obs, fobs f obs m)
  fun enumerate numRows sid db add init =
      let val db = DB.restrict db [sid]
          val tmarks = TestUtil.tmarksOfDb db
          fun withOutcome (tid, tnum) =
              let val outcome = DB.lookup (tid, Int.toString tnum, sid, db)
              in  ((tid, tnum), Outcome.toString outcome)
              end
          val observations = combinations (map withOutcome tmarks) numRows
      in  foldl add init observations
      end

  fun printWeights enumerate db sid weight =
      let fun debugWeights (obs, _) =
              let val w = weight obs
                  fun fmtRow ((tid, tnum), out) = "(" ^ tid ^ " " ^ Int.toString tnum ^ " " ^ out ^ ")"
                  val rows = map fmtRow obs
              in  eprintln $ String.concatWith " " (sid :: rows @ [fmtReal w])
              end
      in ( enumerate sid db debugWeights ()
         ; eprintln ""
         )
      end

  fun oddsAgainstFor sids grades =
      let val numStudents = length sids
          val pairs = map (fn sid => (Map.lookup (sid, grades), sid)) sids
          val gradeHistogram = Entropy.histogram pairs
          fun oddsAgainst g =
              let val numG = Entropy.H.count (g, gradeHistogram)
              in  real (numStudents - numG) / real numG
              end
      in  oddsAgainst
      end

  fun make n db grades =
      let val sids = TestUtil.sidsOfDb db
          fun enumerate' db sid weight = enumerate n db sid weight
          val enumerate = enumerate'
          (* val enumerate = enumerate n *)
          fun update (sid, m) =
              let val grade = Map.lookup (sid, grades)
                  fun add (obs, m) = fmap (fn h => H.inc (grade, h)) obs m
              in  enumerate sid db add m
              end
          val observationMap = foldl update ObservationMap.empty sids
          val oddsAgainst = oddsAgainstFor sids grades
          fun reportFor (sid, m) =
              let val db = DB.restrict db [sid]
                  val g = Map.lookup (sid, grades)
                  fun weight obs =
                      let fun evidenceFor h =
                              let val similar = H.count (g, h)
                                  val total = H.total h
                              in  real similar /
                                  (if similar = total
                                   then 0.5
                                   else real (total - similar))
                              end
                      in  10.0 * (Math.log10 $ fobs evidenceFor obs observationMap * oddsAgainst g)
                      end
                  fun compareWeight (obs0, obs1) = Real.compare (weight obs0, weight obs1)
                  val compareObservation =
                      breakTies (compareWeight,
                                 breakTies (invertOrdering (compareWitness db sid),
                                            invertOrdering ObservationKey.compare))
                  fun observationLt args = compareObservation args = LESS
                  val observations = ListMergeSort.sort observationLt $ enumerate sid db op :: []
                  val (tmarks, weight, ties, posteriorC) =
                      case observations
                        of (obs :: rest) =>
                           let val w = weight obs
                               val ties = length $ List.filter (fn obs' => Real.== (w, weight obs')) rest
                               fun posteriorOf h =
                                   let val similar = H.count (g, h)
                                       val total = H.total h
                                   in  real similar / real total
                                   end
                               val posterior = fobs posteriorOf obs observationMap
                           in (tmarks obs, w, ties, 10.0 * (Math.log10 $ (posterior / (1.0 - posterior))))
                           end
                         | _ => raise Invariant $ "Found no observations for " ^ sid
                  val reports = map (fn tm => (tm, ReportUtil.describe db (sid, tm))) tmarks
                  val prior = 10.0 * Math.log10 (1.0 / oddsAgainst g)
              in  SolutionMap.insert (m, sid, { feedback = reports
                                              , grade = g
                                              , weight = weight
                                              , ties = ties
                                              , prior = prior
                                              , posterior = prior + weight
                                              , posteriorC = posteriorC
                                              })
              end
          val reports = foldl reportFor SolutionMap.empty sids
      in  SolutionMap.listItemsi reports
      end

  fun entryFor (sid, {feedback, grade, weight, ties, prior, posterior, posteriorC}) =
      let val fmt1 = fmtReal' 1
          val fmt2 = fmtReal' 2
          val stats = String.concatWith ", " [ "Weight: " ^ fmt2 weight
                                             , "Prior: " ^ fmt1 prior
                                             ]
          val posteriorStat =
              if Real.isFinite posteriorC andalso Real.abs (posterior - posteriorC) < 0.01
              then fmt1 posterior
              else if Real.isFinite posteriorC
              then fmt2 posterior ^ "; **" ^ fmt2 posteriorC ^ "**"
              else fmt1 posterior ^ "; " ^ fmt1 posteriorC
          val stats = stats ^ ", Posterior: " ^ posteriorStat
          val stats = stats ::
                      (case ties
                        of 0 => []
                         | _ => ["Ties for highest weight: " ^ Int.toString ties])
      in
          { sid = sid
          , grade = grade
          , commentary = map ReportUtil.fmtFeedback feedback
          , internalComments = stats
          }
      end
  fun utlnEntries reports = map entryFor reports

  fun mergedUtlnEntries reportss =
      let fun reportsLt ((sid0, rs0 : report list), (sid1, rs1 : report list)) =
              let fun reportLt (r0 : report, r1 : report) = #weight r0 < #weight r1
                  val rs0 = ListMergeSort.sort reportLt rs0
                  val rs1 = ListMergeSort.sort reportLt rs1
              in  case (rs0, rs1)
                   of (r0 :: _, r1 :: _) => reportLt (r0, r1)
                    | _ => Impossible.impossible "both report lists should be nonempty"
              end
          val reportss = ListMergeSort.sort reportsLt reportss
          val reportEntry = entryFor
          fun mergeEntries (e1 : Utln.entry, e2 : Utln.entry) =
              { sid = #sid e1
              , grade = #grade e1
              , commentary = #commentary e1 @ "" :: #commentary e2
              , internalComments = #internalComments e1 @ "" :: #internalComments e2
              }
          fun entryFor (sid, r :: rest) =
              let fun merge (r, e) = mergeEntries (e, reportEntry (sid, r))
              in  foldl merge (reportEntry (sid, r)) rest
              end
            | entryFor _ = Impossible.impossible "each report list should be nonempty"
      in  map entryFor reportss
      end
end
