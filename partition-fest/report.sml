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
          of Outcome.PASSED => "you passed."
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
              val make : DB.db -> Grade.grade Map.map -> (sid * report) list
              val utlnEntries : (sid * report) list -> Utln.entry list
          end
= struct
  infixr 0 $
  fun f $ x = f x
  exception Invariant of string
  fun eprint s = TextIO.output(TextIO.stdErr, s)
  fun eprintln s = (eprint $ s ^ "\n")
  structure H = Histogram
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

  (* The observation type determines how many entries a student gets
     back in their report. For just (tmark * outcome), they'll only
     get one; for (tmark * outcome * tmark * outcome), they'll get
     two; etc. If it's a list type then we probably need to be careful
     about the sizes of the list---e.g. that all of them need to be
     the same size.
   *)
  type observation = ((tmark * outcome) * (tmark * outcome))
  fun tmarks ((tmark0, _), (tmark1, _)) = [tmark0, tmark1]
  fun shorterWitness db sid (obs0, obs1) =
      let fun lengthWits tmarks =
              foldl op + 0 (map (fn t => String.size $ ReportUtil.describe db (sid, t)) tmarks)
          val tmarks0 = tmarks obs0
          val tmarks1 = tmarks obs1
      in  lengthWits tmarks0 < lengthWits tmarks1
      end

  structure ObservationKey = struct
      type ord_key = observation
      fun compare ((t00, t01), (t10, t11)) =
          let fun compare' ((tmark0, outcome0), (tmark1, outcome1)) =
                  case TmarkKey.compare (tmark0, tmark1)
                   of EQUAL => String.compare (outcome0, outcome1)
                    | order => order
          in  case compare' (t00, t10)
                of EQUAL => compare' (t01, t11)
                 | order => order
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
  fun enumerate sid db add init =
      let val db = DB.restrict db [sid]
          val tmarks = TestUtil.tmarksOfDb db
          fun withOutcome (tid, tnum) =
              let val outcome = DB.lookup (tid, Int.toString tnum, sid, db)
              in  ((tid, tnum), Outcome.toString outcome)
              end
          val observations = Util.combinations (map withOutcome tmarks) 2
          fun add' ([obs0, obs1], v) = add ((obs0, obs1), v)
            | add' _ = let exception ThisCan'tHappen in raise ThisCan'tHappen end
      in  foldl add' init observations
      end

  fun printWeights db sid weight =
      let fun debugWeights (obs as (((tid0, tnum0), outcome0), ((tid1, tnum1), outcome1)), _) =
              let val w = weight obs
                  val tnum0 = Int.toString tnum0
                  val tmark0 = tid0 ^ "," ^ tnum0
                  val tnum1 = Int.toString tnum1
                  val tmark1 = tid0 ^ "," ^ tnum1
              in  eprintln $ String.concatWith " " [sid, tmark0, outcome0, tmark1, outcome1, Util.fmtReal w]
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

  fun make db grades =
      let val sids = TestUtil.sidsOfDb db
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
                  fun biggerObs (obs, NONE) = SOME (obs, weight obs)
                    | biggerObs (obs1, SOME (obs0, w0)) = let val w1 = weight obs1
                                                          in if w0 > w1
                                                             then SOME (obs0, w0)
                                                             else SOME (obs1, w1)
                                                          end
                  fun observationLt (obs0, obs1) =
                      weight obs0 < weight obs1
                      orelse shorterWitness db sid (obs1, obs0)
                      orelse ObservationKey.compare (obs1, obs0) = LESS
                  val observations = ListMergeSort.sort observationLt $ enumerate sid db op :: []
                  val (tmark0, tmark1, weight, ties, posteriorC) =
                      case observations
                        of ((obs as ((t0, _), (t1, _))) :: rest) =>
                           let val w = weight obs
                               val ties = length $ List.filter (fn obs' => Real.== (w, weight obs')) rest
                               fun posteriorOf h =
                                   let val similar = H.count (g, h)
                                       val total = H.total h
                                   in  real similar / real total
                                   end
                               val posterior = fobs posteriorOf obs observationMap
                           in (t0, t1, w, ties, 10.0 * (Math.log10 $ (posterior / (1.0 - posterior))))
                           end
                         | _ => raise Invariant $ "Found no observations for " ^ sid
                  val report0 = (tmark0, ReportUtil.describe db (sid, tmark0))
                  val report1 = (tmark1, ReportUtil.describe db (sid, tmark1))
                  val prior = 10.0 * Math.log10 (1.0 / oddsAgainst g)
              in  SolutionMap.insert (m, sid, { feedback = [report0, report1]
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

  fun utlnEntries reports =
      let val fmt1 = Util.fmtReal' 1
          val fmt2 = Util.fmtReal' 2
          fun entryFor (sid, {feedback, grade, weight, ties, prior, posterior, posteriorC}) =
              let val stats = String.concatWith ", " [ "Weight: " ^ fmt1 weight
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
          fun reportLt ((sid0, r0 : report), (sid1, r1 : report)) = #weight r0 < #weight r1
          val reports = ListMergeSort.sort reportLt reports
      in  map entryFor reports
      end
end
