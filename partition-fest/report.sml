structure ReportUtil :> sig
              type sid = string
              type tid = string
              type tnum = int

              type tmark = (tid * tnum)
              type report = { feedback : (tmark * string) list
                            , comments : string list
                            }
              val describe : DB.db -> (sid * tmark) -> string
              val format : (sid * report) list -> string
              val empty : report
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

  fun format reports =
      let fun fmtEntry ((tid, tnum), description) =
              String.concatWith " " ["On", tid, "test", Int.toString tnum ^ ","
                                    , description
                                    ]
          fun fmtReportFor sId {feedback, comments} =
              let val entries = map fmtEntry feedback
                  val comments = map (fn c => "  (X: " ^ c ^ ")") comments
              in  String.concatWith "\n" $
                                    sId :: map (fn e => "  " ^ e) entries
                                    @ comments
                                    @ [""]
              end
      in  String.concatWith "\n" $ map (fn (s, r) => fmtReportFor s r) reports
      end
  val empty = { feedback = [], comments = [] }
end

structure DecisionTreeReport :> sig
              type sid = string
              type tid = string
              type tnum = int

              type tmark = (tid * tnum)
              type report = { feedback : (tmark * string) list
                            , comments : string list
                            }
              val make : TestResultDecisionTree.decisionTree -> DB.db -> (sid * report) list
              val format : (sid * report) list -> string
          end
= struct
  structure T = TestResultDecisionTree
  infixr 0 $
  fun f $ x = f x
  type sid = string
  type tid = string
  type tnum = int

  type tmark = (tid * tnum)
  type report = { feedback : (tmark * string) list
                , comments : string list
                }
  val reportSize = 3

  fun make (T.Leaf sIds) _ = map (fn sId => (sId, ReportUtil.empty)) sIds
    | make t db = nonemptyReport t (ReportUtil.describe db)

  and nonemptyReport t describe =
      let fun reportOfTree' (T.Leaf sIds) wDecisions =
              let val wDecisions = ListMergeSort.sort (fn ((e0, _), (e1, _)) => e0 < e1) wDecisions
                  val tmarks = map (fn (_, tmark) => tmark) wDecisions
                  val highest = List.take (tmarks, Int.min (length wDecisions, reportSize))
                  fun reportFor sid = (sid, { feedback = map (fn tmark => (tmark, describe (sid, tmark))) highest
                                            , comments = []
                                      })
              in  map reportFor sIds
              end
            | reportOfTree' (T.Branch (entropy, decision, subs)) wDecisions =
              let val wDecisions' = (entropy, decision) :: wDecisions
                  fun report (_, t) = reportOfTree' t wDecisions'
              in  List.concat $ map report subs
              end
      in  reportOfTree' t []
      end

  val format = ReportUtil.format
end

structure TestWeightOfEvidenceReport :> sig
              type sid = string
              type tid = string
              type tnum = int

              type tmark = (tid * tnum)
              type report = { feedback : (tmark * string) list
                            , comments : string list
                            }
              val make : DB.db -> Grade.grade Map.map -> (sid * report) list
              val format : (sid * report) list -> string
          end
= struct
  infixr 0 $
  fun f $ x = f x
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
                , comments : string list
                }

  (* The observation type determines how many entries a student gets
     back in their report. For just (tmark * outcome), they'll only
     get one; for (tmark * outcome * tmark * outcome), they'll get
     two; etc. If it's a list type then we probably need to be careful
     about the sizes of the list---e.g. that all of them need to be
     the same size.
   *)
  type observation = (tmark * outcome)
  structure ObservationKey = struct
      type ord_key = observation
      fun compare ((tmark0, outcome0), (tmark1, outcome1)) =
          case TmarkKey.compare (tmark0, tmark1)
           of EQUAL => String.compare (outcome0, outcome1)
           | order => order
    end
  structure ObservationMap = BinaryMapFn(ObservationKey)
  type histogram = Grade.grade H.counter
  type map = histogram ObservationMap.map

  fun fobs f obs m =
      let val h = ObservationMap.find (m, obs)
      in  f $ getOpt (h, H.zeroes)
      end
  fun fmap f obs m = ObservationMap.insert (m, obs, fobs f obs m)
  fun enumerate db add init =
      let fun add' (tid, tnum, _, outcome, v) =
              let val tnum = valOf (Int.fromString tnum)
                  val obs = ((tid, tnum), Outcome.toString outcome)
              in  add (obs, v)
              end
      in  DB.fold add' init db
      end

  fun printWeights db sid weight =
      let fun debugWeights (obs as ((tid, tnum), outcome), _) =
              let val w = weight obs
                  val tnum = Int.toString tnum
                  val tmark = tid ^ "," ^ tnum
              in  eprintln $ String.concatWith " " [sid, tmark, outcome, Util.fmtReal w]
              end
      in ( enumerate db debugWeights ()
         ; eprintln ""
         )
      end

  fun gradeRatio sids grades =
      let val numStudents = length sids
          val pairs = map (fn sid => (Map.lookup (sid, grades), sid)) sids
          val gradeHistogram = Entropy.histogram pairs
          fun ratioFor g =
              let val numG = Entropy.H.count (g, gradeHistogram)
              in  real (numStudents - numG) / real numG
              end
      in  ratioFor
      end

  fun make db grades =
      let val sids = TestUtil.sidsOfDb db
          fun update (sid, m) =
              let val grade = Map.lookup (sid, grades)
                  val db = DB.restrict db [sid]
                  fun add (obs, m) = fmap (fn h => H.inc (grade, h)) obs m
              in  enumerate db add m
              end
          val observationMap = foldl update ObservationMap.empty sids
          val gradeRatio = gradeRatio sids grades
          fun reportFor (sid, m) =
              let val db = DB.restrict db [sid]
                  fun weight obs =
                      let val g = Map.lookup (sid, grades)
                          fun evidenceFor h =
                              let val similar = H.count (g, h)
                                  val total = H.total h
                              in  if similar = total
                                  then 0.5 (* dodge that NR talked about *)
                                  else real similar / real (total - similar)
                              end
                      in  fobs evidenceFor obs observationMap * gradeRatio g
                      end
                  fun biggerObs (obs, NONE) = SOME (obs, weight obs)
                    | biggerObs (obs1, SOME (obs0, w0)) = let val w1 = weight obs1
                                                          in if w0 > w1
                                                             then SOME (obs0, w0)
                                                             else SOME (obs1, w1)
                                                          end

                  val ((tmark, _), weight) = valOf (enumerate db biggerObs NONE)
                  val report = (tmark, ReportUtil.describe db (sid, tmark))
              in  SolutionMap.insert (m, sid, { feedback = [report]
                                              , comments = ["Weight: " ^ Util.fmtReal weight]
                                              })
              end
          val reports = foldl reportFor SolutionMap.empty sids
      in  SolutionMap.listItemsi reports
      end
  val format = ReportUtil.format
end
