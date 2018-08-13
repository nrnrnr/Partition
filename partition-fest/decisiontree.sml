structure StringMap = BinaryMapFn(BasicStringKey)

signature TEST_DECISION_TREE
= sig
    type sid = string
    type tid = string
    type tnum = int

    type element = sid
    type decision = (tid * tnum)
    datatype decisionTree = Leaf of element list
                          | Branch of (real * decision * (string option * decisionTree) list)

    val make : { outcomes : DB.db
               , informationGain : DB.db -> (tid * tnum) -> (sid list StringMap.map * real)
               } -> decisionTree
    val labeledDecisions : decisionTree -> string list
    val toDot : decisionTree -> Dot.graph
    val toDotWithGrades : decisionTree -> Grade.grade Map.map -> Dot.graph
end

signature INFORMATION_GAIN
= sig
    type sid = string
    type tid = string
    type tnum = int
    type gain = (sid list StringMap.map * real)

    val forStudentTree : DB.db -> (tid * tnum) -> gain
end

structure TestResultDecisionTree :> TEST_DECISION_TREE
= struct
  exception Invariant of string
  infixr 0 $
  fun f $ x = f x
  type sid = string
  type tid = string
  type tnum = int

  type element = sid
  type decision = (tid * tnum)
  type split = { decided : (decision * (string option * element list) list)
               , remaining : decision list
               , entropy : real
               }

  datatype decisionTree = Leaf of element list
                        | Branch of (real * decision * (string option * decisionTree) list)

  fun make {outcomes=db, informationGain} =
      let val decide = (fn db => fn tmarks => decide db informationGain tmarks)
          fun treeOfSplit sids NONE = Leaf sids
            | treeOfSplit sids (SOME {decided = (d, sidss), remaining = remaining, entropy = entropy}) =
              let fun subdecide (l, sids) = (l, treeOfSplit sids (decide (restrict db sids) remaining))
              in  Branch (entropy, d, map subdecide sidss)
              end
      in  treeOfSplit (TestUtil.sidsOfDb db) $ decide db (TestUtil.tmarksOfDb db)
      end

  and decide db0 informationGain tmarks =
      let fun infoLt ((_, _, e0), (_, _, e1)) = e0 < e1
          fun tmarkInfoGain tmark =
              let val (solutionsByOutcome, entropy) = informationGain db0 tmark
              in  (tmark, solutionsByOutcome, entropy)
              end

      in  case ListMergeSort.sort infoLt $ map tmarkInfoGain tmarks
            of [] => NONE

             | ((tmark as (tid, tnum), solutionsByOutcome, entropy) :: _) =>
               (* The way infos were sorted the head of the list
                  should have the highest entropy. And if that entropy
                  is nonzero then outcomes should be at least a two
                  element list. If we only want the test with highest
                  entropy then we can ignore the rest of the list.
                *)
               let fun subdecisionsOf (outcome, solutions) = (SOME outcome, solutions)
                   val subdecisions = map subdecisionsOf $ StringMap.listItemsi solutionsByOutcome

               in  case map (fn (out, _) => out) $ StringMap.listItemsi solutionsByOutcome

                    of [_] => NONE

                     | outs as (_ :: _) =>
                       SOME { decided = (tmark, subdecisions)
                            , remaining = List.filter (fn tm => tm <> tmark) tmarks
                            , entropy = entropy
                            }
                            
                    | _ => raise Invariant ("Got no outcomes for test '" ^ tid ^ " " ^ Int.toString tnum ^ "'")
               end
      end

  and restrict db solutions =
      DB.fold (fn entry as (tid, tnum, sid, outcome, db') =>
                  if List.exists (fn sid0 => sid = sid0) solutions
                  then DB.bind entry
                  else db')
              DB.empty
              db

  fun fmtReal r = Real.fmt (StringCvt.FIX $ SOME 3) r
  fun labelOf entropy (tid, tnum) = String.concatWith " " [ tid
                                                          , Int.toString tnum
                                                          , "(" ^ fmtReal entropy ^ ")"
                                                          ]

  fun labeledDecisions (Leaf _) = []
    | labeledDecisions (Branch (entropy, decision, trees)) =
      let val subdecisions = map (fn (_, tree) => labeledDecisions tree) trees
      in  labelOf entropy decision :: List.concat subdecisions
      end


  fun toDot tree gradesMap =
      let val nextName =
              (* The Dot library /could/ optionally name the nodes for
                 us, with each edge built from the nodes at each end
               *)
              let val lastUsed = ref ~1
              in  fn () => let val next = !lastUsed + 1
                           in  ( lastUsed := next
                               ; "N" ^ Int.toString next
                               )
                           end
              end
          fun toDot mkInEdge (Leaf sIds) =
              let val name = nextName ()
                  val label = Util.renderSolutionIdsNarrow sIds
                  val n = Dot.node { name = name, label = label }
                  val gradeColorAttrs =
                      case gradesMap
                        of NONE => []
                         | SOME gradesMap =>
                           let val {style, fillColor} = Grade.colorIds sIds gradesMap
                           in  [ ("style", style)
                               , ("fillcolor", fillColor)
                               ]
                           end
              in  ( [ Dot.nodeWithAttrs n $ [("shape", "oval")] @ gradeColorAttrs ]
                  , [ mkInEdge name ]
                  )
              end
            | toDot mkInEdge (Branch (entropy, decision, subtrees)) =
              let val branchName = nextName ()
                  val label = labelOf entropy decision
                  fun subtreeToDot (subdecision, t)  =
                      let fun mkSubtreeInEdge to =
                              let val e = Dot.edge { from = branchName, to = to }
                              in  Dot.edgeWithAttrs e [ ("label", getOpt (subdecision, ""))
                                                      , ("penwidth", fmtReal $ edgeWidth entropy)
                                                      , ("color", "gray50")
                                                      ]
                              end
                      in toDot mkSubtreeInEdge t
                      end
                  val n = Dot.node { name = branchName, label = label }
                  val (ns, es) = foldr Dot.appendGraphs ([], []) $ map subtreeToDot subtrees
              in  ( Dot.nodeWithAttrs n [("shape", "rect")] :: ns
                  , mkInEdge branchName :: es
                  )
              end
          val startName = "S"
          val start = Dot.node { name = startName, label = "S" }
          val (ns, es) = toDot (fn to => Dot.edge { from = startName , to = to }) tree
      in  (Dot.nodeWithAttrs start [("style", "invis")] :: ns, es)
      end
  and edgeWidth entropy = entropy * 2.5

  val (toDot, toDotWithGrades) =
      ( (fn tree => toDot tree NONE)
      , (fn tree => fn gradesMap => toDot tree (SOME gradesMap))
      )
end

structure InformationGain :> INFORMATION_GAIN
= struct
  type sid = string
  type tid = string
  type tnum = int
  type gain = (sid list StringMap.map * real)

  fun forStudentTree db tmark =
      let fun addTest ((outcome, solution), sboSoFar) =
              let val others = getOpt (StringMap.find (sboSoFar, outcome), [])
              in StringMap.insert (sboSoFar, outcome, solution :: others)
              end

          fun tmarkInfo (tid, tnum) =
              let val pairs = TestUtil.getOneTest tid tnum db
                  val solutionsByOutcome = foldr addTest StringMap.empty pairs
              in  (solutionsByOutcome, Entropy.entropy (Entropy.histogram pairs))
              end
      in  tmarkInfo tmark
      end
end
