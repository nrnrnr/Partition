structure TestResultDecisionTree :> sig
              type sid = string
              type tid = string
              type tnum = int

              type element = sid
              type decision = (tid * tnum)
              datatype decisionTree = Leaf of element list
                                    | Branch of (string * decision * (string option * decisionTree) list)

              val make : DB.db -> decisionTree
              val labeledDecisions : decisionTree -> string list
              val toDot : decisionTree -> Dot.graph
          end
= struct
  exception Invariant of string
  infixr 0 $
  fun f $ x = f x
  type sid = string
  type tid = string
  type tnum = int
  structure StringMap = BinaryMapFn(BasicStringKey)

  type element = sid
  type decision = (tid * tnum)
  type split = { decided : (decision * (string option * element list) list)
               , remaining : decision list
               , label : string
               }

  datatype decisionTree = Leaf of element list
                        | Branch of (string * decision * (string option * decisionTree) list)

  fun make db =
      let val decide = decide db
          val sids = (TestUtil.sidsOfDb db)
          fun treeOfSplit sids NONE = Leaf sids
            | treeOfSplit sids (SOME {decided = (d, sidss), remaining = remaining, label = label}) =
              Branch (label, d, map (fn (l, sids) => (l, treeOfSplit sids (decide sids remaining))) sidss)
      in  treeOfSplit sids $ decide sids (TestUtil.tmarksOfDb db)
      end

  and decide db solutions tmarks =
      let val db = DB.fold (fn entry as (tid, tnum, sid, outcome, db') =>
                               if List.exists (fn sid0 => sid = sid0) solutions
                               then DB.bind entry
                               else db')
                           DB.empty
                           db

          fun addTest ((outcome, solution), sboSoFar) =
              let val others = getOpt (StringMap.find (sboSoFar, outcome), [])
              in StringMap.insert (sboSoFar, outcome, solution :: others)
              end

          fun tmarkInfo (tmark as (tid, tnum)) =
              let val tests = TestUtil.getOneTest tid tnum db
                  val solutionsByOutcome = foldr addTest StringMap.empty tests
              in  (tmark, solutionsByOutcome, Entropy.entropy $ Entropy.histogram tests)
              end
          fun infoLt ((_, _, e0), (_, _, e1)) = e0 < e1

      in  case ListMergeSort.sort infoLt $ map tmarkInfo tmarks
            of [] => NONE (* should only happen when tmarks is empty *)

             | ((tmark as (tid, tnum), solutionsByOutcome, entropy) :: _) =>
               (* The way infos were sorted the head of the list
                  should have the highest entropy. And if that entropy
                  is nonzero then outcomes should be at least a two
                  element list. If we only want the test with highest
                  entropy then we can ignore the rest of the list.
                *)
               let fun subdecisionsOf (outcome, solutions) = (SOME outcome, solutions)
                   val subdecisions = map subdecisionsOf $ StringMap.listItemsi solutionsByOutcome
                   fun label outcomes = String.concatWith " " [ tid
                                                              , Int.toString tnum
                                                              , "(" ^ Real.fmt (StringCvt.FIX $ SOME 3) entropy ^ ")"
                                                              ]

               in  case map (fn (out, _) => out) $ StringMap.listItemsi solutionsByOutcome

                    of [_] => NONE

                     | outs as (_ :: _) =>
                       SOME { decided = (tmark, subdecisions)
                            , remaining = List.filter (fn tm => tm <> tmark) tmarks
                            , label = label outs
                            }
                            
                    | _ => raise Invariant ("Got no outcomes for test '" ^ label [] ^ "'")
               end
      end
  val decide : DB.db -> element list -> decision list -> split option = decide

  fun labeledDecisions (Leaf _) = []
    | labeledDecisions (Branch (label, decision, trees)) =
      let val subdecisions = map (fn (_, tree) => labeledDecisions tree) trees
      in  label :: List.concat subdecisions
      end

  fun toDot tree =
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
          fun toDot fromName (Leaf sIds) =
              let val name = nextName ()
                  val label = Util.renderSolutionIdsNarrow sIds
              in  ( [ Dot.node { name = name, label = label } ]
                  , [ Dot.edge { from = fromName, to = name } ]
                  )
              end
            | toDot fromName (Branch (label, _, subtrees)) =
              let val branchName = nextName ()
                  fun subtreeToDot (subdecision, t)  =
                      let val subtreeName = nextName ()
                          val (ns, es) = toDot subtreeName t
                      in  ( Dot.node { name = subtreeName, label = getOpt (subdecision, "") } :: ns
                          , Dot.edge { from = branchName, to = subtreeName } :: es
                          )
                      end
                  fun appendGraphs ((ns0, es0), (ns, es)) = (foldr op :: ns ns0, foldr op :: es es0)
                  val (ns, es) = foldr appendGraphs ([], []) $ map subtreeToDot subtrees
              in  ( Dot.node { name = branchName, label = label } :: ns
                  , Dot.edge { from = fromName, to = branchName } :: es
                  )
              end
          val startName = "S"
          val (ns, es) = toDot startName tree
      in  (Dot.node { name = startName, label = "S" } :: ns, es)
      end
end
