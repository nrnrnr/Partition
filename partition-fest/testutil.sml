structure TestUtil : sig
              type sid = string
              val getAllTests : DB.db -> (Outcome.outcome list -> bool) -> (string list * sid) list
              val getOneTest : string -> int -> DB.db -> (string * sid) list
              val tmarksOfDb : DB.db -> (string * int) list
              val sidsOfDb : DB.db -> string list
          end
= struct
  type sid = string
  infixr 0 $
  fun f $ x = f x

  structure StudentMap = BinaryMapFn(SolutionKey)
  structure Outcomes = BinarySetFn(TestResultKey)
  fun getAllTests outcomes filter =
      let fun addTest (tid, tnum, s, outcome, tests) =
              let val tnum = valOf $ Int.fromString tnum
                  val sTests = getOpt (StudentMap.find (tests, s), Outcomes.empty)
                  val sTests = Outcomes.add (sTests, (tid, tnum, outcome))
              in  StudentMap.insert (tests, s, sTests)
              end
          fun third (_, _, x) = x
          val outcomesByStudent = DB.fold addTest StudentMap.empty outcomes
          (* filter is predicate on Outcome.outcome list, so there's some ceremony
             to convert an Outcomes.item list to an Outcome.outcome list *)
          val filter = filter o (map third) o Outcomes.listItems
          val outcomesByStudent = StudentMap.filter filter outcomesByStudent
      in map (fn (sid, outcomes) => (map (Outcome.toString o third) $ Outcomes.listItems outcomes, sid))
             $ StudentMap.listItemsi outcomesByStudent
      end
  fun getOneTest tid tnum outcomes =
      let val tnum = Int.toString tnum
          fun addTest (tid', tnum', student, outcome, testsSoFar) =
              if tid' = tid andalso tnum' = tnum
              then (Outcome.toString outcome, student) :: testsSoFar
              else testsSoFar
      in  DB.fold addTest [] outcomes
      end

  structure Tmarks = BinarySetFn(TmarkKey)
  fun tmarksOfDb db =
      let fun addTmark (tid, tnum, _, tmarks) = Tmarks.add (tmarks, (tid, valOf $ Int.fromString tnum))
      in  Tmarks.listItems $ DB.foldStudents addTmark Tmarks.empty db
      end

  structure Sids = BinarySetFn(SolutionKey)
  fun sidsOfDb db =
      let fun addSid (_, _, results, sids) = Sids.addList (sids, map (fn (sid, _) => sid) results)
      in  Sids.listItems $ DB.foldStudents addSid Sids.empty db
      end
end
      
