structure Utln :> sig
              type comment
              type entry = { sid : string
                           , grade : Grade.grade
                           , commentary : comment list
                           }
              val format : string -> entry list -> string
              val condense : entry list -> entry list

              val plain : string -> comment
              val excised : string -> comment
              val blank : comment
          end
= struct
  infixr 0 $
  fun f $ x = f x
  datatype comment = PLAIN of string
                   | EXCISED of string
                   | BLANK
  val (plain, excised, blank) = (PLAIN, EXCISED, BLANK)

  type entry = { sid : string
               , grade : Grade.grade
               , commentary : comment list
               }
  fun formatComment BLANK = ""
    | formatComment (PLAIN s) = s
    | formatComment (EXCISED "") = "(X:)"
    | formatComment (EXCISED s) = "(X: " ^ s ^ ")"

  fun format header entries =
      let fun fmtEntry {sid, grade, commentary} =
              let val headerLn = (sid ^ "  " ^ Grade.toString grade ^ "\n")
                  val comments = map (fn c => "  " ^ formatComment c) commentary
              in  headerLn ^ String.concatWith "\n" comments
              end
          val headerLn = ("utln " ^ header ^ "\n")
          val entries = map fmtEntry entries
      in  headerLn ^ String.concatWith "\n\n" entries
      end

  fun condense es =
      let fun same (e0 : entry) (e1 : entry) =
              let val (g0, g1) = (#grade e0, #grade e1)
                  val (c0, c1) = (#commentary e0, #commentary e1)
              in  g0 = g1 andalso c0 = c1
              end
          fun merge (e0 : entry, e1 : entry) =
              { sid = #sid e1 ^ ", " ^ #sid e0
              , grade = #grade e0
              , commentary = #commentary e0
              }
          fun mergeAll [] = []
            | mergeAll (e :: es) =
              let val eq = same e
                  val (same, diff) = List.partition eq es
              in  (foldr merge e same) :: mergeAll diff
              end
      in  mergeAll es
      end
end
