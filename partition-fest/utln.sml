structure Utln :> sig
              type entry = { sid : string
                           , grade : Grade.grade
                           , commentary : string list
                           , internalComments : string list
                           }
              val format : string -> entry list -> string
              val condense : entry list -> entry list
          end
= struct
  infixr 0 $
  fun f $ x = f x
  type entry = { sid : string
               , grade : Grade.grade
               , commentary : string list
               , internalComments : string list
               }
  fun format header entries =
      let fun fmtEntry {sid, grade, commentary, internalComments} =
              let val internalComments = map (fn c => "  (X: " ^ c ^ ")") internalComments
              in  String.concatWith "\n" $
                                    (sid ^ "  " ^ Grade.toString grade) :: map (fn c => "  " ^ c) commentary
                                    @ internalComments
                                    @ [""]
              end
      in  String.concatWith "\n" $ ("utln " ^ header) :: map fmtEntry entries
      end

  fun condense es =
      let fun same (e0 : entry, e1 : entry) =
              let val (g0, g1) = (#grade e0, #grade e1)
                  val (c0, c1) = (#commentary e0, #commentary e1)
                  val (i0, i1) = (#internalComments e0, #internalComments e1)
              in  g0 = g1 andalso c0 = c1 andalso i0 = i1
              end
          fun merge (e0 : entry) (e1 : entry) =
              { sid = #sid e1 ^ ", " ^ #sid e0
              , grade = #grade e0
              , commentary = #commentary e0
              , internalComments = #internalComments e0
              }
          fun mergeIfSame (e0, []) = [e0]
            | mergeIfSame (e0, e :: es) =
              if same (e0, e)
              then merge e0 e :: es
              else e0 :: e :: es
      in  foldr mergeIfSame [] es
      end
end
