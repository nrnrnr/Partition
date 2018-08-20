structure Utln :> sig
              type entry = { sid : string
                           , grade : Grade.grade
                           , commentary : string list
                           , internalComments : string list
                           }
              val format : string -> entry list -> string
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
end
