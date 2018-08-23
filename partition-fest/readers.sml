
structure OutcomeReader : sig
    type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : Outcome.outcome
           }


    val lex     : string -> string list (* for debugging only *)
    val outcome : string -> t
    val outcome2 : string -> string list option
end
= struct
    type t = { testid  : string    (* source of the test *)
           , num     : int       (* number of the test from that source *)
           , solnid  : string    (* identity of the solution *)
           , outcome : Outcome.outcome
           }
  structure I = Impossible

  open Lex (* never do this *)

  infix 0 |||
  infix 2 >>> 
  infix 2 >>! 
  infix 5 ---


  fun getWitness (_, cs) =
      let val cs = Util.dropWhile Char.isSpace cs
          val cs = rev (Util.dropWhile Char.isSpace (rev cs))
      in  SOME (implode cs, [])
      end

  fun isDelim c = c = #","   (* limited notion of delim *)
  fun idChar c = Char.isAlphaNum c orelse c = #"-" orelse c = #"."
                                   orelse c = #"/" orelse c = #"?"

  val lex : string -> string list = 
     (* precondition: no space characters *)
    let fun token chars = dropwhite (  
            charEq #"-" --- charEq #"-"                  >>! getWitness
        ||| charSat isDelim                              >>> str
        ||| repeat1Chars idChar                          >>> implode
        ||| anyChar >>! invalid
        ) chars
       and invalid (c, cs) =
            I.impossible ("invalid initial character in `" ^ implode (c::cs) ^ "'")
    in #1 o valOf o repeatLex token o explode
  end


  fun toOutcome ["-given", id, "test", num, ",", soln, "passed"] =
        finish id num soln (Outcome.PASSED "")
    | toOutcome ["-given", id, "test", num, ",", soln, badthing, witness] =
        finish id num soln (Outcome.NOTPASSED { outcome = badthing
                                              , witness = witness })
    | toOutcome ["given", id, "test", num, ",", soln, "passed", witness] =
        finish id num soln (Outcome.PASSED witness)
    | toOutcome ["given", id, "test", num, ",", soln, badthing, witness] =
        finish id num soln (Outcome.NOTPASSED { outcome = badthing
                                              , witness = witness })

    | toOutcome _ = I.impossible "ill-formed input line"


  and finish id num soln outcome =
        case Int.fromString num
          of NONE   => I.impossible "test number would not convert to integer"
           | SOME n => { testid = id, num = n, solnid = soln, outcome = outcome }
  fun test ["-given", id, "test", num, ",", soln, "passed"] =
        NONE
    | test ["-given", id, "test", num, ",", soln, badthing, witness] =
	NONE
    | test x = SOME x;


  val outcome : string -> t = toOutcome o lex

  val outcome2 : string -> string list option = test o lex

  structure UnitTests = struct

    val s1 = "-given bounds-check test 1, akhaku01 passed" : string
    val s2 = "-given full-methods test 3, bleike01 failed -- missing mapping methods"

    val o1 = { testid = "bounds-check", num = 1, solnid = "akhaku01"
             , outcome = Outcome.PASSED "" }

    val o2 = { testid = "full-methods", num = 3, solnid = "bleike01"
             , outcome = Outcome.NOTPASSED { outcome = "failed"
                                           , witness = "missing mapping methods" }
             }

    val _ = I.assert (outcome s1 = o1)
    val _ = I.assert (outcome s2 = o2)
        
  end

end

structure GradeReader :> sig
  val readToMap : TextIO.instream -> Grade.grade Map.map
end = struct
    val (VG, G, F, P, NC, UNKNOWN) = (Grade.VG, Grade.G, Grade.F, Grade.P, Grade.NC, Grade.UNKNOWN)
    exception InvalidUtln = D.InvalidUtln

    fun invalidLine l = raise InvalidUtln ("Unrecognized line: '" ^ l ^ "'")
    fun utlnLinesToMap [] = raise InvalidUtln "Tried to read grades from a UTLN file with no lines"
      | utlnLinesToMap (header :: rest) =
        let fun isBodyLine s = Char.isSpace (String.sub (s, 0))
            fun getEntry nameLine rest =
              case String.tokens Char.isSpace nameLine
                of [name, g] => (Util.dropWhile isBodyLine rest, (name, Grade.ofString g))
                 | _ => raise InvalidUtln ("Didn't get two tokens in name line: " ^ nameLine)
            fun loop [] entries = entries
              | loop (l0 :: ls) entries =
                case String.tokens Char.isSpace l0
                 of ("[[" :: _) => loop ls entries (* not sure if this is valid UTLN format *)
                  | ("]]" :: _)  => loop ls entries
                  | macro :: "=" :: _ => loop (Util.dropWhile isBodyLine ls) entries
                  | [_, _] =>
                    let val (rest, (name, grade)) = getEntry l0 ls
                    in  loop rest (Map.bind (name, grade, entries))
                    end
                  | (s :: rest) => if String.sub (s, 0) = #"<" then
                                       loop ls entries
                                   else
                                       invalidLine l0
                  | [] => loop ls entries
        in  loop rest Map.empty
        end

    fun readToMap fd =
      let fun fdLines sofar =
            case TextIO.inputLine fd
             of NONE => rev sofar
              | SOME l => fdLines (l :: sofar)
      in  utlnLinesToMap (fdLines [])
      end

    structure UnitTests = struct
        val assert = Impossible.assert
        val lines = [ "utln solver"
                    , "aph01   VG"
                    , "  We found no faults in this code."
                    , "wasp11  G"
                    , "bee   F"
                    , "asp NR"
                    ]
        val grades : Grade.grade Map.map = utlnLinesToMap lines
        fun gradeOf id = Map.lookup (id, grades)

        val () = app assert [ gradeOf "aph01" = VG
                            , gradeOf "wasp11" = G
                            , gradeOf "ajksdl" = NC
                              handle Map.NotFound "ajksdl" => true
                            , gradeOf "asp" = UNKNOWN "NR"
                            ]
    end
end
