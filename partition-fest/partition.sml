structure Partition = struct

  infixr 0 $
  fun f $ x = f x
  fun eprint s = TextIO.output(TextIO.stdErr, s)      

  datatype opt
      = RankClaessen
      | RankUnion
      | WitnessRed
      | Outfile of string
      | Gradesfile of string
      | PrintDistribution of TextIO.outstream

  fun options argv =
    let fun eat (options', "-c" :: argv) = eat (RankClaessen :: options', argv)
          | eat (options', "-u" :: argv) = eat (RankUnion    :: options', argv)
          | eat (options', "-o" :: filename :: argv) =
              eat (Outfile filename :: options', argv)
          | eat (options', "-g" :: filename :: argv) =
              eat (Gradesfile filename :: options', argv)
          | eat (options', "-w" :: argv) = eat (WitnessRed :: options', argv)
          | eat (options', "-d" :: argv) = eat (PrintDistribution TextIO.stdErr :: options', argv)
          | eat (options', argv) = (options', argv)
        val (options', argv) = eat ([], argv)
    in  (rev options', argv)
    end

  fun outopt (Outfile s) = SOME s
    | outopt _           = NONE

  fun outfile options =
    case List.mapPartial outopt options
      of [] => "ranking.dot"  (* default output *)
       | [file] => file
       | xs => List.last xs (* should be an error *)

  fun witnessfile options = "witnesses.out" (* not implemented yet *)
  fun gradesfile [] = NONE
    | gradesfile (Gradesfile s :: _) = SOME s
    | gradesfile (_ :: options) = gradesfile options
  fun distributiondest [] = NONE
    | distributiondest (PrintDistribution out :: _) = SOME out
    | distributiondest (_ :: options) = distributiondest options

  fun doPartition (prog, argv) =
      case options argv
       of (options, [outcomes]) =>
          ( Basis.buildGraph outcomes (outfile options)
                             (witnessfile options)
                             (gradesfile options)
                             (distributiondest options)
                             []
          ; OS.Process.success
          )
        | (options, argv) =>
          ( app eprint ["Usage: ", prog, " partition [-c | -u | -o filename | -g filename | -d] outcomefile\n" ]
          ; eprint "Got these args:" ; app (fn s => app eprint [" ", s]) argv
          ; eprint "\n"
          ; OS.Process.failure
          )




  exception BadOption of string
  fun checkSingleTest tid tnum =
      case Int.fromString tnum
       of SOME tnum =>
          if tnum >= 0
          then D.SingleTest (tid, tnum)
          else raise BadOption ("For single tests, the tnum must be nonnegative; got '" ^ Int.toString tnum ^ "'")
        | NONE => raise BadOption ("For single tests, the tnum must be an integer; got '" ^ tnum ^ "'")


  fun perfect outcomes = List.all (fn out => out = OutcomeSingle.PASSED) outcomes

  fun entropyOptions argv =
      let fun eat (options', "-t" :: tid :: tnum :: argv) = eat (checkSingleTest tid tnum :: options', argv)
            | eat (options', "--single" :: tid :: tnum :: argv) = eat (checkSingleTest tid tnum :: options', argv)
            | eat (options', "--individual" :: argv) = eat (D.IndividualTests :: options', argv)
            | eat (options', "--all" :: argv) = eat (D.AllTests (fn x => true) :: options', argv)
            | eat (options', "--all-imperfect" :: argv) = (D.AllTests (not o perfect) :: options', argv)
            | eat (options', argv) = (options', argv)
          val (options', argv) = eat ([], argv)
      in  (rev options', argv)
      end

  fun success s = ( TextIO.output (TextIO.stdOut, s ^ "\n")
                  ; OS.Process.success
                  )

  fun doEntropy (prog, argv) =
      (case entropyOptions argv
        of ([whichTest], [outcomes]) => success (Basis.renderEntropy whichTest outcomes)
         | ([], [outcomes]) => success (Basis.renderEntropy (D.AllTests (fn x => true)) outcomes)
         | (options, argv) =>
           ( app eprint ["Usage: ", prog, " entropy [--single tid tnum | --all | --all-imperfect | --individual] outcomes\n"]
           ; eprint "Got these args:" ; app (fn s => app eprint [" ", s]) argv
           ; eprint "\n"
           ; OS.Process.failure
      ))
      handle BadOption s => (app eprint [s, "\n"] ; OS.Process.failure)

  fun doTree (prog, argv') =
      let val (options, argv) = options argv'
          fun gradesFile [] = (NONE, [])
            | gradesFile (Gradesfile s :: rest) = (SOME s, rest)
            | gradesFile (other :: rest) =
              let val (g, r') = gradesFile rest
              in  (g, other :: r')
              end
          val (grades, options) = gradesFile options
          fun makeStudentTree outcomes =
              TestResultDecisionTree.make { outcomes = outcomes
                                          , informationGain = InformationGain.forStudentTree
                                          }
          val makeTree = makeStudentTree o FileReader.readToMap
          val success = (fn t => success $ Dot.toString t)
      in case (grades, argv)
           of (SOME grades, [outcomes]) =>
              let val tree = Util.withInputFromFile outcomes makeTree
                  val grades = Util.withInputFromFile grades GradeReader.readToMap
              in  success $ TestResultDecisionTree.toDotWithGrades tree grades
              end
            | (NONE, [outcomes]) =>
              let val tree = Util.withInputFromFile outcomes makeTree
              in  success $ TestResultDecisionTree.toDot tree
              end
            | _ =>
              ( eprint (String.concatWith " " ["Usage:", prog, "decision-tree [-g filename] outcomes\n"])
              ; eprint "Got these args : "; app (fn s => app eprint [" ", s]) argv'
              ; eprint "\n"
              ; OS.Process.failure
              )
      end

  fun doReport (prog, [outcomes]) =
      let val db = Util.withInputFromFile outcomes FileReader.readToMap
          val tree = TestResultDecisionTree.make { outcomes = db
                                                 , informationGain = InformationGain.forStudentTree
                                                 }
      in  success $ DecisionTreeReport.format $ DecisionTreeReport.make tree db
      end
    | doReport (prog, argv) =
      ( eprint (String.concatWith " " ["Usage:", prog, "report outcomes\n"])
      ; eprint "Got these args : "; app (fn s => app eprint [" ", s]) argv
      ; eprint "\n"
      ; OS.Process.failure
      )

  fun run (prog, argv) =
      let val (mode, argv) =
              case argv
               of ("partition" :: argv) => (doPartition, argv)
                | ("entropy" :: argv) => (doEntropy, argv)
                | ("decision-tree" :: argv) => (doTree, argv)
                | ("report" :: argv) => (doReport, argv)
                | _ => (doPartition, argv)
      in  mode (prog, argv)
      end
end
