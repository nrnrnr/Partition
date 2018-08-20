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
      | StudentTree
      | GradeTree
      | WeightOfEvidence of string
      | TreeReport

  fun options argv =
    let fun eat (options', "-c" :: argv) = eat (RankClaessen :: options', argv)
          | eat (options', "-u" :: argv) = eat (RankUnion    :: options', argv)
          | eat (options', "-o" :: filename :: argv) =
              eat (Outfile filename :: options', argv)
          | eat (options', "-g" :: filename :: argv) =
              eat (Gradesfile filename :: options', argv)
          | eat (options', "-w" :: argv) = eat (WitnessRed :: options', argv)
          | eat (options', "-d" :: argv) = eat (PrintDistribution TextIO.stdErr :: options', argv)
          | eat (options', "--student-tree" :: argv) = eat (StudentTree :: options', argv)
          | eat (options', "--grade-tree" :: argv) = eat (GradeTree :: options', argv)
          | eat (options', "--weight-of-evidence" :: filename :: argv) = eat (WeightOfEvidence filename :: options', argv)
          | eat (options', "--tree-report" :: argv) = eat (TreeReport :: options', argv)
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

  fun gotArgsMsg [] = "No args found"
    | gotArgsMsg argv = String.concatWith " " ("Got these args:" :: argv)

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
          ( app eprint ["Usage: ", prog, " [-c | -u | -o filename | -g filename | -d] outcomefile\n" ]
          ; eprint "Note: the default mode is partition, and so the mode is omitted from the usage above\n"
          ; eprint $ gotArgsMsg argv ^ "\n"
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
           ; eprint $ gotArgsMsg argv ^ "\n"
           ; OS.Process.failure
      ))
      handle BadOption s => (app eprint [s, "\n"] ; OS.Process.failure)

  fun splitGradesFile [] = (NONE, [])
    | splitGradesFile (Gradesfile s :: rest) = (SOME s, rest)
    | splitGradesFile (other :: rest) =
      let val (g, r') = splitGradesFile rest
      in  (g, other :: r')
      end

  fun doTree (prog, argv') =
      let val (options, argv) = options argv'
          fun treeStyle [] = (SOME StudentTree, [])
            | treeStyle (StudentTree :: rest) = (SOME StudentTree, rest)
            | treeStyle (GradeTree :: rest) = (SOME GradeTree, rest)
            | treeStyle (other :: rest) =
              let val (s, r') = treeStyle rest
              in  (s, other :: r')
              end
          val (grades, options) = splitGradesFile options
          val (treeStyle, options) = treeStyle options
          fun makeStudentTree ins =
              let val outcomes = FileReader.readToMap ins
              in TestResultDecisionTree.make { outcomes = outcomes
                                             , informationGain = InformationGain.forStudentTree
                                             }
              end
          fun makeGradeTree grades ins =
              let val outcomes = FileReader.readToMap ins
              in TestResultDecisionTree.make { outcomes = outcomes
                                             , informationGain = InformationGain.forGradeTree grades
                                             }
              end
          val success = (fn t => success $ Dot.toString t)
      in case (grades, treeStyle, argv)
          of  (SOME grades, SOME GradeTree, [outcomes]) =>
              let val grades = Util.withInputFromFile grades GradeReader.readToMap
                  val tree = Util.withInputFromFile outcomes (makeGradeTree grades)
              in  success $ TestResultDecisionTree.toDotWithGrades tree grades
              end
            | (SOME grades, SOME StudentTree, [outcomes]) =>
              let val grades = Util.withInputFromFile grades GradeReader.readToMap
                  val tree = Util.withInputFromFile outcomes makeStudentTree
              in  success $ TestResultDecisionTree.toDotWithGrades tree grades
              end
            | (NONE, SOME StudentTree, [outcomes]) =>
              let val tree = Util.withInputFromFile outcomes makeStudentTree
              in  success $ TestResultDecisionTree.toDot tree
              end
            | (NONE, NONE, [outcomes]) =>
              let val tree = Util.withInputFromFile outcomes makeStudentTree
              in  success $ TestResultDecisionTree.toDot tree
              end
            | _ =>
              ( eprint (String.concatWith " " ["Usage:", prog, "decision-tree [-g filename] [--student-tree | --grade-tree] outcomes\n"])
              ; eprint "When given '-g', '--grade-tree' is allowed; otherwise only the default '--student-tree' is allowed\n"
              ; eprint $ gotArgsMsg argv ^ "\n"
              ; OS.Process.failure
              )
      end

  fun splitReportStyle [] = (NONE, [])
    | splitReportStyle (WeightOfEvidence f :: rest) = (SOME (WeightOfEvidence f), rest)
    | splitReportStyle (TreeReport :: rest) = (SOME TreeReport, rest)
    | splitReportStyle (other :: rest) =
      let val (s, r') = splitReportStyle rest
      in  (s, other :: r')
      end

  fun doReport (prog, argv') =
      let val (options, argv) = options argv'
          val (reportStyle, options) = splitReportStyle options
          val success = success o Utln.format ""
      in case (reportStyle, argv)
           of (SOME TreeReport, [outcomes]) =>
              let val db = Util.withInputFromFile outcomes FileReader.readToMap
                  val tree = TestResultDecisionTree.make { outcomes = db
                                                         , informationGain = InformationGain.forStudentTree
                                                         }
              in  success $ DecisionTreeReport.utlnEntries $ DecisionTreeReport.make tree db
              end
            | (NONE, [outcomes]) =>
              let val db = Util.withInputFromFile outcomes FileReader.readToMap
                  val tree = TestResultDecisionTree.make { outcomes = db
                                                         , informationGain = InformationGain.forStudentTree
                                                         }
              in  success $ DecisionTreeReport.utlnEntries $ DecisionTreeReport.make tree db
              end
            | (SOME (WeightOfEvidence grades), [outcomes]) =>
              let val db = Util.withInputFromFile outcomes FileReader.readToMap
                  val grades = Util.withInputFromFile grades GradeReader.readToMap
                  val report = TestWeightOfEvidenceReport.make db grades
              in  success $ TestWeightOfEvidenceReport.utlnEntries report
              end
            | _ =>
              ( eprint (String.concatWith " " ["Usage:", prog, "report [--tree-report | --weight-of-evidence grades] outcomes\n"])
              ; eprint $ gotArgsMsg argv ^ "\n"
              ; OS.Process.failure
              )
      end

  fun run (_, argv) =
      let val (mode, argv) =
              case argv
               of ("partition" :: argv) => (doPartition, argv)
                | ("entropy" :: argv) => (doEntropy, argv)
                | ("decision-tree" :: argv) => (doTree, argv)
                | ("report" :: argv) => (doReport, argv)
                | _ => (doPartition, argv)
      in  mode ("partition", argv)
      end
end
