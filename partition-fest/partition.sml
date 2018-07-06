structure Partition = struct

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
            | eat (options', "--all" :: argv) = eat (D.AllTests (fn x => true) :: options', argv)
            | eat (options', "--all-imperfect" :: argv) = (D.AllTests (not o perfect) :: options', argv)
            | eat (options', argv) = (options', argv)
          val (options', argv) = eat ([], argv)
      in  (rev options', argv)
      end

  fun success s = ( TextIO.output (TextIO.stdOut, s)
                  ; OS.Process.success
                  )

  fun doEntropy (prog, argv) =
      (case entropyOptions argv
        of ([whichTest], [outcomes]) => success (Basis.renderEntropy whichTest outcomes ^ "\n")
         | ([], [outcomes]) => success (Basis.renderEntropy (D.AllTests (fn x => true)) outcomes ^ "\n")
         | (options, argv) =>
           ( app eprint ["Usage: ", prog, " entropy [--single tid tnum | --all | --all-imperfect] outcomes\n"]
           ; eprint "Got these args:" ; app (fn s => app eprint [" ", s]) argv
           ; eprint "\n"
           ; OS.Process.failure
      ))
      handle BadOption s => (app eprint [s, "\n"] ; OS.Process.failure)

  fun run (prog, argv) =
      let val (mode, argv) =
              case argv
               of ("partition" :: argv) => (doPartition, argv)
                | ("entropy" :: argv) => (doEntropy, argv)
                | _ => (doPartition, argv)
      in  mode (prog, argv)
      end
end
