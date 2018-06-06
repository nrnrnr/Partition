structure CommandLine = struct

  fun eprint s = TextIO.output(TextIO.stdErr, s)
      

  datatype opt
      = RankClaessen
      | RankUnion
      | WitnessRed
      | Outfile of string
      | Gradesfile of string

  fun options argv =
    let fun eat (options', "-c" :: argv) = eat (RankClaessen :: options', argv)
          | eat (options', "-u" :: argv) = eat (RankUnion    :: options', argv)
          | eat (options', "-o" :: filename :: argv) =
              eat (Outfile filename :: options', argv)
          | eat (options', "-g" :: filename :: argv) =
              eat (Gradesfile filename :: options', argv)
          | eat (options', "-w" :: argv) = eat (WitnessRed :: options', argv)
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

  fun fail s = ( eprint s
               ; OS.Process.failure
               )

  fun run (prog, argv) =
    (case options argv
      of (options, [outcomes]) =>
          ( Basis.buildGraph outcomes (outfile options)
                                      (witnessfile options)
                                      (gradesfile options)
                                      []
          ; OS.Process.success
          )
       | (options, argv) =>
            ( app eprint ["Usage: ", prog, " [-c | -u | -o filename | -g filename] outcomefile\n" ]
            ; eprint "Got these args:" ; app (fn s => app eprint [" ", s]) argv
            ; eprint "\n"
            ; OS.Process.failure
            ))
    handle (D.InvalidUtln s) => fail ("Invalid UTLN: " ^ s ^ "\n")
         | e => fail ("Unhandled exception: " ^ exnMessage e ^ "\n")
end
