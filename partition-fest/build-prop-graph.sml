structure BuildPropGraph = struct (* for use on the command line *)
    
    val usage = ref (fn () => ())
    fun run f (infile::outfile::failures::flags) = ignore (f infile outfile failures flags)
      | run _ _ = !usage ()

    fun run2 f (infile::outfile::flags) = ignore (f infile outfile flags)
      | run2 _ _ = !usage ()

    val commands =
        [ ( "build-graph", run Basis.buildGraph 
          , "build-graph infile outfile fail [flags]"
          )
        , ( "build-prop-graph", run2 Basis.buildPropGraph
          , "build-prop-graph infile outfile [flags]"
          )
        ]
    val _ = usage :=
        (fn () => app (fn (_, _, s) => app print ["Usage: ", s, "\n"]) commands)
    
    val arg0 = CommandLine.name ()

    val _ = case List.filter (fn (n, _, _) => String.isSuffix n arg0) commands
              of [(_, f, _)] => f (CommandLine.arguments())
               | _           => !usage ()
end
