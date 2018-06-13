structure FileWriter : sig
  val printGraph : 
    BasicGraph.graph -> string Map.map -> TextIO.outstream -> bool -> unit

  (* the bool represents whether the arrow should point backwards, true if it
     should, false if it should not *)

  val printSolnGraph :
      BasicGraph.graph ->
      D.NodeInfo Map.map ->
      SolnCollection.collection list ->
      TextIO.outstream ->
      string list Map.map -> (* Additional attributes (e.g. color) for a
                                particular node, which get inserted just
                                after the corresponding node is created.
                             *)
      unit

  val printStudentFailures :
    SolnCollection.collection -> TextIO.outstream -> unit

end =
struct
  val output = TextIO.output

  fun printGraph graph map out back =
      (output (out, "digraph testgraph { fontsize=\"9\" \nsize=\"10.3,7.7\"; ratio=compress\nnode [fontsize=\"9\"] \nedge [fontsize=\"9\"]\ncolorscheme=\"brewer\"\n"); 
       foldl (fn (name, _) =>
               let val label = G.getNodeLabel name
               in ((output (out, label ^ " [label=\"" ^
                                Map.lookup(label, map) ^ "\"]\n")); [])
         end)
       [] (G.getNodes graph);
       foldl (fn (name, _) =>
               foldr (fn (name2, _) => 
                     if back then
                         ((output (out, G.getNodeLabel name ^ " -> " ^
                                        G.getNodeLabel name2 ^ 
                                        " [dir=back]\n")); [])
                             else 
                          ((output (out, G.getNodeLabel name ^ " -> " ^
                                        G.getNodeLabel name2 ^ 
                                        " \n")); []))
                  [] (G.getSuccessorNodes (name, graph)))
             [] (G.getNodes graph);
       (output (out, "}")))

  fun printSolnGraph graph map solns out nodeAttrs =
    let fun output' s = output (out, s)
    in  ( output' "digraph testgraph { fontsize=\"9\" \nsize=\"10.3,7.7\"; ratio=compress\nnode [fontsize=\"9\"] \nedge [fontsize=\"9\"]\n"
        ; printSolutions output' map nodeAttrs solns
        ; printEdges output' graph
        ; output' "}"
        )
    end

  and printSolutions output map nodeAttrs solns =
      app (fn soln =>
              app (fn (label, results) =>
                      let val (_, names) = Map.lookup (label, map)
                          fun formatAttr a = label ^ " " ^ a ^ "\n"
                          val attrs = List.map formatAttr (Map.lookup (label, nodeAttrs))
                                      handle Map.NotFound _ => []
                      in output (renderSolution label results names attrs)
                      end)
                  soln) solns

  and renderSolution nodeId outcomeSet names extraAttrs =
      String.concat [ nodeId
                    , " [label=\"" , names , "\\n"
                    , renderOutcomeSet outcomeSet
                    , "\"]\n"
                    , String.concat extraAttrs
                    ]

  and renderOutcomeSet os =
      let fun renderOutcome Outcome.PASSED = "|"
            | renderOutcome (Outcome.NOTPASSED _) = "."
            | renderOutcome Outcome.DNR = "/"
      in  String.concat (map renderOutcome (map (fn (_, outcome) => outcome) os))
      end

  and renderEdge e =
      let val src = G.getIn e
          val label = G.getEdgeLabel e
          val dest = G.getOut e
      in  String.concat [ src, " -> ", dest
                        , "[dir=back,label=\""
                        , label, "\"]\n"
                        ]
      end

  and printEdges output graph =
      app (fn name =>
              app (fn edge => output (renderEdge edge))
                  (G.getSuccessorEdges (name, graph)))
          (G.getNodes graph)

  fun idToString (x,y) = x ^ " " ^ y

  fun printStudentFailures solns out = 
   foldl (fn ((student, results), _) =>
           ((output (out, student ^ " failed tests:\n"));
            (output (out, 
            foldr (fn ((id, outcome), failures) =>
             (case outcome 
               of Outcome.NOTPASSED {witness = witness, outcome = outcome} =>
                   "   " ^ idToString id ^ " : " ^ witness ^ "\n" ^ failures
                | _  => failures))
            "" results)); (output (out, "\n\n"))))
  () solns



(*
  fun printStudentFailures solns out = 
   foldl (fn ((student, results), _) =>
           (
            (output (out, 
            foldr (fn ((id, outcome), failures) =>
             (case outcome 
               of Outcome.NOTPASSED {witness = witness, outcome = outcome} =>
                   "   " ^ idToString id ^ (*" : " ^ witness ^*) "\n" ^ failures
                | _  => failures))
            "" results))))
  () solns
*)
             
(*
  fun printReducedFailures solns (g,m,p) out = 
    foldr (fn ((student, results), _) =>
            ((output (out, student ^ " relevantly failed:\n"));
             (output (out, 
*)
end
