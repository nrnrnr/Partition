structure Dot :> sig
              type node
              type edge
              type graph = (node list * edge list)

              val node : {name : string, label : string} -> node
              val edge : {from : string, to : string} -> edge
              val toString : graph -> string
          end
= struct

  infixr 0 $
  fun f $ x = f x

  datatype node = Node of {name : string, label : string}
  datatype edge = Edge of {from: string, to : string}

  type graph = (node list * edge list)

  val node = Node
  val edge = Edge

  (* And what should we do about escaping the SML strings---is
     String.toString sufficient for dot? We should also check
     that e.g. all nodes have distinct names.
   *)
  fun nodeString (Node {name, label}) = name ^ " [label=\"" ^ label ^ "\"]"
  fun edgeString (Edge {from, to}) = from ^ " -> " ^ to
  fun toString (nodes, edges) =
      String.concatWith "\n" $
                        [ "digraph testgraph { fontsize=\"9\""
                        , "size=\"10.3,7.7\"; ratio=compress"
                        , "node [fontsize=\"9\"]"
                        , "edge [fontsize=\"9\"]"
                        ]
                        @ map nodeString nodes
                        @ map edgeString edges
                        @ ["}"]
  end
