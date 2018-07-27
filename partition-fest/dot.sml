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

  exception TODO
  fun toString (nodes, edges) =
      let fun nodeString (Node {name, label}) = raise TODO
          fun edgeString (Edge {from, to}) = raise TODO
      in  String.concatWith "\n" $
                            [ "digraph testgraph { fontsize=\"9\""
                            , "size=\"10.3,7.7\"; ratio=compress"
                            , "node [fontsize=\"9\"]"
                            , "edge [fontsize=\"9\"]"
                            ]
                            @ map nodeString nodes
                            @ map edgeString edges
      end
  end
