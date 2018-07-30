structure Dot :> sig
              type node
              type edge
              type graph = (node list * edge list)

              val node : {name : string, label : string} -> node
              val edge : {from : string, to : string} -> edge
              val edgeWithAttrs : edge -> (string * string) list -> edge
              val toString : graph -> string
              val appendGraphs : (graph * graph) -> graph
          end
= struct

  infixr 0 $
  fun f $ x = f x

  datatype node = Node of {name : string, label : string}
  datatype edge = Edge of {from : string, to : string, attrs : (string * string) list option}

  type graph = (node list * edge list)

  val node = Node
  fun edge {from, to} = Edge {from = from, to = to, attrs = NONE}
  fun edgeWithAttrs (Edge {from, to, attrs = NONE}) attrs =
      Edge {from = from, to = to, attrs = SOME attrs}
    | edgeWithAttrs (Edge {from, to, attrs = SOME attrs0}) attrs =
      Edge {from = from, to = to, attrs = SOME $ foldr op :: attrs0 attrs}

  (* And what should we do about escaping the SML strings---is
     String.toString sufficient for dot? Maybe we need a datatype to
     represent dot values, such as strings or numbers or colors or
     names, since there otherwise isn't nice way to mix e.g. numeric-
     and string-valued attributes. Clients then don't have to mush
     things into a string datatype, and we can do a little more to
     ensure things get escaped right.

     Slightly related: we should also check that e.g. all nodes have
     distinct names.
   *)
  fun nodeString (Node {name, label}) = name ^ " [label=\"" ^ label ^ "\"]"
  fun edgeString (Edge {from, to, attrs = SOME (attrs as _::_)}) =
      let val prefix = edgeString (Edge {from = from, to = to, attrs = NONE})
          val attrs = map (fn (key, value) => key ^ "=\"" ^ value ^ "\"") attrs
      in prefix ^ "[" ^ String.concatWith "," attrs ^ "]"
      end
    | edgeString (Edge {from, to, attrs = _}) = from ^ " -> " ^ to
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
  fun appendGraphs ((ns0, es0), (ns, es)) = (foldr op :: ns ns0, foldr op :: es es0)
  end
