structure Dot :> sig
              type node
              type edge
              type graph = (node list * edge list)

              val node : {name : string, label : string} -> node
              val edge : {from : string, to : string} -> edge
              val edgeWithAttrs : edge -> (string * string) list -> edge
              val nodeWithAttrs : node -> (string * string) list -> node
              val toString : graph -> string
              val appendGraphs : (graph * graph) -> graph
          end
= struct

  infixr 0 $
  fun f $ x = f x

  datatype node' = Node of {name : string, label : string}
  datatype edge' = Edge of {from : string, to : string}

  (* maybe withAttrs doesn't need the option---I'm not sure if we need
     to distinguish between the empty list of attributes and no
     attributes
   *)
  type 'a withAttrs = ('a * (string * string) list option)
  type node = node' withAttrs
  type edge = edge' withAttrs

  type graph = (node list * edge list)

  fun node n = (Node n, NONE)
  fun edge e = (Edge e, NONE)
  fun withAttrs (v, NONE) attrs = (v, SOME attrs)
    | withAttrs (v, SOME attrs0) attrs = (v, SOME $ foldr op :: attrs0 attrs)
  val (edgeWithAttrs, nodeWithAttrs) = (withAttrs, withAttrs)
  fun attrsString (SOME (attrs as _::_)) =
      let val attrs = map (fn (key, value) => key ^ "=\"" ^ value ^ "\"") attrs
      in  String.concatWith "," attrs
      end
    | attrsString _ = ""

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
  fun nodeString (Node {name, label}, attrs) =
      let val prefix = name ^ " [label=\"" ^ label ^ "\"]"
      in  prefix ^ "[" ^ attrsString attrs ^ "]"
      end
  fun edgeString (Edge {from, to}, attrs) =
      let val prefix = from ^ " -> " ^ to
      in prefix ^ "[" ^ attrsString attrs ^ "]"
      end
  fun toString (nodes, edges) =
      let val nodeCount = length nodes
          val (width, height) = if nodeCount < 7
                                then (3.0, 3.0)
                                else if nodeCount < 10
                                then (3.5, 3.5)
                                else if nodeCount < 15
                                then (5.0, 4.0)
                                else if nodeCount < 20
                                then (6.0, 5.0)
                                else if nodeCount < 25
                                then (7.0, 6.0)
                                else if nodeCount < 30
                                then (8.0, 7.0)
                                else (10.0, 8.0)
          val fmt = Real.fmt (StringCvt.FIX $ SOME 3)
          val size = fmt width ^ "," ^ fmt height
      in String.concatWith "\n" $
                           [ "digraph testgraph { fontsize=\"9\""
                           , "size=\"" ^ size ^ "\"; ratio=auto"
                           , "node [fontsize=\"9\"]"
                           , "edge [fontsize=\"9\"]"
                           ]
                           @ map nodeString nodes
                           @ map edgeString edges
                           @ ["}"]
      end
  fun appendGraphs ((ns0, es0), (ns, es)) = (foldr op :: ns ns0, foldr op :: es es0)
  end
