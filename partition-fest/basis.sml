structure Basis : sig 
  val buildGraph : string -> string -> unit 
  val buildPropGraph : string -> string -> unit
end =
struct

  (* Turn map into a TestSet *)
fun makeTestSet db = 
  DB.foldLists (fn (test, testno, rList, set) => 
                TestSet.add ((test, testno, rList), set))
               TestSet.empty db

  (* Partition TesTestSet *)
  val partitionTests : TestSet.set -> TestSet.set list = TestSet.partition TestSet.eq

  (* Make map from solns -> test * outcome list using a representative from each
  eq class *)
  exception Impossible
  fun addToMap (set, map) = case TestSet.representative set
                              of SOME (name, number, ol) => foldr
                                 (fn ((soln, out), m) => 
                                   ListMap.add (explode soln, 
                                               (name, number, out),m))
                                 map ol
                               | NONE => raise Impossible


  val makeSolnMap :  
    TestSet.set list -> (string * string * Outcome.outcome) ListMap.map =
      fn set => foldr addToMap ListMap.empty set

  (* Turn map into SolnSet *)
  val makeSolnSet : 
    (string * string * Outcome.outcome) ListMap.map -> SolnSet.set = 
       fn map => ListMap.mapFold
         (fn (k, testList, set) => SolnSet.add ((implode k, testList), set))
         SolnSet.empty map

  (* Partition SolnSet *)
  val partitionSolns : SolnSet.set -> SolnSet.set list = SolnSet.partition SolnSet.eq

  (* Produce graph using subset relations *)

  val /<=/ = SolnSet./<=/
  val /==/ = SolnSet./==/
  infix 3 /<=/ /==/


  fun solnRep s = case SolnSet.representative s
                    of SOME y => y
                     | NONE   => raise Impossible

  fun testRep s = case TestSet.representative s
                    of SOME y => y
                     | NONE   => raise Impossible

  (* Make a new set list with renamed members, and a map to the students that
     the new names represent *)
  val buildMapAndSet : SolnSet.set list -> SolnSet.set list * string Map.map =
  fn sl =>
   let val (s, m, _) =
    foldr (fn (s, (set, map, c)) =>
    let val string = SolnSet.fold (fn ((n, _), str) => n^(" "^ str)) "" s
        val (_, l) = solnRep s
        val node = "N"^Int.toString(c)
    in (SolnSet.add((node, l), set), 
        Map.bind(explode node, string, map), c+1) end) 
    (SolnSet.empty, Map.empty, 1) sl
   in (SolnSet.partition SolnSet.eq s,m) end

  fun edge id1 label id2 = G.makeEdge (G.makeNode id1, label, G.makeNode id2)

  (* Make the graph structure *)
  val makeGraph : SolnSet.set list -> BasicGraph.graph =
  fn sl => 
    foldr (fn (x, graph) => 
     let val (id1, _) = solnRep x
     in foldr (fn (y, g) =>
      let val (id2, _) = solnRep y
      in if x /<=/ y andalso not (y /<=/ x) 
         then G.addEdge (edge id2 "" id1, g)
         else g
      end)
         (G.addNode(G.makeNode id1, graph)) sl
     end)
    G.empty sl

  infixr 0 $
  fun f $ x = f x

  val partitionProps = Prop.partition Prop.eq

  fun buildPropGraph infile outfile =
    let val (s, m) = Prop.makePropMapAndSet $ Prop.removeDuals $ 
                     partitionProps $ Prop.makePropList $
                     partitionTests $ makeTestSet $
                     FileReader.readToMap $ TextIO.openIn infile
        val g      = Prop.makePropGraph s
        val fd     = TextIO.openOut outfile
        val ()     = FileWriter.printGraph g m fd false
        val ()     = TextIO.closeOut fd
    in  ()
    end


  fun buildGraph infile outfile = 
    let val (s, m) = buildMapAndSet $ partitionSolns $ makeSolnSet $
                     makeSolnMap $ partitionTests $ makeTestSet $
                     FileReader.readToMap $ TextIO.openIn infile
        val g = makeGraph s
    in FileWriter.printGraph g m (TextIO.openOut outfile) true
    end

end
