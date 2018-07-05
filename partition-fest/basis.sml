structure Basis : sig
  val buildGraph : string -> string -> string -> string option -> TextIO.outstream option ->
                   string list -> SolnCollection.collection 
  val buildPropGraph : string -> string -> 
                       string list -> Prop.prop list list
  val readGrades : string -> Grade.grade Map.map
  val gradeNodeColors : D.NodeInfo Map.map -> Grade.grade Map.map -> string list Map.map
  val renderEntropy : D.entropyOpt -> string -> string
end =
struct

  (* helper function because Haskell syntax is useful *)
  infixr 0 $
  fun f $ x = f x


  (* Turn map into a TestCollection *)
  fun makeTestCollection db = 
    DB.foldStudents (fn (test, testno, rList, set) => 
                     TestCollection.add ((test, testno, rList), set))
                     TestCollection.empty db

  (* Partition TesTestCollection *)
  val partitionTests : TestCollection.collection -> TestCollection.collection list = TestCollection.partition TestCollection.eq

  (* Make map from solns -> test * outcome list using a representative from each
  eq class *)
  exception Impossible
  fun addToMap (set, map) = case TestCollection.representative set
                              of SOME (name, number, ol) => foldr
                                 (fn ((soln, out), m) => 
                                   ListMap.add ( soln, 
                                               ((name, number), out),m))
                                 map ol
                               | NONE => raise Impossible


  val makeSolnMap :  
    TestCollection.collection list -> ((string * string) * Outcome.outcome) ListMap.map =
      fn set => foldr addToMap ListMap.empty set


  (* Make test vector from map, with DNRs represented in the vector *)
  val makeSolnCollection : 
    ((string * string) * Outcome.outcome) ListMap.map -> SolnCollection.collection = 
       fn map => ListMap.mapFold
         (fn (k, testList, set) => SolnCollection.add ((k, testList), set))
         SolnCollection.empty map

  (* Partition SolnCollection *)
  val partitionSolns : SolnCollection.collection -> SolnCollection.collection list = SolnCollection.partition SolnCollection.eq

  (* Produce graph using subset relations *)

  val /</ = SolnCollection./</
  infix 3 /</

  fun solnRep s = case SolnCollection.representative s
                    of SOME y => y
                     | NONE   => raise Impossible

  fun testRep s = case TestCollection.representative s
                    of SOME y => y
                     | NONE   => raise Impossible
  (* Make a new set list with renamed members, and a map to the students that
     the new names represent *)
  val buildMapAndSet : SolnCollection.collection list -> SolnCollection.collection list * D.NodeInfo Map.map =
  fn sl =>
   let val (s, m, _) =
    foldr (fn (s, (set, map, c)) =>
      let val ids = SolnCollection.fold (fn ((id, _), ids) => id :: ids) [] s
          fun withIndex (id, (lastIndex, ids)) = (lastIndex+1, (lastIndex+1, id) :: ids)
          val (_, indexed) = foldl withIndex (~1, []) ids
          val string = case rev indexed
                         of [] => ""
                          | ((_, id0) :: ids) =>
                            let fun join ((index, id), rest) =
                                  if index mod 5 = 0 then
                                      id ^ ",\\n" ^ rest
                                  else
                                      id ^ ", " ^ rest
                            in  foldr join id0 ids
                            end
          val (_, l) = solnRep s
          val nodeName = "N"^Int.toString(c)
          val info = (ids, string)
      in (SolnCollection.add((nodeName, l), set),
          Map.bind(nodeName, info, map), c+1)
      end) 
    (SolnCollection.empty, Map.empty, 1) sl
   in (partitionSolns s,m) 
   end

  (* curried, simplified function to make an edge *)
  fun edge id1 label id2 = G.makeEdge (G.makeNode id1, label, G.makeNode id2)

  (* Make the implication graph *)
  fun id (solnid, _) = solnid
  val makeGraph : SolnCollection.collection list -> BasicGraph.graph =
  fn sl => 
    foldr (fn (x, graph) => 
           let val x = solnRep x
           in  foldr (fn (y, g) =>
                      let val y = solnRep y
                      in  if x /</ y andalso not (y /</ x) 
                          then G.addEdge (edge (id y) "" (id x), g)
                          else g
                      end)
               (G.addNode(G.makeNode (id x), graph)) sl
           end)
    G.empty sl



  val partitionProps = Prop.partition Prop.eq
 
  (* Transforms a map from label -> prop list into a map from label -> string *)
  fun condenseMap map = 
    Map.mapFold (fn (key, pList, m2) => Map.bind (key, Prop.toString pList, m2))
      Map.empty map

  (* Takes a map from strings to prop lists and returns a map from
     strings to prop lists with any tautological equivalences removed *)
  fun removeIntraNodeTautologies m = 
    Map.mapFold (fn (key, pList, m2) => 
                  Map.bind (key, Prop.removeIntraNodeTautologies pList, m2))
      Map.empty m

  fun removeTautologies (g, m) =
    let val edges = BasicGraph.getEdges g
        val edges2 = 
          foldr (fn (e, es) => 
            if Prop.tautology (Map.lookup ( BasicGraph.getIn e, m),
                               Map.lookup ( BasicGraph.getOut e, m))
            then es
            else e::es)
            [] edges
    in (BasicGraph.getGraphFromEdges edges2, m)
    end
       
  fun getFalseReps pl =  Prop.getPropsWithResult pl 
                          (true, Outcome.NOTPASSED{outcome="errored",
                                                   witness=""})

  fun containsFalse pl = not $ null $ getFalseReps pl
                        

  fun propRep [] = raise Impossible
    | propRep (x::xs) = x

  val testRep = propRep

  fun look m node = Map.lookup (BasicGraph.getNodeLabel node, m)

  fun getImplyingFailReps (n, g, m) =
    let val implyingNodes = BasicGraph.getPredecessorNodes (n, g)
        val implyingProps = map (look m) implyingNodes
        val falseImplyingProps = List.filter containsFalse implyingProps
    in map propRep falseImplyingProps
    end

  fun getImplyingFailures (n, g, m) = 
    let val implyingNodes = BasicGraph.getPredecessorNodes (n, g)
        val implyingProps = map (look m) implyingNodes
    in Util.flatten $ map getFalseReps implyingProps
    end

  fun makeFailureMap (g, m) = 
    let val nodes = BasicGraph.getNodes g
        val aList = map (fn n => (look m n, n)) nodes
        val failList = map (fn (props, n) => (getFalseReps props, n)) aList
    in foldr (fn ((tests, node), map) =>
               foldr (fn (test, map) =>
                 let val (test, num) = Prop.getId test
                     val id = test ^ num
                 in Map.bind (id, node, map) end) map tests) 
       Map.empty failList
    end


(*  COMMENTED CODE IS OPTIONAL CODE FOR DUMPING CAUSE OF EACH WITNESS
    REDUCTION TO A FILE NAMED "causes" *)

  fun reduceWitness failMap nodeMap g (solnid, ol) = 
    let (*val causes = TextIO.openOut "causes"
        val _      = TextIO.output (causes, solnid^": \n")
*)
        val failures = List.filter (fn ((_,_), Outcome.NOTPASSED {...}) => true
                                     | _                            => false)
                       ol
        fun redundantNode ((test,num), 
                           Outcome.NOTPASSED {witness = witness, ...}) = 
            let val id = test ^ num
              val node  = Map.lookup (id, failMap)
                          handle NotFound => BasicGraph.makeNode "FAKENODE"
              val impls = map Prop.getId $ getImplyingFailures (node,g,nodeMap)
              val cause = List.find 
                            (fn (id2, _) => List.exists (fn id1 => id1 = id2)
                                            impls) failures
            in case cause 
                of NONE => false
                 | SOME ((t,n), Outcome.NOTPASSED {witness = witness2, ...}) => 
			true


(*                (let val id2 = t^n
                 in TextIO.output (TextIO.openOut "causes",
                                   "Test "^id^" made redundant by "^id2(*^
                                   "\n"^id2^"'s witness:\n"^witness2^
                                   "\n"^id^"'s witness:\n"^witness*)^"\n\n")
		end
                                         ; true) *)


                 | _ => raise Impossible
            end
          | redundantNode _ = false
     in (solnid, List.filter (not o redundantNode) failures)
     end

  fun reduceWitnesses (g,m) solns = 
    let val failMap = makeFailureMap (g, m)
    in map (reduceWitness failMap m g) solns
    end


  exception NoImplyingFailures
  (* Produces the union of the failure sets of all implying proposition lists
     that have at last one failure in them, i.e. the union of all implying
     failure sets *)
  val getFalseUnion : BasicGraph.node * BasicGraph.graph * 
                      Prop.prop list Map.map -> Prop.prop = 
  (fn (n, g, m) =>
    let val reps = getImplyingFailReps (n, g, m)
    in if null reps
       then raise NoImplyingFailures
       else Prop.unionstar reps
    end)

  fun failureRedundantUnderClaessen (n, g, m) =
    let val self = Map.lookup (BasicGraph.getNodeLabel n, m)
        val impls = getImplyingFailures (n, g, m)
    in List.exists (fn n1 => List.exists
                               (fn n2 => Prop.eq (propRep self, 
                                                  Prop.union (n1, n2)))
                             impls) impls
    end

  (* Returns whether or not a given node's set of true values is equivalent to
     the union of all implying failure sets.
     PRECONDITION: n maps to a prop list that contains at least one failure *)
  fun failureRedundantUnderUnion (n, g, m) = 
    let val self = Map.lookup (BasicGraph.getNodeLabel n, m)
    in Prop.eq (propRep self, getFalseUnion (n, g, m))
       handle _ => false
    end
  

  (* Returns a proposition list where each proposition is the failure of a test
     found redundant under union using the graph g and map m *)
  fun findRedundantFailures (g, m) redundantF =
    let val nodes = BasicGraph.getNodes g
        fun addIfRedundant (n, xs) = 
          let val pl = Map.lookup (BasicGraph.getNodeLabel n, m)
          in if not $ containsFalse pl then xs
             else if redundantF (n,g,m)
                  then getFalseReps pl @ xs
                  else xs
          end
    in foldr addIfRedundant [] nodes
    end

  (* change this to filter *)

  fun removeRedundantTests (tl, g, m) redundantF = 
    let val toBeRemoved = map Prop.getId (findRedundantFailures (g, m) 
                                                                 redundantF)
        fun redundant t = List.exists (fn n => n = TestCollection.getId (testRep t)) 
                                       toBeRemoved
    in List.filter (not o redundant) tl
    end

  fun getTestPartitions infile = partitionTests $ makeTestCollection $
                                 FileReader.readToMap $ TextIO.openIn infile

  fun buildPropGraphAndMap tests = 
    let val p      = partitionProps $ Prop.makePropList tests 
        val (s, m) = Prop.makePropMapAndSet p
                  
    in (Prop.makePropGraph s, m, p)
    end

  fun unionReduction method (g, m, p) tests = 
    removeRedundantTests (tests, g, m) method


  exception InvalidFlag of string
  fun reduction [] = (fn x => x)
    | reduction (flag::xs) = 
        case flag of "-t" => removeTautologies o reduction xs
                   | x    => raise InvalidFlag x

  fun mapReduction [] = condenseMap
    | mapReduction (flag::xs) =
        case flag of "-t" => mapReduction xs o removeIntraNodeTautologies
                   | _    => mapReduction xs

  fun TestCollectionReduction [] _ = (fn x => x)
    | TestCollectionReduction (flag::xs) (g, m, p) = 
        case flag of "-u" => (unionReduction failureRedundantUnderUnion 
                                             (g,m,p))
                              o TestCollectionReduction xs (g,m,p)
                   | "-c" => (unionReduction failureRedundantUnderClaessen
                                             (g,m,p)) 
                              o TestCollectionReduction xs (g,m,p)
                   | _    => TestCollectionReduction xs (g, m, p)

  fun solnReduction [] _ = (fn x => x)
    | solnReduction (flag::xs) (g, m) = 
        case flag of "-w" => (reduceWitnesses (g,m)) o solnReduction xs (g, m)
                   | _    => solnReduction xs (g, m)


  (* XXX: need to remove anonymization code since we now have an
     extenral tool for it *)
  fun anonymize solns = 
    let fun stripResults (soln, _) = soln
        val names = map stripResults solns
        fun anon x = x
    in map (fn (soln, results) => (anon soln, results)) solns
    end

  fun reduceTests tests = 
    let fun getId (x, _) = x

        val solns = map getId $ map solnRep $ partitionSolns $ 
                    anonymize $ makeSolnCollection $ makeSolnMap tests

        val tests = map testRep tests

        val limit = List.filter (not o (fn (x,out) => 
                                         List.exists (fn y => x = y) solns))
                    
    in map (fn (x,y,ol) => (x,y,limit ol)) tests
    end 

  fun buildPropGraph infile outfile  flags =
    let val tests   = getTestPartitions infile
        val (g,  m, p) = buildPropGraphAndMap $ map testRep tests
        val (g', m') = reduction flags (g, m)
        val fixMap  = mapReduction flags
        val fd      = TextIO.openOut outfile
        val ()      = FileWriter.printGraph g' (fixMap m') fd false
        val ()      = TextIO.closeOut fd
    in  p
    end

  fun readGrades f =
    let val fd = TextIO.openIn f
        val grades = GradeReader.readToMap fd
        val () = TextIO.closeIn fd
    in  grades
    end

  fun eprint s = TextIO.output (TextIO.stdErr, s ^ "\n")
  structure Gr = Grade
  fun gradeNodeColors m gradesMap =
      let fun gradeFor id0 g0 ids =
          let fun loop [] = g0
                | loop (id::ids) =
                  let val g = Map.lookup (id, gradesMap)
                  in  ( if g0 <> g then
                            eprint (String.concatWith
                                        " "
                                        [ "Solution", id0, "had grade"
                                        , Gr.toString g0
                                        , "yet", id, "had grade"
                                        , Gr.toString g
                                        ])
                        else ()
                      ; loop ids
                      )
                  end
          in  loop ids
          end

        fun colorFor Gr.E = "//gray"
          | colorFor Gr.VG = "/orrd9/7"
          | colorFor Gr.G = "/orrd9/5"
          | colorFor Gr.F = "/orrd9/3"
          | colorFor Gr.P = "/orrd9/1"
          | colorFor Gr.NC = "//white"
          | colorFor (Gr.UNKNOWN _) = "//yellow2"

        fun colorNode (_, ([], _), _) = raise Impossible (* each label is mapped to n > 0 solution ids *)
          | colorNode (label, (id0::ids, _), colors) =
            let val g0 = Map.lookup (id0, gradesMap)
                val color = colorFor (gradeFor id0 g0 ids)
                val attr = "[style=filled,fillcolor=\"" ^ color ^ "\"]"
            in  Map.bind (label, [attr], colors)
            end
    in  Map.mapFold colorNode Map.empty m
    end

  structure NodeMap = BinaryMapFn(type ord_key = G.node
                                  val compare = String.compare)
  fun renderDistribution g gradesMap =
      let val root = getRoot g
          fun getS n = G.getSuccessorNodes (n, g)
          val distances = addDistances getS root 0 (NodeMap.insert (NodeMap.empty, root, 0))
          fun renderNodeDistance n = String.concat [G.getNodeLabel n, "->", Int.toString $ valOf $ NodeMap.find (distances, n)]
      in  String.concatWith "\n" (map renderNodeDistance (G.getNodes g))
      end
  and addDistances getS from curDist distances =
      let val tos = getS from
          val distances' = List.foldl (addDistance curDist) distances tos
          fun toDistances (to, ds) = addDistances getS to (curDist + 1) ds
      in  List.foldl toDistances distances' tos
      end
  and addDistance dist (n, ds) =
      let val d = getOpt (NodeMap.find (ds, n), dist)
      in  if dist < d
          then NodeMap.insert (ds, n, dist)
          else ds
      end
  and getRoot g =
      let val nodes = G.getNodes g
          fun maximal n = null (G.getSuccessorNodes (n, g))
      in  case List.filter maximal nodes
           of [r] => r
            | _ => raise Match
      end

  fun buildGraph infile outfile outfileFailures gradeFile distributionOut flags =
    let val tests     = getTestPartitions infile
	val (g ,m ,p) = buildPropGraphAndMap $ map testRep tests 
	val tests'    = TestCollectionReduction flags (g, m, p) tests

        val solns      = anonymize $ makeSolnCollection $ makeSolnMap tests' 
        val (s, m')    = buildMapAndSet $ partitionSolns solns
        val g'         = makeGraph s
        val (fd, ffd)  = (TextIO.openOut outfile, 
                          TextIO.openOut outfileFailures)
        val nodeColors = case gradeFile
                           of NONE => Map.empty
                            | SOME f => gradeNodeColors m' (readGrades f)
        val ()         = FileWriter.printSolnGraph g' m' s fd nodeColors
        val ()         = case (gradeFile, distributionOut)
                          of (SOME f, SOME out) =>
                             let val gradesMap = readGrades f
                             in  TextIO.output (out, renderDistribution g' gradesMap)
                             end
                           | _ => ()
        val solns      = solnReduction flags (g', m) solns 
        val ()         = FileWriter.printStudentFailures solns ffd
        val _          = (TextIO.closeOut fd;
                          TextIO.closeOut ffd)
    in solns
    end

  structure StudentMap = BinaryMapFn(type ord_key = string
                                     val compare = String.compare)
  structure Outcomes = BinarySetFn(type ord_key = (string * int * Outcome.outcome)
                                     fun compare ((tid1, tnum1, _), (tid2, tnum2, _)) =
                                         case String.compare (tid1, tid2)
                                          of EQUAL => Int.compare (tnum1, tnum2)
                                           | order => order)
  fun getAllTests outcomes =
      let fun addTest (tid, tnum, s, outcome, tests) =
              let val tnum = valOf $ Int.fromString tnum
                  val sTests = getOpt (StudentMap.find (tests, s), Outcomes.empty)
                  val sTests = Outcomes.add (sTests, (tid, tnum, outcome))
              in  StudentMap.insert (tests, s, sTests)
              end
          val outcomesByStudent = DB.fold addTest StudentMap.empty outcomes
          fun third (_, _, x) = x
      in  map (fn (sid, outcomes) => (map third $ Outcomes.listItems outcomes, sid))
              $ StudentMap.listItemsi outcomesByStudent
      end
  fun getOneTest tid tnum outcomes =
      let val tnum = Int.toString tnum
          fun addTest (tid', tnum', student, outcome, testsSoFar) =
              if tid' = tid andalso tnum' = tnum
              then (outcome, student) :: testsSoFar
              else testsSoFar
      in  DB.fold addTest [] outcomes
      end

  fun withInputFromFile path f =
      let val is = TextIO.openIn path
      in  f is before TextIO.closeIn is
      end

  fun entropyOf tests =
      let val histogram = Entropy.histogram tests
          val () = if null (Entropy.H.nonzeroKeys histogram)
                   then eprint "Warning: no tests found"
                   else ()
      in  Entropy.entropy histogram
      end

  fun renderEntropy whichTest outcomesPath =
      let val outcomesByTest = withInputFromFile outcomesPath FileReader.readToMap
          val entropy = case whichTest
                         of D.AllTests => entropyOf $ getAllTests outcomesByTest
                          | D.SingleTest (tid, tnum) => entropyOf $ getOneTest tid tnum outcomesByTest
      in  Real.toString entropy
      end
end
