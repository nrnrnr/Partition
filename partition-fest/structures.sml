structure Map = StringMap(TernaryKeyChar)
structure ListMap = ListMapFn(Map)
structure CharListMap = CharListMapFn(TernaryKeyChar)
structure M = EmbedListMap(structure L1 = Map
		           structure L2 = ListMap)
structure DB = TestDB(structure M1 = CharListMap
                      structure M2 = CharListMap
                      structure M3 = CharListMap)  
structure G = BasicGraph
structure TestCollection = TestCollection(Outcome)
structure SolnCollection = SolnCollection(Outcome)

structure Grade = struct
    datatype grade = E
                   | VG
                   | G
                   | F
                   | P
                   | NC
                   | UNKNOWN of string
    fun ofString s =
      case String.map Char.toUpper s
       of "E" => E
        | "VG" => VG
        | "G" => G
        | "F" => F
        | "P" => P
        | "NC" => NC
        | _ => UNKNOWN s
    fun toString E = "E"
      | toString VG = "VG"
      | toString G = "G"
      | toString F = "F"
      | toString P = "P"
      | toString NC = "NC"
      | toString (UNKNOWN s) = s
  end

(* XXX: Not sure where to put NodeInfo. But when it wasn't in a structure
   CM complained "Warning: definition not tracked by CM" *)
structure Data = struct
    type NodeInfo = (string list * string)
    exception InvalidUtln of string
    datatype entropyOpt = SingleTest of (string * int)
                        | AllTests of (Outcome.outcome list -> bool)
                        | IndividualTests
  end
structure D = Data

structure BasicStringKey : ORD_KEY = struct
    type ord_key = string
    val compare = String.compare
  end
structure SolutionKey : ORD_KEY = BasicStringKey

structure TmarkKey : ORD_KEY = struct
    type ord_key = (string * int)
    fun compare ((tid1, tnum1), (tid2, tnum2)) =
        case String.compare (tid1, tid2)
         of EQUAL => Int.compare (tnum1, tnum2)
          | order => order
  end
structure TestResultKey : ORD_KEY = struct
    type ord_key = (string * int * Outcome.outcome)
    fun compare ((tid1, tnum1, _), (tid2, tnum2, _)) =
        case String.compare (tid1, tid2)
         of EQUAL => Int.compare (tnum1, tnum2)
          | order => order
  end
