signature UTILITIES = sig
  val insertion_sort : ('a * 'a -> order) -> 'a list -> 'a list
  val vcompare :
      (* given cmp, an ordering on 'a; xs ys, two 'a lists of equal length
         returns SOME o where (considering the cases top to bottom)
           o = EQUAL if all pairwise (x, y) x = y
               LESS if all pairwise (x, y) x <= y
               GREATER if all pairwise (x, y) x >= y
         otherwise, returns NONE *)
      ('a * 'a -> order) -> 'a list * 'a list -> order option
  val vcomparePartial : ('a * 'a -> order option) -> 'a list * 'a list -> order option
  val flatten  : 'a list list -> 'a list
  val dropWhile : ('a -> bool) -> 'a list -> 'a list
  val member : ''a -> ''a list -> bool
  val allDistinct : ''a list -> bool
  val allSame : ''a list -> bool
  val withInputFromFile : string -> (TextIO.instream -> 'b) -> 'b
  val renderSolutionIdsNarrow : string list -> string
  val unique : ''a list -> ''a list
  val fmtReal : real -> string
  val fmtReal' : int -> real -> string
end

structure Util : UTILITIES = struct
infixr 0 $
fun f $ x = f x

fun insertion_sort _ [] = []
 | insertion_sort cmp (x::xs) = insert cmp x (insertion_sort cmp xs)
and insert _ x [] = [x]
 | insert cmp x (l as y::ys) =
      case cmp (x, y) of GREATER => y :: insert cmp x ys
                       | _       => x :: l


fun eq x y = x = y
fun vcompare cmp ([], []) = SOME EQUAL
  | vcompare cmp (x::xs, y::ys) =
      (case cmp (x, y)
         of EQUAL   => vcompare cmp (xs, ys)
          | LESS    => if ListPair.exists (eq GREATER o cmp) (xs, ys)
                       then NONE else SOME LESS
          | GREATER => if ListPair.exists (eq LESS o cmp) (xs, ys)
                       then NONE else SOME GREATER)
  | vcompare _ _ = raise ListPair.UnequalLengths

fun vcomparePartial cmp ([], []) = SOME EQUAL
  | vcomparePartial cmp (x::xs, y::ys) =
      let fun le (x, y) = case cmp (x, y) of SOME EQUAL => true
                                           | SOME LESS => true
                                           | SOME GREATER => false
                                           | NONE => false
          fun ge (x, y) = le (y, x)
      in  case cmp (x, y)
            of NONE => NONE
             | SOME EQUAL => vcomparePartial cmp (xs, ys)
             | SOME LESS  => if ListPair.allEq le (xs, ys) then SOME LESS else NONE
             | SOME GREATER => if ListPair.allEq ge (xs, ys) then SOME GREATER else NONE
      end
  | vcomparePartial _ _ = raise ListPair.UnequalLengths

fun flatten [] = []
  | flatten [x] = x
  | flatten (x::xs) = flatten [x] @ flatten xs

fun dropWhile p [] = []
  | dropWhile p (c::cs) = if p c then dropWhile p cs
                          else c :: cs
fun member x [] = false
  | member x (y::ys) = x = y orelse member x ys

fun allDistinct [] = true
  | allDistinct (x::xs) = not (member x xs) andalso allDistinct xs
fun allSame [] = true
  | allSame [x] = true
  | allSame (x0::x1::xs) = x0 = x1 andalso allSame (x1 :: xs)

fun withInputFromFile path f =
    let val is = TextIO.openIn path
    in  f is before TextIO.closeIn is
    end

fun renderSolutionIdsNarrow sIds =
    let val sIds = Vector.fromList sIds
        val len = Vector.length sIds
        fun renderId (i, id, s) =
            case ((i+1) mod 5, len - i - 1)
             of (_, 0) => id ^ s
              | (0, _) => id ^ "\\n" ^ s
              | _ => id ^ ", " ^ s
    in  Vector.foldri renderId "" sIds
    end

fun unique [] = []
  | unique (x::xs) =
    if member x xs
    then unique xs
    else x :: (unique xs)

fun fmtReal r =
    let val fmt = Real.fmt (StringCvt.FIX $ SOME 3)
        val (sign, r) = if Real.signBit r
                        then ("-", ~ r)
                        else ("", r)
    in  sign ^ fmt r
    end
fun fmtReal' n r =
    let val fmt = Real.fmt (StringCvt.FIX $ SOME n)
        val (sign, r) = if Real.signBit r
                        then ("-", ~ r)
                        else ("", r)
    in  sign ^ fmt r
    end
end
