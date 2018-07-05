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
end

structure Util : UTILITIES = struct
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

end
