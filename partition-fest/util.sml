signature UTILITIES = sig
  val insertion_sort : ('a * 'a -> order) -> 'a list -> 'a list
  val vcompare : ('a * 'a -> order) -> 'a list * 'a list -> order option
  val vcomparePartial : ('a * 'a -> order option) -> 'a list * 'a list -> order option
  val flatten  : 'a list list -> 'a list
  val dropWhile : ('a -> bool) -> 'a list -> 'a list
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
end
