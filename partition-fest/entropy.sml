structure Entropy :> sig
              structure H : HISTOGRAM

              type entropy = real
              val entropy : ''a H.counter -> entropy
              val histogram : (''a * ''b) list -> ''a H.counter
                  (* a's are repeated but b's are unique, where an a
                     represents an outcome and b represents a student
                     that got that outcome *)
                  (* if we want to compute the entropy of a single test,
                     instantiate a with a single outcome; if we want to compute
                     the entropy of a set of tests, instantiate a with a vector of outcomes *)
                  (* for each a, the returned histogram counts how many b's have that a *)
          end =
  struct
    fun impossible s = let exception Impossible of string in raise Impossible s end

    fun member x [] = false
      | member x (y::ys) = x = y orelse member x ys

    fun allDistinct [] = true
      | allDistinct (x::xs) = not (member x xs) andalso allDistinct xs

    fun histogram pairs =
        let val (outcomes, students) = ListPair.unzip pairs
            val () = if not (allDistinct students)
                     then  impossible "duplicate students"
                     else  ()
        in  foldl H.inc H.zeroes outcomes
        end


    fun log2 r = Real.Math.ln r / Real.Math.ln 2

    fun entropy histogram =
        let val keys = H.nonzeroKeys histogram
            val total = real (H.total histogram)
            fun e key = (* summand for key's contribution to the entropy *)
                let val p = real (H.count (key, histogram)) / total
                in  p * log2 p
                end
        in  ~ (foldl (fn (key, sum) => e key + sum) 0.0 keys)
        end
  end

signature HISTOGRAM = sig
      type 'a counter (* A finite map from a to a natural number *)
      val zeroes : 'a counter (* Maps every a to zero *)
      val inc : ''a * ''a counter -> ''a counter
      val count : ''a * ''a counter -> int
      val total : 'a counter -> int (* Sum of the numbers associated with all of the a's *)

      val nonzeroKeys : 'a counter -> 'a list
  end

structure Histogram :> HISTOGRAM =
  struct
    structure NodeMap :> FINITE_MAP where type key = G.node = struct
  type key = G.node
  type 'a map = (key * 'a) list
  exception NotFound of key

  val empty : 'a map = []

  fun lookup (k, []) = raise NotFound k
    | lookup (k, (j, v)::rest) =
      if k = j then v
      else lookup (k, rest)

  fun bind (k, v, m) = (k, v) :: m

  fun mapFold _ z [] = z
    | mapFold f z ((k, v)::rest) = mapFold f (f (k, v, z)) rest
end

  end
