fun log2 x = Math.ln x / Math.ln 2.0

fun round n x =
  let val big = floor (real n * x + 0.5)
  in  real big / real n
  end

fun entropy ns =
  let val total = real (foldr op + 0 ns)
      val rs = map (fn n => real n / total) ns
      fun summand r = r * log2 r
  in  ~ (foldr (fn (r, total) => summand r + total) 0.0 rs)
  end

      