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
