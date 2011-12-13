signature PROPOSITION = sig
  type testset
  type prop = (bool * string * string * string * (string * bool) list)
  type simpProp = (string * (string * bool) list)

  val /->/ : simpProp * simpProp -> bool
  val partition : (prop * prop -> bool) -> prop list -> prop list list
  val makePropList : testset list -> prop list
  val makePropMapAndSet : prop list list -> simpProp list * string Map.map
  val makePropGraph : simpProp list -> BasicGraph.graph
  val eq : prop * prop -> bool
end
