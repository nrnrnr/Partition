signature FINITE_MAP = 
sig
  type key
  type 'a map

  exception NotFound of Key

  val empty  : 'a map
  val bind   : key * 'a * 'a map -> 'a map
  val lookup : key * 'a map -> 'a
end