signature SET = 
sig
  type elem
  type set

  exception NotFound of elem

  val empty  : set
  val add    : elem * set -> set
  val member : elem * set -> bool
  val isEmpty : set -> bool

  val fold : (elem * 'a -> 'a) -> 'a -> set -> 'a

  val partition : (elem * elem -> bool) -> set -> set list
    (* arg is an equivalence relation,
       partitions set into maximal equivalent subsets, i.e. if

          p = partition eq s

       then for all s' in p : for all x, x' in s' : eq (x, x')
       also for all s, s' in p :
               if s <> s' then forall x in s, x' in s' : not (eq (x, x'))


       Example: partition (fn (n, m) => n mod 10 = n mod 10) { 1, 3, 11, 13, 33 }
                 =
                [ {1, 11}, {3, 13, 33} ]

   *)           

  val /<=/ : set * set -> bool (* subset *)
  val /==/ : set * set -> bool (* equality *)

  val /*/  : set * set -> set (* intersection *)
  val /+/  : set * set -> set (* union *)
  val /-/  : set * set -> set (* difference *)

  val representative : set -> elem option
    (* representative empty == NONE *)
    (* if s is not empty, member (representative s, s) *)

end