signature TEST_DB = 
sig
  type db

  type tid  = string
  type tnum = string
  type sid  = string

  val empty  : db
  val bind   : tid * tnum * sid * Outcome.outcome * db -> db
  val lookup : tid * tnum * sid * db -> Outcome.outcome
  val fold : ((tid * tnum * sid * Outcome.outcome * 'b) -> 'b) -> 'b -> db -> 'b
  
  (*calls function once for each student in the DB, regardless of whether they
    have a test result, allowing for DNRs *)
  val foldStudents : ((string * string * (string * Outcome.outcome) list * 'b) -> 'b) -> 'b -> db -> 'b
  val restrict : db -> sid list -> db
end
