package trc1.resolution

/** Represents the disjunction in a CNF - keeps the literals in a set
  * @param literals1 a set of literals this clause will keep 
  * @note filters out elements
  * @throws EmptyClauseException if the final literals set is empty
  */
class Clause(literals1:Set[Literal]) {

  /** builds a clause from a list of strings by converting it into a set of Literals. */
  def this(stringlist:List[String]) = this(stringlist.toSet map { (s:String) => Literal(s) })
  
  /** the actually kept set of literals. this works because no literals are removed from literals1. */
  val literals = literals1 filterNot { l =>
    literals1 exists { _ isNegationOf l }
  }

  val size = literals.size
  val isSingleton = size == 1 
  val isEmpty = size == 0 

  if (isEmpty) throw new EmptyClauseException(toString)

  /** applies a substitution by returning a new Clause containing the updated literals */
  def applySubstitution(substitution:Substitution):Clause = 
    new Clause(literals map { _ applySubstitution substitution })

  override def toString:String = s"Clause(${literals})"

  /** produces a list of string literals representation for the clause */
  def toList = literals.toList map { _.toString }
}

