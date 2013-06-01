package trc1.resolution

import scala.collection._
import scala.util.matching.Regex
import scala.util.control.Exception._

//----------------------------------------------------------
// Clause class
// keeps a disjunction of literals as a set.
class Clause(literals1:Set[Literal]) {

  def this(stringlist:List[String]) = this(stringlist.toSet map { (s:String) => Literal(s) })
  
  val literals = literals1 filterNot { l =>
    literals1 exists { _ isNegationOf l }
  }

  val size = literals.size
  val isSingleton = size == 1 
  val isEmpty = size == 0 

  if (isEmpty) throw new EmptyClauseException(toString)

  def applySubstitution(substitution:Substitution):Clause = 
    new Clause(literals map { _ applySubstitution substitution })

  // for inspection: represent this class as the list of literals
  override def toString:String = s"Clause(${literals})"

  def toList = literals.toList map { _.toString }
}

