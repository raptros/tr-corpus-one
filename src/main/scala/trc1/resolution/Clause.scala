package trc1.resolution

import scala.collection._
import scala.util.matching.Regex
import scala.util.control.Exception._

//----------------------------------------------------------
// Clause class
// keeps a disjunction of literals as a set.
class Clause(literals1:Set[Literal]) {

  val literals = literals1 filterNot { l =>
    literals1 exists { _ isNegationOf l }
  } toSet

  if (isEmpty) throw new EmptyClauseException(toString)

  def this(stringlist:List[String]) = this(stringlist map { Literal(_) } toSet)

  // size of the clause: the number of its literals
  def size = literals.size

  // tests for singleton and empty clauses
  def isSingleton = size == 1 
  def isEmpty = size == 0 

  def applySubstitution(substitution:Substitution):Clause = 
    new Clause(literals map { _ applySubstitution substitution })
  

  // for inspection: represent this class as the list of literals
  override def toString:String = { "Clause(" + this.literals.toString + ")"}

  def toList = literals.toList map { _.toString }
}

