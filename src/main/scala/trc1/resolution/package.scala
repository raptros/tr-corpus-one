package trc1

package object resolution {
  import scala.collection._
  import scala.util.matching.Regex
  import scala.util.control.Exception._
  
  /** this is just the crucial fields out of an InferenceRule */ 
  case class InferenceRuleFinal(quantifiers:List[(String, Set[String])], lhs:List[String], rhs:List[String])

  /** pr */
  def finalizeInference(ir:InferenceRule):InferenceRuleFinal = InferenceRuleFinal(ir.quantifiers, ir.lhs, ir.rhs)

  /** this is it. */
  def compIRFs(first:InferenceRuleFinal, second:InferenceRuleFinal):Boolean = {
    (first.lhs == second.lhs) && (first.rhs == second.rhs)
  }

  /** a substitution is a mapping from variables to terms */
  type Substitution = Map[String,String]
  def newSubstitution = Map[String,String]()
  def newSubstitutionInit(values:(String,String)*) :Substitution = values.toMap

  /** deriving the empty class is an error - G is not supposed to follow from F without any rewriting rule */
  case class EmptyClauseException(data:String) extends Exception

  /** unexpected format in formula */
  case class UnexpectedFormatOfFormulaException(data:String) extends Exception

  /** an attempt to perform some action with some data where it cannot be done sensibly */
  case class ShouldNotBeHereException(where:String, what:String) extends Exception {
    val data = s"should not be here: ${where} with ${what}"
  }

  import scalaz._
  import std.map._
  import std.set._
  import std.string._
  import std.option._
  import std.anyVal._
  import syntax.std.function2._
  import syntax.monoid._

  /** NOT A PROPER SEMIGROUP IT JUST DISCARDS THE SECOND ONE. 
    * do not use this for situations where this would lead to literals being appended! it's only here so that an Option involving it can be
    * a Monoid!
    */
  implicit def literalSemi:Semigroup[Literal] = new Semigroup[Literal] {
    def append(f1:Literal, f2: => Literal):Literal = f1
  }

  /** this allows substitutions to be a monoid. the fact that it is necessary constitutes a pain in the ... neck. */
  implicit def substMonoid:Monoid[Substitution] = new Monoid[Substitution] {
    def zero = Map[String, String]()
    def append(m1: Map[String, String], m2: => Map[String, String]) = m1 |+| m2
  } 

  /** clauses are a monoid - however, actually creating the zero clause will lead to an exception. */
  implicit def clauseMonoid:Monoid[Clause] = new Monoid[Clause] {
    def zero = new Clause(Set.empty[Literal])
    def append(m1:Clause, m2: => Clause):Clause = new Clause(m1.toList ++ m2.toList)
  }

  /** yes, CNFs do constitute a monoid. this enables a couple of neat shortcuts. */
  implicit def cnfMonoid:Monoid[CNF] = new Monoid[CNF] { 
    def zero = new CNF(Map.empty[Int,Clause])
    def append(m1:CNF, m2: => CNF) = new CNF(m1.toListList ++ m2.toListList)
  }


}
