package logic

package object resolution {
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
  case class EmptyClauseException(data:String) extends Exception(data)

  /** unexpected format in formula */
  case class UnexpectedFormatOfFormulaException(data:String) extends Exception(data)

  /** an attempt to perform some action with some data where it cannot be done sensibly */
  case class ShouldNotBeHereException(where:String, what:String) extends Exception(s"should not be here: ${where} with ${what}")

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

  /** calls run with arguments flipped, then swaps the resulting pair. */
  def swapBack[A](run:(A, A) => Option[(A,A)]):(A,A) => Option[(A,A)] = { (l, r) =>
    run flip(l, r) map { _.swap }
  }


  import State._
  import syntax.state._
  import annotation.tailrec

  /** runs sf repeatedly - a do-while loop implemented for State. the step function is run before the test is applied.
    * @tparam S the type that represents the state
    * @tparam V the value returned by the step function
    * @param initState the initial state for the loop
    * @param sf the state stepping function
    * @param t a predicate on S to determine if the loop should continue
    * @return the final state and value
    */
  def runWhile[S,V](initState:S)(sf:State[S,V])(t:(S => Boolean)):(S, V) = {
    @tailrec
    def loop(sPre:S):(S,V) = {
      val (s, v) = (sf run sPre)
      if (t(s)) loop(s) else (s, v)
    }
    loop(initState)
  }
    
}
