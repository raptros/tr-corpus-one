package trc1.resolution

import scalaz._
import syntax.std.boolean._
import syntax.monoid._
import std.string._
import std.list._
import std.map._
import std.anyVal._

import scala.util.matching.Regex

/** Extracts the inference rule from resolved CNFs */
class InferenceRule(lhsCNF:CNF, rhsCNF:CNF) {
  // left-hand side of the rule (with old variable names, to be changed below),
  // sorted alphabetically by predicate symbols
  val lhsOld = cnfToLitList(lhsCNF)
  val rhsOld = cnfToLitList(rhsCNF)

  val constantVariableRegex = new Regex("""^[A-Za-z][A-Za-z0-9_]*$""")

  /** create a mapping from lhs constants and variables to new variable names */
  val lhsArgumentMap = mapArgumentsToNewVariables(lhsOld, "l")
  
  /** a mapping from rhs constants and variables to new variable names, without constants and variables that already occur in the lhs */
  val rhsArgumentMap = mapArgumentsToNewVariables(rhsOld, "r", lhsArgumentMap.keySet)

  /** the combined mapping */
  val combinedMap = lhsArgumentMap ++ rhsArgumentMap

  /** converts a CNF into a list of literals.
    * Note that this means it only handles singleton clauses.
    * @param f a CNF.
    * @return a list of literals, prepared and sorted properly
    * @throws UnexpectedFormatOfFormulaException if the formula is not a flat junction of singletons.
    */
  def cnfToLitList(f:CNF):List[Literal] = if (f.clauses.values exists { clause => !clause.isSingleton })
    throw new UnexpectedFormatOfFormulaException("formula not flat " + f)
  else {
    // map f to  flat list of literals
    val litListUnsorted:List[Literal] = f.clauses.values.toList map { _.literals.head }

    // check if we have any predicates that occur more than once. those will go
    // all the way to the end of the list of sorted literals
    // powered by monoid instances!
    val predCount:Map[String,Int] = (litListUnsorted map { lit => Map(lit.negate -> 1) } foldLeft Map.empty[String, Int]) { _ |+| _ }

    // convert the CNF to a list of literals sorted alphabetically by negation and predicate symbols.
    // Literals whose predicate symbol occurs more than once appear all the way at the end
    import scala.math.Ordering
    implicit val litSort:Ordering[Literal] = Ordering.by { (l:Literal) => 
      val lneg = l.negate
      val lpc = predCount.getOrElse(lneg, 0)
      (lpc -> lneg)
    }
    litListUnsorted.sorted
  }

  /** converts a literal to its negation status and predicate symbol */
  def negAndPredSymbol(lit:Literal):String = (lit.isNegated) ?? "-"  + lit.predSymbol

  /** make a list of all the constants and variables in both the lhs and RHS CNF, remove all occurrences of a duplicate except the first, and
    * pair each constant or variable with a string Xsi for s = l/r for left-hand or right-hand side 
    * and i the position in the list of constants and variables
    */
  def mapArgumentsToNewVariables(f:List[Literal],formulaLabel:String, exceptArguments:Set[String] = Set()):Map[String,String] = {
    val argListNodup:List[String] = f.distinct.reverse flatMap { _.argList } filterNot { exceptArguments contains _ }

    // sanity check: must be all constants or variables, no function symbols
    argListNodup foreach { _ match {
        case constantVariableRegex() =>  // if we match the constant & variable regex, we are good
        case somethingElse => throw new UnexpectedFormatOfFormulaException(somethingElse) // if we don't match it, we have an error
      }
    }

    // pair each constant or variable with a string Xsi, and transform to a map 
    argListNodup.zipWithIndex.toMap mapValues { oldArgIndex => f"X${formulaLabel}${oldArgIndex}%d" }
  }


  ///here be monoids!
  def literalToStringWithNewArguments(literal:Literal, newArgMap: Map[String,String]):String = {
    val args = (literal.arity > 0) ?? {  
      "(" + (literal.argList map { arg => newArgMap.getOrElse(arg, "XXX") } mkString ",") + ")"
    }
    val litSym = (literal.isNegated) ?? "-" 
    s"${litSym}${literal.predSymbol}${args}"
  }

  override def toString = {
    val qs = (quantifiers map { case (q, vs) => q + " " + (vs mkString ",") } mkString " ")
    val lS = lhs mkString " & "
    val rS = rhs mkString " & "
    s"${qs}: \n${lS} -> \n${rS}"
  }

  /** now starting with the parts of this class to be used from outside quantifiers: 
    * a list of pairs of a string ("forall", "exists") and a set of variables that have this scope The variables introduced in the LHS are
    * universally quantified and non-nested.  The variables introduced in the RHS, if any, are existentially quantified and nested below the
    * universal quantifiers
    */
  val quantifiers:List[(String,Set[String])] =
    ("forall" -> lhsArgumentMap.values.toSet)::((rhsArgumentMap.size > 0) ?? List("exists" -> rhsArgumentMap.values.toSet))

  /** lhs is a conjunction of literals, realized as a list of strings.  negation is realized as "-".*/
  val lhs:List[String] = lhsOld map { lit => literalToStringWithNewArguments(lit, lhsArgumentMap) }

  /** rhs is a conjunction of literals, realized as a list of strings.  negation is realized as "-".*/
  val rhs:List[String] = rhsOld map { lit => literalToStringWithNewArguments(lit, combinedMap) }

  /** It's enough to just compare the lhs and the rhs formulas.  We don't need to compare the quantifiers, as every variable that has an "l"
    * in its name (is introduced in the LHS) is universally quantified, and every variable that has an "r" in its name (is introduced in the
    * RHS) is existentially quantified. So you could in principle read the quantifier off the formula.
    */
  override def equals(that:Any) :Boolean = {
    that.isInstanceOf[InferenceRule] && 
    lhs == that.asInstanceOf[InferenceRule].lhs && 
    rhs == that.asInstanceOf[InferenceRule].rhs
  }

  /** produces the InferenceRuleFinal version of this. */
  def inferenceFin:InferenceRuleFinal = InferenceRuleFinal(quantifiers, lhs, rhs)
}
