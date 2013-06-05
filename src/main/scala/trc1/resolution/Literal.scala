package trc1.resolution

import scalaz._
import syntax.std.boolean._
import std.option._
import std.anyVal._
import optionSyntax._
import std.string._


/** represents a literal, with the original string, the polarity, the predicate, and list of arguments as individual strings
  * @note the originalLiteralString does not have to accurately represent this literal. use toString instead.
  */
case class Literal(
  val originalLiteralString:String,
  val posLiteralString:String, 
  val isNegated:Boolean,
  val predSymbol:String, 
  val argList:List[String]) extends Unification {

  // arity: number of arguments
  val arity = argList.size

  /** in Johan Bos' Boxer system, which uses Neo-Davidsonian semantics,  content literal has only one argument, and its predicate is not
   * 'event'
   */
  val isContent = (arity == 1) && (predSymbol != "event")

  /** a grounded literal does not contain any variables.
    * convert to string to get the string variant of the current form of this literal, then break the term apart at "word boundaries" to get
    * all terms and variables at all embedding depths, then test that none of those pieces is a variable.
    */
  val isGrounded:Boolean = takeTermApart(toString) forall { piece => !(isVariable(piece)) }

  /** determines if this literal and the other differ in negation, but have the same predicate symbol and arity */
  def possibleMatch(otherL:Literal):Boolean = otherL.isNegated != isNegated &&  otherL.predSymbol == predSymbol && otherL.arity == arity

  /** check if this literal unifies with another literal, by examining argument lists
    * @note try this only if both literals have the same predicate symbols and arity, and if one of them is negated and the other is not.
    * @note the (??) syntax comes from scalaz - maps constitute monoids, so the boolean syntax for this can be used.
    */
  def unifiableAndNegated(otherL:Literal):Option[Substitution] = possibleMatch(otherL) ?? computeSubstitution(argList, otherL.argList)

  /** this is the same as the other literal if both have the same predicate and arguments.
   * @note Don't use literalString as a basis, as that can have extra whitespace
   */
  override def equals(that:Any):Boolean = {
    that.isInstanceOf[Literal] && 
    predSymbol == that.asInstanceOf[Literal].predSymbol && 
    argList == that.asInstanceOf[Literal].argList &&
    isNegated == that.asInstanceOf[Literal].isNegated
  }

  /** Two literals are opposites if they are the same except for their polarity */
  def isNegationOf(that:Any):Boolean = {
    that.isInstanceOf[Literal] && 
    predSymbol == that.asInstanceOf[Literal].predSymbol && 
    argList == that.asInstanceOf[Literal].argList &&
    isNegated != that.asInstanceOf[Literal].isNegated
  }

  /** applies substitution by creating a copy of the literal with a modified arglist. */
  def applySubstitution(substitution:Substitution):Literal = {
    copy(argList = argList map { a => applySubstitution(a, substitution) })
  }

  /** produces a string representing the negation of this literal. */
  def negate:String = negateStringLiteral(toString)

  /** for inspection: represent this class as the literal string */
  override def toString:String = (isNegated ?? "-") + predSymbol + (argList.size > 0) ?? "(" + (argList mkString ",") + ")"
}


object Literal extends Unification {
  def apply(ols:String, p1:(String, Boolean), p2:(String, List[String])):Literal = apply(ols, p1._1, p1._2, p2._1, p2._2)
 
  // posLiteralString is the literal as string, without negation.
  // isNegated is true if literalString is negated.

  def apply(ols:String, p1:(String, Boolean)):Literal = apply(ols, p1, separatePredArg(p1._1))

  def apply(ols: String):Literal = apply(ols, separateLiteralAndNegation(ols))
}
