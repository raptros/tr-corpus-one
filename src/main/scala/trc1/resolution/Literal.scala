package trc1.resolution

import scala.collection._
import scala.util.matching.Regex
import scala.util.control.Exception._

import scalaz._
import syntax.std.boolean._
import std.option._
import optionSyntax._
import syntax.monad._


//----------------------------------------------------------
// literal class
// keeps the literal as a string,
// the polarity of the literal (isNegated),
// and the predicate and list of arguments as individual strings
case class Literal(
  val originalLiteralString:String,
  val posLiteralString:String, 
  val isNegated:Boolean,
  val predSymbol:String, 
  val argList:List[String]) extends Unification {


  // arity: number of arguments
  def arity = argList.size

  // content literal:
  // This is specific to Johan Bos' Boxer system, 
  // which uses Neo-Davidsonian semantics!
  // A content literal has only one argument, and its predicate is not 'event'
  def isContent = (arity == 1) && (predSymbol != "event")

  // grounded literal: does not contain any variables
  // convert to string to get the string variant of the current form of this literal,
  // then break the term apart at "word boundaries" to get all terms and variables at all embedding depths,
  // then test that none of those pieces is a variable.
  def isGrounded:Boolean = takeTermApart(toString) forall { piece => !(isVariable(piece)) }

  // possibleMatch with other literal:
  // if they differ in negation, but have the same predicate symbol and arity
  def possibleMatch(otherL:Literal):Boolean = {
    otherL.isNegated != isNegated &&
    otherL.predSymbol == predSymbol &&
    otherL.arity == arity
  }

  // check if this literal unifies with another literal.
  // input: both literals' argument lists.
  // try this only if both literals have the same predicate symbols and arity,
  // and if one of them is negated and the other is not.
  def unifiableAndNegated(otherL:Literal):Option[Substitution] = if (possibleMatch(otherL)) {
    computeSubstitution(argList, otherL.argList)
  } else None

  // Two literals are the same if they have the same predicate and arguments.
  // Don't use literalString as a basis, as that can have extra whitespace
  override def equals(that:Any):Boolean = {
    that.isInstanceOf[Literal] && 
    predSymbol == that.asInstanceOf[Literal].predSymbol && 
    argList == that.asInstanceOf[Literal].argList &&
    isNegated == that.asInstanceOf[Literal].isNegated
  }

  // Two literals are opposites if they are the same except for their polarity
  def isNegationOf(that:Any) :Boolean = {
    that.isInstanceOf[Literal] && 
    predSymbol == that.asInstanceOf[Literal].predSymbol && 
    argList == that.asInstanceOf[Literal].argList &&
    isNegated != that.asInstanceOf[Literal].isNegated
  }

  def applySubstitution(substitution:Substitution):Literal = {
    copy(argList = argList map { a => applySubstitution(a, substitution) })
  }

  // for inspection: represent this class as the literal string
  override def toString:String = (if (isNegated) "-" else "") + 
  predSymbol + 
  (if (argList.size > 0) "(" + argList.mkString(",") + ")"
   else "") 
}


object Literal extends Unification {
  def apply(ols:String, p1:(String, Boolean), p2:(String, List[String])):Literal = apply(ols, p1._1, p1._2, p2._1, p2._2)

  // posLiteralString is the literal as string, without negation.
  // isNegated is true if literalString is negated.

  def apply(ols:String, p1:(String, Boolean)):Literal = apply(ols, p1, separatePredArg(p1._1))

  def apply(ols: String):Literal = apply(ols, separateLiteralAndNegation(ols))
}
