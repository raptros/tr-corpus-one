package trc1.resolution

import scala.collection._
import scala.util.matching.Regex
import scala.util.control.Exception._

import scala.language.postfixOps

import scalaz._
import syntax.id._
import syntax.std.boolean._
import std.map._
import syntax.std.map._

/** represents a formula as a mapping from indices to Clause objects.
  * 
  */
class CNF(val clauses:Map[Int, Clause]) {

  /** constructs a CNF from the list of list of string literals format.
    * @throws EmptyClauseException if one of the clauses turns out to be empty
    */
  def this(listlist:List[List[String]]) = this((listlist.zipWithIndex map { _.swap }).toMap mapValues { new Clause(_) })

  /** clauseIndices: set of indices in the clauses map */
  val clauseIndices = clauses.keySet

  /** optionally finds the only literal in this CNF that is unifiable with the given literal.
    * @note matching requires the literals to be of opposite polarity.
    */
  def uniqueUnificationMatch(literal:Literal):Option[(Int,Literal,Substitution)] = {
    val ml = uniqueUnificationMatchLiterals(literal)
    (ml.size == 1) option ml.head 
  }

  /** finds match through comprehension; split out from uniqueUnificationMatch for clarity */
  def uniqueUnificationMatchLiterals(literal:Literal):List[(Int,Literal,Substitution)] = for {
    clauseIx <- clauseIndices.toList
    thisLiteral <- clauses(clauseIx).literals
    substitution <- thisLiteral.unifiableAndNegated(literal)
  } yield (clauseIx, thisLiteral, substitution)

  /** optionally finds a literal that is the exact negation of the given literal, if the given literal is grounded. */
  def groundedNegationMatch(literal1:Literal):Option[(Int, Literal)] = groundedNegationMatchCands(literal1).headOption

  /** finds grounded match candidates - comprehension in groundedNegationMatch split out for clarity */
  def groundedNegationMatchCands(literal1:Literal) = for {
    idx <- clauseIndices.toStream if (literal1.isGrounded)
    literal2 <- clauses(idx).literals if (literal2 isNegationOf literal1)
  } yield (idx, literal2)

  /** 
    * @param clauseIx (the index of) a singleton clause that has just been involved in a resolution step, and thus needs removal
    * @param substitution a substitution that will be applied to all clauses.
    * @return the CNF representing the changes
    * @throws ShouldNotBeHereException if the target of resolution, clauseIx, isn't available
    */
  def resolveAsSingleton(clauseIx:Int, substitution:Substitution):CNF = if (!(clauses contains clauseIx)) 
    throw new ShouldNotBeHereException("trying to resolve nonexisting clause as singleton", clauseIx.toString)
  else applySubstitution2(substitution) { clauses - clauseIx }

  /** 
    * @param clauseIx a clause that has just been involved in a resolution step with a singleton 
    * @param literal the target of the resolution step, and therefore to be removed.
    * @param substitution a substitution that will be applied to all clauses.
    * @return the CNF representing the changes
    * @throws ShouldNotBeHereException if the target clause, clauseIx, isn't here, or if that clause doesn't contain the literal
    * @throws EmptyClauseException if the updated clause becomes empty
    */
  def resolveWithSingleton(clauseIx:Int, literal:Literal, substitution:Substitution):CNF = if (!(clauses contains clauseIx)) 
    throw new ShouldNotBeHereException("trying to resolve nonexisting clause as singleton", clauseIx.toString)
  else if (!(clauses(clauseIx).literals contains literal)) 
    throw new ShouldNotBeHereException("trying to resolve", s"nonexisting literal ${literal}")
  else applySubstitution2(substitution) {
    val clause = clauses(clauseIx)
    clauses.updated(clauseIx, new Clause(clause.literals - literal))
  }


  /** apply the given substitution to every literal of every clause of this formula */
  def applySubstitution(substitution:Substitution):CNF = new CNF(clauses mapValues { _ applySubstitution substitution })

  /** apply the given substitution to every literal of every clause of this formula - with an updated clause map */
  def applySubstitution2(substitution:Substitution)(nClauses: => Map[Int, Clause]) = new CNF(nClauses mapValues { 
    _ applySubstitution substitution 
  })

  override def toString:String = clauses.valuesIterator map { _.toString } mkString " & "

  /** creates a List of List of string Literals to represent this CNF formula */
  def toListList:List[List[String]] = clauses.values.toList map { _.toList }

  /** creates a new CNF that represents the negation of this CNF, but only for single-clause cnfs 
    * @throws UnexpectedFormatOfFormulaException if this CNF does not have exactly one clause.
    */
  def negate:CNF = if (clauses.size != 1) 
    throw new UnexpectedFormatOfFormulaException(toString)
  else new CNF(clauses.head._2.literals.toList map { l => List(l.negate) })
}

