package trc1.resolution

import scala.collection._
import scala.util.matching.Regex
import scala.util.control.Exception._

import scala.language.postfixOps

import scalaz._
import syntax.std.boolean._

/** keep a formula as a mapping from indices to Clause objects.*/
class CNF(var clauses:Map[Int, Clause]) {

  def this(listlist:List[List[String]]) = this(listlist.zipWithIndex map {
    case (clauselist, index) => (index, new Clause(clauselist)) 
  } toMap)

  /** clauseIndices: set of indices in the clauses map */
  def clauseIndices:Set[Int] = clauses.keySet

  /** check if the given literal is only unifiable with a single literal in this CNF formula. If this is the case,
    * return a triple of  
    * (clause Index, literal in this CNF that is matched, substitution) otherwise return None. 
    * Note that matching requires the literals to be of opposite polarity.
    */
  def uniqueUnificationMatch(literal:Literal):Option[(Int,Literal,Substitution)] = {
    val matchingLiterals = for {
      clauseIx <- clauseIndices.toList
      thisLiteral <- clauses(clauseIx).literals
      substitution <- thisLiteral.unifiableAndNegated(literal)
    } yield (clauseIx, thisLiteral, substitution)
    // single matching literal.
    (matchingLiterals.size == 1) option matchingLiterals.head
  }

  /** check whether the given literal is grounded, and the current formula contains a literal that is the exact negation of that literal */
  def groundedNegationMatch(literal1:Literal):Option[(Int, Literal)] = {
    val cands = for {
      idx <- clauseIndices.toStream if (literal1.isGrounded)
      literal2 <- clauses(idx).literals if (literal2 isNegationOf literal1)
    } yield (idx, literal2)
    cands.headOption
  }

  /** clauseIx is a singleton clause that has just been involved in a resolution step, so this clause disappears. Remove it from the
    * "clauses" map,  then apply the substitution to all remaining clauses. raises an error if we don't have this clause
    */
  def resolveAsSingleton(clauseIx:Int, substitution:Substitution):CNF = if (!this.clauses.contains(clauseIx)) 
    throw new ShouldNotBeHereException("trying to resolve nonexisting clause as singleton", clauseIx.toString)
  else applySubstitution2(substitution) { clauses - clauseIx }

  /** clauseIx is a clause that has just been involved in a resolution step with a singleton targeting the literal 'literal' occurring in
    * clause 'clauseIx'. Remove 'literal' from the clause 'clauseIx'. (There will be no new literals added to 'clauseIx' because the other
    * clause was a singleton.) Apply the substitution to all clauses. 
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

  def applySubstitution1(nClauses:Map[Int, Clause], substitution:Substitution) = new CNF(nClauses mapValues { 
    _ applySubstitution substitution 
  })

  def applySubstitution2(substitution:Substitution)(nClauses: => Map[Int, Clause]) = new CNF(nClauses mapValues { 
    _ applySubstitution substitution 
  })

  override def toString:String = clauses.valuesIterator map { _.toString } mkString " & "

  def toListList:List[List[String]] = clauses.values.toList map { _.toList }

}

