package trc1.resolution
/****
 * Robinson resolution: used to determine the difference between two formulas in CNF
 * 
 * Usually, Robinson resolution works like this:
 * -Given two formulas F, G, form CNF(F & not(G)) then try to derive the empty clause in order to conclude that F |= G
 * -Do search to find all possible ways to apply resolution to pairs of clauses, as F |= G follows if we can derive the empty clause in any
 * branch of the search tree.
 *
 * In our case, things work differently.
 * -Given two formulas F, G in CNF that we know represent two synonymous sentences, use Robinson resolution but keep F, G separate so we
 * always know which clause came from F and which came from G.
 * -Use resolution to remove common elements in F and G, leaving behind the difference between F and G.
 * 
 * The aim is to derive an inference rule F' -> G' where F' is a sub-formula of F, G' is a sub-formula of G, and the meaning of "F' -> G'"
 * is "Given any formula H, you can rewrite a sub-formula F' of H to G' without changing the semantics".
 * 
 * Do not use search, as we want to derive a _single_ inference rule "F' -> G'".
 * So we only do resolution on two clauses Cf of F and Cg of G if they contain literals Lf, not(Lg) such that Lf, Lg are unifiable and there
 * is no other positive literal in F that Lg could unify with, and there is no other negative literal in G that Lf could unify with.
 *
 * Method:
 * -Keep F, G separate.
 * -When doing resolution on clauses Cf of F and Cg of G such that Cf is a singleton: remove Cf from F, remove the resolution literal from
 * Cg.
 * -If Cg becomes empty (that is, resolution succeeds in deriving the empty clause), raise an EmptyClauseException, because in our setting
 * we do not expect F and G to be logically contradictory.
 * -When doing resolution on clauses Cf of F and Cg of G such that neither Cf and Cg is singleton:
 * --add the resulting clause Cr to both F and G.
 * -In F, mark all those literals in Cr that came from Cg as "needs to be removed".
 * -In G, mark all those literals in Cr that come from Cf as "needs to be removed."
 * If at the end of resolution, there are any literals left with a "needs to be removed" flag,
 * fail.
 *
 * (The case for singleton Cf is already implemented. The case for non-singleton Cf and Cg is not implemented yet.)
 *
 * Transforming the result of a successful resolution run to an inference rule:
 * TO BE DONE.
 */

import scala.collection._
import scala.util.matching.Regex
import scala.util.control.Exception._

import scalaz._
import State._
import syntax.state._
import std.list._
import std.option._
import optionSyntax._
import Traverse._
import syntax.traverse._
import syntax.monad._
import syntax.id._
import std.boolean._
import syntax.std.boolean._
import Validation._
import std.anyVal._
import syntax.monoid._
import std.tuple._

object Resolution {

  type CNFP = (CNF, CNF)
  type LitPred = Literal => Boolean
  type Step = (Int, Boolean, CNFP)

  val isContentLiteral:LitPred = _.isContent
  val isContentOrGrounded:LitPred = l => (l.isContent || l.isGrounded)
      
  /** finds the difference through modified, restriced Robinson resolution.
    *  @param f1 a first-order logic formula in CNF, represented as a list of clauses, where each clause is a list of string literals.
    *  @param f2 same as f1
    *  @return optionally, a pair of inference rule objects representing the differences
    */
  def resolveToFindDifference(f1:List[List[String]], f2:List[List[String]]):Option[(InferenceRule,InferenceRule)] =  {
    val resv = resolveToFindDifferenceV(f1, f2) 
    resv.swap foreach { (t:Throwable) => println(s"have a (${t.getClass}) with message: ${t.getMessage}") }
    resv.toOption
  }
  
  /** performs the resolution within the context of Validation, to catch exceptions */
  def resolveToFindDifferenceV(f1:List[List[String]], f2:List[List[String]]):Validation[Throwable,(InferenceRule,InferenceRule)] = for {
    formula1 <- fromTryCatch { new CNF(f1) } // empty clause exceptions
    formula2 <- fromTryCatch { new CNF(f2) } // ditto
    outerRes <- fromTryCatch { doResolution(formula1, formula2) } //should-not-be-here, empty clause
    innerRes <- (outerRes) //contains the possibility of failure
    (res1, res2) = innerRes //otherwise we get warnings about inability to monadize for filtering
    res2Neg <- fromTryCatch { res2.negate } //negation - unexpected format, probably
    ir1 <- fromTryCatch { new InferenceRule(res1, res2Neg) } //unexpected format
    ir2 <- fromTryCatch { new InferenceRule(res2Neg, res1) } //ditto
  } yield ir1 -> ir2

  /** performs two stages of resolution:
    * -one pass performed by startPair
    * -repeated loops of stepPair until changes cease, or until a step count limit is reached
    * @return the new state of the two formulas after successful resolution, or a failure
    */
  def doResolution(formula1:CNF, formula2:CNF):Validation[Throwable, CNFP] = {
    //first resolution stage
    val sp = startPair(formula1, formula2)
    val initState = (0, true, sp)
    //loop for second stage
    val ((count, last, fp), oFP2) = runWhile(initState)(stepPair) {
      case (c, l, _) => c < 100 && l
    }
    //ensure we have the most recent version of the formula pair
    val finalForm = oFP2 getOrElse fp
    //guard against unfinished resolutions by checking last here.
    if (!last) success(finalForm) else
      failure(new Exception(f"resolution failed: ran ${count}%d steps but have ${last}"))
  } 

  /** prepares the first stage of resolution - it does resolution on literals L1 of clause C1 and L2 of C2 only if 
    * -either C1 or C2 is a singleton clause,
    * -both L1 and L2 are "content" rather than meta-predicates, where meta-predicates are predicates like agent(X, Y) or patient(X, Y) or
    * event(X) that stem from a Neo-Davidsonian representation
    * -L2 is the only literal that L1 unifies with in formula 1, and vice versa -- or L1 and L2 are fully grounded, and one is the negation
    * of the other
    * @return a pair of cnfs describing what (if any) updates were made.
    * @throws ShouldNotBeHereException if resolution is attempted on something invalid
    * @throws EmptyClauseException if a clause becomes empty
    */
  def startPair(f1:CNF, f2:CNF):CNFP = {
    val rsocCL = (runStepOverClauses(isContentLiteral) _)
    val p1 = (f1 -> f2)
    val p2 = (rsocCL tupled p1) getOrElse p1
    (swapBack(rsocCL) tupled p2) getOrElse p2
  }

  /** steps the state of the second resolution stage, doing resolution on L1 of C1 and L2 of C2 only if:
    * -either C1 or C2 is singleton 
    * -L2 is the only formula-2 literal that L1 unifies with, and vice versa -- or L1 and L2 are fully grounded, and one is the negation of the other
    * -either L1, L2 are content literals, or they are fully grounded
    * changes the state
    */ 
  def stepPair:State[Step, Option[CNFP]] = for {
    cur <- get:State[Step, Step]
    (count, last, fCur) = cur
    oFNext = last ?? rsocBoth.tupled(fCur) //heck yeah
    nextState = (count + test(last), oFNext.nonEmpty, oFNext getOrElse fCur)
    _ <- put(nextState)
  } yield oFNext

  /** runStepOverClauses shortcut - it requires that the literals involved are content or grounded */
  val rsoc:(CNF, CNF) => Option[CNFP] = runStepOverClauses(isContentOrGrounded) _

  /** runStepOverClauses combining shortcut - first attempts to do f1, f2, then tries f2, f1 */
  val rsocBoth:(CNF, CNF) => Option[CNFP] = (f1, f2) => rsoc(f1, f2) orElse swapBack(rsoc)(f1, f2)

  /** runs the step clause function over every clause in the first formula passed in
    * @param f1 the formula to step over clauses in
    * @param f2 the formula to look in
    * @param test a predicate for testing whether literals are valid targets
    * @return optionally, the final state of the formulas
    * @throws ShouldNotBeHereException if resolution is attempted on something invalid
    * @throws EmptyClauseException if a clause becomes empty
    */
  def runStepOverClauses(test:LitPred)(f1:CNF, f2:CNF):Option[CNFP] = {
    val (lastState, changes) = (singletonClauseIndices(f1) runTraverseS (f1 -> f2)) { stepClause(test)(_) }
    changes.flatten.lastOption
  }
  
  /** steps the state by applying singletonClauseUnificationStep with the arguments. */
  def stepClause(test:LitPred)(ix:Int):State[CNFP, Option[CNFP]] = for {
    fp <- init:State[CNFP, CNFP]
    (f1, f2) = fp
    oNewFp = singletonClauseUnificationStep(f1, f2, ix, test)
    _ <- put(oNewFp | fp)
  } yield oNewFp

  /** searches for a unique unification match for the literal in a singleton clause within a second formula, and attempts to perform
    * resolution with the match.
    * @return optionally, the paired, updated f1 and f2.
    * @throws ShouldNotBeHereException if one of the resolution targets turns out not to be actually present
    * @throws EmptyClauseException if this causes a clause to become empty
    */
  def singletonClauseUnificationStep(f1:CNF, f2:CNF, c1Index:Int, test:LitPred):Option[CNFP] = for {
    literal1 <- f1.clauses(c1Index).literals.headOption
    (c2Index, literal2, substitution) <- usableMatch(literal1, f1, f2, test)
    nf1 = f1.resolveAsSingleton(c1Index, substitution) //exceptions here
    nf2 = f2.resolveWithSingleton(c2Index, literal2, substitution) //and here
  } yield (nf1, nf2)

  /** returns the indices of the clauses in f that are singleton clauses */
  def singletonClauseIndices(f:CNF):List[Int] = f.clauseIndices.toList filter { ix => f.clauses(ix).isSingleton }

  /** finds a usable match in formula f2 given a literal in formula f1:
    * either a literal that is the exact negation of the literal, and both are grounded,
    * or a unique unification match
    */
  def usableMatch(l1:Literal, f1:CNF, f2:CNF, test:LitPred):Option[(Int,Literal,Substitution)] = {
    test(l1) ?? (usableMatch1(l1, f1, f2, test) orElse usableMatch2(l1, f1, f2, test))
  }
  
  /** matches a literal exactly negating l1 if both literals are grounded */
  def usableMatch1(l1:Literal, f1:CNF, f2:CNF, test:LitPred) = for {
    (c2Index, l2) <- (f2 groundedNegationMatch l1)
  } yield (c2Index, l2, newSubstitution)

  /** finds a unique unification match. */
  def usableMatch2(l1:Literal, f1:CNF, f2:CNF, test:LitPred) = for {
    (c2idx, l2, substitution) <- (f2 uniqueUnificationMatch l1) if test(l2)
    good <- (f1 uniqueUnificationMatch l2)
  } yield (c2idx, l2, substitution)
}

