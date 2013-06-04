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
import Apply._
import syntax.std.function2._
import std.anyVal._
import syntax.monoid._
import std.tuple._

import annotation.tailrec

object Resolution {

  type StepState = Option[(CNF, CNF)]
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
    resv.swap foreach { (t:Throwable) => println(t.getMessage) }
    resv.toOption
  }
  /** performs the resolution within the context of Validation, to catch exceptions */
  def resolveToFindDifferenceV(f1:List[List[String]], f2:List[List[String]]):Validation[Throwable,(InferenceRule,InferenceRule)] = for {
    formula1 <- fromTryCatch { new CNF(f1) } // empty clause exceptions
    formula2 <- fromTryCatch { new CNF(f2) } // ditto
    outerRes <- fromTryCatch { doResolution(formula1, formula2) } //should-not-be-here, empty clause
    innerRes <- (outerRes toSuccess (new Throwable("resolution failed"))) //could be None without those
    (res1, res2) = innerRes //otherwise we get warnings about inability to monadize for filtering
    res2Neg <- fromTryCatch { res2.negate } //negation - unexpected format, probably
    ir1 <- fromTryCatch { new InferenceRule(res1, res2Neg) } //unexpected format
    ir2 <- fromTryCatch { new InferenceRule(res2Neg, res1) } //ditto
  } yield ir1 -> ir2

  /** performs two stages of resolution:
    * -one pass performed by startPair
    * -repeated loops of stepPair until changes cease, or until a step count limit is reached
    * @return the new state of the two formulas, as long as resolution was successful
    */
  def doResolution(formula1:CNF, formula2:CNF):Option[CNFP] = for {
    sp <- startPair(formula1, formula2)
    initState = (0, true, sp)
    ((count, last, fp), _) = runUntil(initState)(stepPair) {
      case (c, l, _) => c < 100 && l
    }
  } yield fp

  /** prepares the first stage of resolution - it does resolution on literals L1 of clause C1 and L2 of C2 only if 
    * -either C1 or C2 is a singleton clause,
    * -both L1 and L2 are "content" rather than meta-predicates, where meta-predicates are predicates like agent(X, Y) or patient(X, Y) or
    * event(X) that stem from a Neo-Davidsonian representation
    * -L2 is the only literal that L1 unifies with in formula 1, and vice versa -- or L1 and L2 are fully grounded, and one is the negation
    * of the other
    * @returns optionally, a pair of CNFS describing the new formula state
    * @throws ShouldNotBeHereException if resolution is attempted on something invalid
    * @throws EmptyClauseException if a clause becomes empty
    */
  def startPair(formula1:CNF, formula2:CNF):Option[CNFP] = for {
    (f1p, f2p) <- runStepOverClauses(formula1, formula2, true, isContentLiteral) 
    (f2pp, f1pp) <- runStepOverClauses(f2p, f1p, true, isContentLiteral) 
  } yield (f1pp -> f2pp)

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
    nextState = (count + test(last), oFNext.nonEmpty, oFNext | fCur)
    _ <- put(nextState)
  } yield oFNext

  /** runStepOverClauses shortcut - it requires that the literals involved are content or grounded */
  val rsoc:(CNF, CNF) => Option[CNFP] = runStepOverClauses(_, _, false, isContentOrGrounded)

  /** runStepOverClauses combining shortcut - first attempts to do f1, f2, then tries f2, f1 */
  val rsocBoth:(CNF, CNF) => Option[CNFP] = (f1, f2) => rsoc(f1, f2) orElse { rsoc(f2, f1) map { _.swap } }

  /** runs the step clause function over every clause in the first formula passed in
    * @param f1 the formula to step over clauses in
    * @param f2 the formula to look in
    * @param addLast
    * @param test a predicate for testing whether literals are valid targets
    * @return optionally, the new state of the formulas
    * @throws ShouldNotBeHereException if resolution is attempted on something invalid
    * @throws EmptyClauseException if a clause becomes empty
    */
  def runStepOverClauses(f1:CNF, f2:CNF, addLast:Boolean, test:LitPred):Option[CNFP] = {
    val (lastState, changes) = (singletonClauseIndices(f1) runTraverseS (f1 -> f2)) { stepClause(test)(_) }
    (changes :+ (addLast option lastState)).flatten.lastOption
  }

  /** steps the state by applying singletonClauseUnificationStep with the arguments. */
  def stepClause(test:LitPred)(ix:Int):State[CNFP, Option[CNFP]] = for {
    fp <- init:State[CNFP, CNFP]
    (f1, f2) = fp
    oNewFp = singletonClauseUnificationStep(f1, f2, ix, test)
    _ <- put(oNewFp | fp)
  } yield oNewFp

  /** runs the stepUntil function from an initial state */
  def runUntil[S,V](initState:S)(sf:State[S,V])(t:(S => Boolean)):(S, V) = stepUntil[S,V](sf)(t).run(initState)

  /** basically, a do-while loop that operates on States */
  def stepUntil[S,V](sf:State[S,V])(test:S => Boolean):State[S,V] = for {
    v <- sf
    p <- gets(test)
    n <- p ? stepUntil(sf)(test) | state(v)
  } yield n

  /** searches for a unique unification match for the literal in a singleton clause within a second formula, and attempts to perform
    * resolution with the match.
    * @returns optionally, the paired, updated f1 and f2.
    * @throws ShouldNotBeHereException if one of the resolution targets turns out not to be actually present
    * @throws EmptyClauseException if this causes a clause to become empty
    */
  def singletonClauseUnificationStep(f1:CNF, f2:CNF, c1Index:Int, testLit: (Literal) => Boolean):Option[CNFP] = for {
    literal1 <- f1.clauses(c1Index).literals.headOption
    (c2Index, literal2, substitution) <- usableMatch(literal1, f1, f2, testLit)
    nf1 = f1.resolveAsSingleton(c1Index, substitution) //exceptions here
    nf2 = f2.resolveWithSingleton(c2Index, literal2, substitution) //and here
  } yield (nf1, nf2)

  /** filter for singleton clause indices in formula f */
  def singletonClauseIndices(f:CNF):List[Int] = f.clauseIndices.toList filter { ix => f.clauses(ix).isSingleton }

  /** find a usable match in formula f2 given a literal in formula f1:
    * either a literal that is the exact negation of the literal, and both are grounded,
    * or a unique unification match
    */
  def usableMatch(l1:Literal, f1:CNF, f2:CNF, literalOK:(Literal => Boolean)):Option[(Int,Literal,Substitution)] = {
    literalOK(l1) ?? (usableMatch1(l1, f1, f2, literalOK) orElse usableMatch2(l1, f1, f2, literalOK))
  }
  
  /** matches a literal exactly negating l1 if both literals are grounded */
  def usableMatch1(l1:Literal, f1:CNF, f2:CNF, literalOK:(Literal => Boolean)):Option[(Int,Literal,Substitution)] = for {
    (c2Index, l2) <- (f2 groundedNegationMatch l1)
  } yield (c2Index, l2, newSubstitution)

  /** finds a unique unification match. */
  def usableMatch2(l1:Literal, f1:CNF, f2:CNF, literalOK:(Literal => Boolean)):Option[(Int,Literal,Substitution)] = for {
    (c2idx, l2, substitution) <- (f2 uniqueUnificationMatch l1) if literalOK(l2)
    good <- (f1 uniqueUnificationMatch l2)
  } yield (c2idx, l2, substitution)
}

