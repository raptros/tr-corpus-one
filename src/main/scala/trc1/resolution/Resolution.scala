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

import scalaz.syntax.std.boolean._
import scalaz.std.option._
import optionSyntax._
import scalaz.syntax.id._

object Resolution extends TakeLiteralApart {

  // Resolve to find difference:
  // Use Robinson's resolution to find the "difference" between two first-order formulas.
  // Keep the two formulas apart rather than joining them, as is usual in resolution.
  // Also only use resolution on a literal when there is only a single literal in the other formula
  // that is available for resolution. 
  // That is, be conservative in determining what the possible difference can be between formulas.
  // The function returns a pair of lists of string lists,
  // wrapped in an Option.
  def resolveToFindDifference(f1:List[List[String]], f2:List[List[String]]) :Option[(InferenceRule,InferenceRule)] = {
    
    var formula1 = new CNF(f1)
    var formula2 = new CNF(f2)
    try { 
      for {
        (res1, res2) <- doResolution(formula1, formula2)
        res2Neg = negateFormula(res2)
      } yield (new InferenceRule(res1, res2Neg), new InferenceRule(res2Neg, res1))
    } catch {
      // if we found the formulas too complex to process, return None
      case e:UnexpectedFormatOfFormulaException => None
    }
  }

  type StepState = Option[(CNF, CNF)]

  def runStepOverClauses(f1:CNF, f2:CNF, tail:Boolean, test:(Literal => Boolean)):StepState = {
    val buf = mutable.ListBuffer(f1 -> f2)
    for (ix <- singletonClauseIndices(f1); (cf1, cf2) <- buf.lastOption; 
         (nf1, nf2) <- singletonClauseUnificationStep(cf1, cf2, ix, test)) {
        buf += (nf1 -> nf2)
    }
    if (tail) buf.tail.lastOption else buf.lastOption
  }

  def doResolution(formula1:CNF, formula2:CNF):Option[(CNF, CNF)] = {
    /* In a first step, do resolution on literal L1 of clause C1 and L2 of C2 only if 
       * either C1 or C2 is a singleton clause,
       * both L1 and L2 are "content" rather than meta-predicates, where meta-predicates are predicates like agent(X, Y) or patient(X, Y) or
         event(X) that stem from a Neo-Davidsonian representation
       * L2 is the only literal that L1 unifies with in formula 1, and vice versa -- or L1 and L2 are fully grounded, and one is the
         negation of the other
     */

    val isContentLiteral = (l:Literal) => l.isContent
    val isContentOrGrounded = (l:Literal) => (l.isContent || l.isGrounded)

    try { 
      val init = for {
        (f1p, f2p) <- runStepOverClauses(formula1, formula2, false, isContentLiteral)
        (f2pp, f1pp) <- runStepOverClauses(f2p, f1p, false, isContentLiteral)
      } yield (f1pp -> f2pp)

      /* Now do resolution on L1 of C1 and L2 of C2 only if 
       * either C1 or C2 is singleton, 
       * L2 is the only formula-2 literal that L1 unifies with, and vice versa -- or L1 and L2 are fully grounded, and one is the negation of the other
       * either L1, L2 are content literals, or they are fully grounded
       * run this while there is change in either formula.
       */
      
      val rsocLeft = { (f1:CNF, f2:CNF) => 
        runStepOverClauses(f1, f2, true, isContentOrGrounded) 
      }.tupled

      val rsocRight = { (f1:CNF, f2:CNF) => 
        rsocLeft(f2, f1) map { _.swap } 
      }.tupled

      var next:StepState = None
      var cur = init
      do {
        next = cur flatMap { p => rsocLeft(p) orElse rsocRight(p) }
        cur = next orElse cur
      } while (next.nonEmpty)
      cur
    } catch {
      case e:EmptyClauseException => None
    }
  }

  /** invert the second of the two formulas.  currently only works when we have a single clause */
  def negateFormula(f:CNF):CNF = {
    if (f.clauses.size != 1) {
      throw new UnexpectedFormatOfFormulaException(f.toString)
    }
    val newclauses:List[List[String]] = f.clauses(0).literals.toList map { oldL =>
      List(negateStringLiteral(oldL.toString))
    }
    new CNF(newclauses)
  }

  // filter for singleton clause indices in formula f
  def singletonClauseIndices(f:CNF):List[Int] = f.clauseIndices.toList filter { ix => f.clauses(ix).isSingleton }

  // given a literal in a formula f1, find if it has a usable match in the given formula f2:
  // either a literal that is the exact negation of the literal, and both are grounded,
  // or a unique unification match
  def usableMatch(l1:Literal, f1:CNF, f2:CNF, literalOK:(Literal => Boolean)):Option[(Int,Literal,Substitution)] = {
    if (literalOK(l1)) (usableMatch1(l1, f1, f2, literalOK) orElse usableMatch2(l1, f1, f2, literalOK)) else None
  }
  
  def usableMatch1(l1:Literal, f1:CNF, f2:CNF, literalOK:(Literal => Boolean)):Option[(Int,Literal,Substitution)] = for {
    (c2Index, l2) <- (f2 groundedNegationMatch l1)
  } yield (c2Index, l2, newSubstitution)

  def usableMatch2(l1:Literal, f1:CNF, f2:CNF, literalOK:(Literal => Boolean)):Option[(Int,Literal,Substitution)] = for {
    (c2idx, l2, substitution) <- (f2 uniqueUnificationMatch l1) if literalOK(l2)
    good <- (f1 uniqueUnificationMatch l2)
  } yield (c2idx, l2, substitution)

  // given a singleton clause and a second formula,
  // try to find a unique unification match for the literal in the singleton clause
  // in the second formula, and do a resolution step if it works
  // returns true if a resolution step was done
  def singletonClauseUnificationStep( f1:CNF, f2:CNF, c1Index:Int, testLit: (Literal) => Boolean):Option[(CNF, CNF)] = {
    val literal1:Literal = f1.clauses(c1Index).literals.head
    for {
      (c2Index, literal2, substitution) <- usableMatch(literal1, f1, f2, testLit)
      nf1 = f1.resolveAsSingleton(c1Index, substitution)
      nf2 = f2.resolveWithSingleton(c2Index, literal2, substitution)
    } yield (nf1, nf2)
  }
}

