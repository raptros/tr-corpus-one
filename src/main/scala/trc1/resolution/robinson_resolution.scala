package trc1.resolution
/****
 * Robinson resolution
 * used to determine the difference between two formulas in CNF
 * 
 * Usually, Robinson resolution works like this:
 * Given two formulas F, G, form CNF(F & not(G))
 * then try to derive the empty clause in order to conclude that F |= G
 * Do search to find all possible ways to apply resolution to pairs of clauses,
 * as F |= G follows if we can derive the empty clause in any branch of the search tree.
 *
 * In our case, things work differently.
 * 
 * Given two formulas F, G in CNF
 * that we know represent two synonymous sentences,
 * use Robinson resolution but keep F, G separate
 * so we always know which clause came from F and which came from G.
 * Use resolution to remove common elements in F and G,
 * leaving behind the difference between F and G.
 * 
 * The aim is to derive an inference rule F' -> G'
 * where F' is a sub-formula of F, G' is a sub-formula of G,
 * and the meaning of "F' -> G'" is
 * "Given any formula H, you can rewrite a sub-formula F' of H to G'
 * without changing the semantics".
 * 
 * Do not use search, as we want to derive a _single_ inference rule "F' -> G'".
 * So we only do resolution on two clauses Cf of F and Cg of G if they contain
 * literals Lf, not(Lg) such that Lf, Lg are unifiable and
 * there is no other positive literal in F that Lg could unify with, and
 * there is no other negative literal in G that Lf could unify with.
 *
 * Method:
 * Keep F, G separate.
 * When doing resolution on clauses Cf of F and Cg of G such that Cf is a singleton:
 * remove Cf from F, remove the resolution literal from Cg.
 * If Cg becomes empty (that is, resolution succeeds in deriving the empty clause),
 * raise an EmptyClauseException, because in our setting we do not expect
 * F and G to be logically contradictory.
 * When doing resolution on clauses Cf of F and Cg of G such that neither Cf and Cg is singleton:
 * add the resulting clause Cr to both F and G.
 * In F, mark all those literals in Cr that came from Cg as "needs to be removed".
 * In G, mark all those literals in Cr that come from Cf as "needs to be removed."
 * If at the end of resolution, there are any literals left with a "needs to be removed" flag,
 * fail.
 *
 * (The case for singleton Cf is already implemented. The case for non-singleton Cf and Cg
 * is not implemented yet.)
 *
 * Transforming the result of a successful resolution run to an inference rule:
 * TO BE DONE.
***/

import scala.collection._
import scala.util.matching.Regex
import scala.util.control.Exception._


//----------------------------------------------------------
// custom exception:
// In our case, we don't want to derive the empty clause,
// as that means that G follows from F logically, without 
// application of any rewriting rule
case class EmptyClauseException(data:String) extends Exception

// another custom exception:
// unexpected format in formula
case class UnexpectedFormatOfFormulaException(data:String) extends Exception

//----------------------------------------------------------
// a substitution is a mapping from variables to terms

//----------------------------------------------------------
// Given a literal (without negation), separate predicate and arguments
// and return them as a pair (predicate, argList)
// predicate is a string, and argList is a list of strings
//
// Given a possibly negated literal, take it apart into 
// positive literal and potential negation
trait TakeLiteralApart {
  // two regular expressions that take apart predicate and arguments
  val predArgRegex = new Regex("""^(.*?)\((.*?)\)$""", "pred", "argstring")
  val atomRegex = new Regex("""^([^(),]*)$""", "atom")
  // identify negated literal
  val negationRegex = new Regex("""^\-(.*?)$""", "pL")

  // use the regular expressions to take apart literals and detect atoms
  def separatePredArg(literalString: String) :Pair[String, List[String]] = {
    // do we have a predicate with arguments?
    predArgRegex findFirstIn literalString match {

      case Some(predArgRegex(pred, argstring)) => (pred, splitArgstring(argstring))

      // failing that, do we have an atom?
      case None => atomRegex findFirstIn literalString match { 

	case Some(atomRegex(atom)) => (atom, List[String]())
	case None => throw new UnexpectedFormatOfFormulaException("expected atom or literal, got " + literalString)
      }
    }
  }

  // split a string that represents a sequence of comma-separated arguments:
  // don't split where a comma is embedded in an argument, as in f(X, Y)
  def splitArgstring(argstring:String, priorBracketLevel:Int = 0, index:Int = 0, startOfArg:Int = 0) :List[String] = {
    if (index >= argstring.size) List(argstring.substring(startOfArg).replaceAll("""\s+""", ""))
    else {
      argstring.charAt(index) match {
	case '(' =>  splitArgstring(argstring, priorBracketLevel + 1, index + 1, startOfArg)
        case ')' =>  splitArgstring(argstring, priorBracketLevel - 1, index + 1, startOfArg)
	case ',' =>  if (priorBracketLevel == 0) { 
	  argstring.substring(startOfArg, index).replaceAll("""\s+""", "") :: 
	  splitArgstring(argstring, priorBracketLevel, index + 1, index + 1)
	  } else { splitArgstring(argstring, priorBracketLevel, index + 1, startOfArg) }
	case _ => splitArgstring(argstring, priorBracketLevel, index + 1, startOfArg)
      }
    }
  }

  // Note that we assume only a single negation symbol, no repeated negation
  // as we assume the formula is in CNF
  def separateLiteralAndNegation(literalString:String) :(String,Boolean) = {
    negationRegex findFirstIn literalString match {
    case Some(negationRegex(pL)) => (pL, true)
    case None => (literalString, false)
    }
  }

  def negateStringLiteral(literalString:String) : String = {
    val (posL, lIsNegated) = separateLiteralAndNegation(literalString)
      if (lIsNegated) { posL }
      else { "-" + posL }
  }
}


//----------------------------------------------------------
trait VariableFindAndReplace {
  // distinguishing variables from constants/function symbols:
  // variables start with an uppercase letter
  def isVariable(term:String) :Boolean = { term(0).isUpper }

  // taking apart a term by splitting on "word boundaries"
  // this will split at commas and parentheses.
  // Note that sequences of parentheses will stay as they are
  def takeTermApart(term:String) :List[String] = {
    term.split("""\s*\b\s*""").toList.filter (s => s != "")
  }

  // occurs check: does the given variable occur in the given term?
  def occursCheck(variable:String, term:String) :Boolean = {
    takeTermApart(term).contains(variable)
  }

  def applySubstitution(term:String, substitution:Substitution) :String = {
    takeTermApart(term).map (subterm =>
      if (substitution.contains(subterm)) { substitution(subterm) }
      else { subterm }).mkString("")
  }
}

//----------------------------------------------------------
// compute a substitution using the naive recursive approach of Baader
trait Unification extends TakeLiteralApart with VariableFindAndReplace {

  // add a pair x |-> term to a substitution,
  // and at the same time apply the new substitutions to all right-hand sides 
  // in the existing substitution
  def addSubstitution( t1:String, t2:String, substitution:Substitution) :Substitution = {
    val t1ToT2:Substitution = newSubstitutionInit(t1 -> t2)
    
    val pairs = substitution.map { case (lhs, rhs) => (lhs, applySubstitution(rhs, t1ToT2)) }
    
    t1ToT2 ++ pairs
  }

  def unifyTerms(t1O:String,t2O:String, substitution:Substitution) :Option[Substitution] = {
    val t1:String = applySubstitution(t1O, substitution)
    val t2:String = applySubstitution(t2O, substitution)
    
    if (isVariable(t1)) {
      if (t1 == t2) { new Some(substitution) }
      else if (occursCheck(t1, t2)) { None }
      else {
	new Some(addSubstitution(t1, t2, substitution))
      }
    } else if (isVariable(t2)) {
      if (occursCheck(t2, t1)) { None }
      else {
	new Some(addSubstitution(t2, t1, substitution))
      }
    } else {
      val (pred1, args1) = separatePredArg(t1)
      val (pred2, args2) = separatePredArg(t2)
      if (pred1 == pred2 && args1.size == args2.size) {
	// Make pairs of matching arguments from 1st and 2ns term,
	// then iterate through them starting with an Option on an empty substitution.
	// Each successive term pair adds to the substitution, or changes the Option to None.
	// Once it is changed to None, it stays None.
	args1.zip(args2).foldLeft(new Some(substitution):Option[Substitution]) { (optsubs, tpair) => 
	  optsubs.flatMap (subs => unifyTerms(tpair._1, tpair._2, subs)) }
      } else { None }
    }
  }
    
  // given two lists of terms, compute a substitution
  // that unifies them.
  def computeSubstitution(l1:List[String], l2:List[String]) : Option[Substitution] = { 
    if (l1.size != l2.size) {
      throw new Exception("error in computeSubstitution: term lists differ in length " + l1.toString + " " + l2.toString)
    }

    // pair up terms in l1 with terms in l2, 
    // and try to unify them.
    // unifyTerms returns Option[Substitution].
    // if we got None for some term pair, just carry the None to the end of the foldLeft.
    // Otherwise, each call to unifyTerms starts with the substitution that the 
    // previous call generated.
    l1.zip(l2).foldLeft(new Some(newSubstitution):Option[Substitution]) { (optsubs, tpair) => 
	  optsubs.flatMap (subs => unifyTerms(tpair._1, tpair._2, subs)) }
  }
}


//----------------------------------------------------------
// literal class
// keeps the literal as a string,
// the polarity of the literal (isNegated),
// and the predicate and list of arguments as individual strings
class Literal(val originalLiteralString: String) extends Unification {

  // posLiteralString is the literal as string, without negation.
  // isNegated is true if literalString is negated.
  val (posLiteralString, isNegated) : (String, Boolean) = separateLiteralAndNegation(originalLiteralString)

  var (predSymbol, argList) = separatePredArg(posLiteralString)

  // arity: number of arguments
  def arity :Int = argList.size

  // content literal:
  // This is specific to Johan Bos' Boxer system, 
  // which uses Neo-Davidsonian semantics!
  // A content literal has only one argument, and its predicate is not 'event'
  def isContent : Boolean = {
    arity == 1 && predSymbol != "event"
  }

  // grounded literal: does not contain any variables
  // convert to string to get the string variant of the current form of this literal,
  // then break the term apart at "word boundaries" to get all terms and variables at all embedding depths,
  // then test that none of those pieces is a variable.
  def isGrounded : Boolean = {
    takeTermApart(this.toString).forall ( piece => !(isVariable(piece)))
  }

  // possibleMatch with other literal:
  // if they differ in negation, but have the same predicate symbol and arity
  def possibleMatch(otherL:Literal) :Boolean = {
    otherL.isNegated != isNegated &&
    otherL.predSymbol == predSymbol &&
    otherL.arity == arity
  }

  // check if this literal unifies with another literal.
  // input: both literals' argument lists.
  // try this only if both literals have the same predicate symbols and arity,
  // and if one of them is negated and the other is not.
  def unifiableAndNegated(otherL:Literal) : Option[Substitution] = {
    if (possibleMatch(otherL)) { computeSubstitution(argList, otherL.argList) }
    else { None }
  }

  // Two literals are the same if they have the same predicate and arguments.
  // Don't use literalString as a basis, as that can have extra whitespace
  override def equals(that:Any) :Boolean = {
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

  def applySubstitution(substitution:Substitution) : Unit = {
    argList = argList.map( a => applySubstitution(a, substitution))
  }

  // for inspection: represent this class as the literal string
  override def toString :String = (if (isNegated) "-" else "") + 
  predSymbol + 
  (if (argList.size > 0) "(" + argList.mkString(",") + ")"
   else "") 
}

//----------------------------------------------------------
// Clause class
// keeps a disjunction of literals as a set.
class Clause(val stringlist: List[String]) {
  // convert list of strings to mutable ListBuffer of literals
  var literals :Set[Literal] = (stringlist.map (litString => new Literal(litString))).toSet
  cleanup

  // size of the clause: the number of its literals
  def size :Int = literals.size

  // tests for singleton and empty clauses
  def isSingleton :Boolean = { size == 1 }
  def isEmpty :Boolean = { size == 0 }

  // clean up literals: 
  // collapse duplicates
  // remove pairs of literals that are inverses of each other.
  def cleanup : Unit = {
    this.literals = this.literals.toList.toSet.filter ( l => 
      !(this.literals.exists(l2 => l.isNegationOf(l2))))

    if (isEmpty) {
      throw new EmptyClauseException(this.toString)
    }
  }

  def applySubstitution(substitution:Substitution) : Unit = { 
    this.literals.foreach ( l => l.applySubstitution(substitution))
    // if some literals have become equal through substitution,
    // we need to convert to a list and back to a set in order to remove duplicates
    this.literals = this.literals.toList.toSet
    cleanup
  }

  // for inspection: represent this class as the list of literals
  override def toString :String = { "Clause(" + this.literals.toString + ")"}

  def toList :List[String] = this.literals.map (l => l.toString).toList
}

//----------------------------------------------------------
// class CNF:
// keep a formula as a mapping from indices to Clause objects.
class CNF(val listlist: List[List[String]]) {
  // mutable map that maps indice to clauses.
  // mutable because some clauses may be deleted
  var clauses : mutable.Map[Int,Clause] = listlist.zipWithIndex.map { case (clauselist, index) => 
								    (index, new Clause(clauselist)) }(collection.breakOut)

  // clauseIndices: set of indices in the clauses map
  def clauseIndices :Set[Int] = clauses.keySet

  // uniqueUnificationMatch: check if the given literal
  // is only unifiable with a single literal in this CNF formula.
  // If this is the case, return a triple of 
  // (clause Index, literal in this CNF that is matched, substitution)
  // otherwise return None.
  // Note that matching requires the literals to be of opposite polarity.
  def uniqueUnificationMatch(literal:Literal) : Option[(Int,Literal,Substitution)] = {
    val matchingLiterals : List[(Int,Literal,Substitution)] = clauseIndices.toList.flatMap {
      // map each clause index to a list of tuples (clauseIx, literal, substitution)
      // for all the matching literals in that clause
      clauseIx => clauses(clauseIx).literals.flatMap {
	// map each literal to either Some(clauseIx, literal, substitution) or None
	thisLiteral => thisLiteral.unifiableAndNegated(literal) match {
	  case Some(substitution) => new Some(clauseIx, thisLiteral, substitution)
	  case None => None
	}
      }
    }
    if (matchingLiterals.size == 1) {
      // single matching literal.
      new Some(matchingLiterals(0))
    } else { None }
  }

  // groundedNegationMatch:
  // check whether the given literal is grounded, and the current formula contains a literal 
  // that is the exact negation of that literal
  def groundedNegationMatch(literal1:Literal) :Option[(Int, Literal)] = {
    if (!literal1.isGrounded) { None }
    else {
      clauseIndices.toList.foldLeft(None:Option[(Int, Literal)]) ( (prevResult, clauseIx) => 
	prevResult match {
	    // if we have previously found a matching clause, no need to look further
	  case Some(_) => prevResult
	    // if we haven't found anything yet, check this clause
	  case None => clauses(clauseIx).literals.toList.find( literal2 => literal2.isNegationOf(literal1)) match {
	    // did we find some literal now? if yes, then it is the next result to be passed on
	    case Some(literal2) => new Some((clauseIx, literal2))
	      // if we didn't find anything, then pass on None
	    case None => None
	  }
	}
      )
    }
  }

  // clauseIx is a singleton clause that has just been involved in a resolution step,
  // so this clause disappears. Remove it from the "clauses" map,
  // then apply the substitution to all remaining clauses.
  def resolveAsSingleton(clauseIx:Int, substitution:Substitution) : Unit = {
    // raise an error if we don't have this clause
    if (!this.clauses.contains(clauseIx)) {
      throw new Exception("Shouldn't be here: trying to resolve nonexisting clause as singleton " + clauseIx.toString)
    }
    this.clauses -= clauseIx
    applySubstitution(substitution)
 }

  // clauseIx is a clause that has just been involved in a resolution step with a singleton
  // targeting the literal 'literal' occurring in clause 'clauseIx'. Remove 'literal'
  // from the clause 'clauseIx'. (There will be no new literals added to 'clauseIx' because
  // the other clause was a singleton.)
  // Apply the substitution to all clauses. 
  def resolveWithSingleton(clauseIx:Int, literal:Literal, substitution:Substitution) : Unit = { 
    if (!this.clauses.contains(clauseIx)) {
      throw new Exception("Shouldn't be here: trying to resolve nonexisting clause as singleton " + clauseIx.toString)
    }
    if (!this.clauses(clauseIx).literals.contains(literal)) {
      throw new Exception("Shouldn't be here: trying to resolve with nonexisting literal " + literal.toString)
    }

    clauses(clauseIx).literals = clauses(clauseIx).literals - literal
    applySubstitution(substitution)
  }

  // apply substitution:
  // apply the given substitution to every literal of every clause of this formula
  def applySubstitution(substitution:Substitution) : Unit = { 
    this.clauses.valuesIterator.foreach ( c => c.applySubstitution(substitution))
  }

  // for inspection: represent this class as the literal string
  override def toString :String = this.clauses.valuesIterator.map (c => c.toString ).mkString(" & ")

  def toListList :List[List[String]] = this.clauses.valuesIterator.map (c => c.toList ).toList

}

//----------------------------------------------------------
class InferenceRule(lhsCNF:CNF, rhsCNF:CNF) {

  def cnfToLitList(f:CNF) : List[Literal] = {
    // both lhs and rhs CNFs have to consist of singleton clauses only
    // we are not handling anything else for now
    if (f.clauses.valuesIterator.exists( clause => !(clause.isSingleton)))
      throw new UnexpectedFormatOfFormulaException("formula not flat " + f)

    // map f to a flat list of literals
    val litListUnsorted :List[Literal] = f.clauses.values.map (clause => clause.literals.head).toList

    // map a literal to its negation status and predicate symbol
    def negAndPredSymbol(lit:Literal) :String = {
      (if (lit.isNegated) "-" else "") + lit.predSymbol
    }

    // check if we have any predicates that occur more than once. those will go
    // all the way to the end of the list of sorted literals
    val predCount :Map[String,Int] = litListUnsorted.foldLeft(Map():Map[String,Int]) ((pcmap, lit) =>
      pcmap updated (negAndPredSymbol(lit), pcmap.getOrElse(negAndPredSymbol(lit), 0) + 1))

    // convert the CNF to a list of literals sorted alphabetically by negation and predicate symbols.
    // Literals whose predicate symbol occurs more than once appear all the way at the end
    litListUnsorted.sortWith((l1, l2) => {
      val l1ps = negAndPredSymbol(l1)
      val l2ps = negAndPredSymbol(l2)
      val l1pc = predCount.getOrElse(l1ps, 0)
      val l2pc = predCount.getOrElse(l2ps, 0)
      if (l1pc != l2pc) { l1pc < l2pc }
      else { l1ps < l2ps }
    })
  }
    
  // left-hand side of the rule (with old variable names, to be changed below),
  // sorted alphabetically by predicate symbols
  val lhsOld :List[Literal] = cnfToLitList(lhsCNF)
  val rhsOld :List[Literal] = cnfToLitList(rhsCNF)

  val constantVariableRegex = new Regex("""^[A-Za-z][A-Za-z0-9_]*$""")

  // make a list of all the constants and variables in both the lhs and RHS CNF,
  // remove all occurrences of a duplicate except the first,
  // and pair each constant or variable with a string
  // Xsi
  // for s = l/r for left-hand or right-hand side 
  //and i the position in the list of constants and variables
  def mapArgumentsToNewVariables(f:List[Literal], formulaLabel:String, 
				 exceptArguments:Set[String] = Set()) :Map[String,String] = {

    val argListNodup :List[String] = f.flatMap( literal => 
      // extract list of arguments
      literal.argList ).filter ( arg => 
      // remove arguments thar are in the exceptArguments list
      !exceptArguments.contains(arg)).foldLeft(List():List[String])((arglist,arg) =>
      // remove entries that have occurred earlier in the list
      if (arglist.contains(arg)) { arglist } else {arg :: arglist}).reverse

    // sanity check: must be all constants or variables, no function symbols
    argListNodup.foreach { arg => arg match {
	// if we match the constant & variable regex, we are good
      case constantVariableRegex() => {}
	// if we don't match it, we have an error
      case somethingElse => throw new UnexpectedFormatOfFormulaException(arg)
    }}

    // pair each constant or variable with a string Xsi,
    // and transform to a map 
    argListNodup.zipWithIndex.map{ case (oldArg, oldArgIndex) =>
				   (oldArg, "X" + formulaLabel + oldArgIndex.toString) 
				 }(collection.breakOut).toMap
  }

  // create a mapping from lhs constants and variables to new variable names
  val lhsArgumentMap :Map[String,String] = mapArgumentsToNewVariables(lhsOld, "l")
  // create a mapping from rhs constants and variables to new variable names,
  // remove constants and variables that already occur in the lhs
  val rhsArgumentMap :Map[String,String] = mapArgumentsToNewVariables(rhsOld, "r",
								      lhsArgumentMap.keySet)
  val combinedMap :Map[String,String] = lhsArgumentMap ++ rhsArgumentMap

  def literalToStringWithNewArguments(literal:Literal, newArgMap: Map[String,String]) :String = {
    (if (literal.isNegated) { "-" } else { "" }) + literal.predSymbol + 
    (if (literal.arity > 0) { "(" + literal.argList.map(arg => newArgMap.getOrElse(arg, "XXX")).mkString(",") + ")" } 
     else { "" } )
  }

  override def toString :String = {
    // forall X,Y,Z exists A,B,C
    quantifiers.map (q => q._1 + " " + q._2.mkString(",")).mkString(" ") + ": \n" + 
    // LHS
    lhs.mkString(" & ") + 
    " -> \n" + 
    // RHS
    rhs.mkString(" & ")
  }

  // ===============================
  // now starting with the parts of this class to be used from outside
  // quantifiers:
  // a list of pairs of a string ("forall", "exists") and a set of variables that have this scope
  // The variables introduced in the LHS are universally quantified and non-nested.
  // The variables introduced in the RHS, if any, are existentially quantified and nested below the universal quantifiers
  val quantifiers :List[(String,Set[String])] = 
    if (rhsArgumentMap.size > 0) 
      { List(("forall", lhsArgumentMap.values.toSet), ("exists", rhsArgumentMap.values.toSet)) }
    else { List(("forall", lhsArgumentMap.values.toSet)) }

  // lhs is a conjunction of literals, realized as a list of strings.
  // negation is realized as "-".
  val lhs :List[String] = lhsOld.map (lit => literalToStringWithNewArguments(lit, lhsArgumentMap))

  // rhs is a conjunction of literals, realized as a list of strings.
  // negation is realized as "-".
  val rhs :List[String] = rhsOld.map (lit => literalToStringWithNewArguments(lit, combinedMap))

  // equality test:
  // It's enough to just compare the lhs and the rhs formulas.
  // We don't need to compare the quantifiers, as every variable
  // that has an "l" in its name (is introduced in the LHS) is universally quantified,
  // and every variable that has an "r" in its name (is introduced in the RHS)
  // is existentially quantified. So you could in principle read the quantifier off the formula.
  override def equals(that:Any) :Boolean = {
    that.isInstanceOf[InferenceRule] && 
    lhs == that.asInstanceOf[InferenceRule].lhs && 
    rhs == that.asInstanceOf[InferenceRule].rhs
  }
}

//----------------------------------------------------------
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
      doResolution(formula1, formula2) match { 
	case Some((res1, res2)) => 
	  // resolution succeeded.
	  // negate formula 2.
	  val res2Neg:CNF = negateFormula(res2)
	  // then form 2 inference rules, one inferring -res2 from res1
	  // and one inferring res1 from -res2
	  new Some((new InferenceRule(res1, res2Neg), new InferenceRule(res2Neg, res1)))
	  
	case None => None
      }
    } catch {
	// if we found the formulas too complex to process, return None
      case e:UnexpectedFormatOfFormulaException => None
    }
  }

  def doResolution(formula1:CNF, formula2:CNF) :Option[(CNF, CNF)] = {
    // In a first step, do resolution on literal L1 of clause C1 and L2 of C2 only 
    // if 
    // * either C1 or C2 is a singleton clause,
    // * both L1 and L2 are "content" rather than meta-predicates,
    //   where meta-predicates are predicates like agent(X, Y) or patient(X, Y)
    //   or event(X) that stem from a Neo-Davidsonian representation
    // * L2 is the only literal that L1 unifies with in formula 1, and vice versa -- 
    //   or L1 and L2 are fully grounded, and one is the negation of the other

    val isContentLiteral = (l:Literal) => l.isContent

    try { 
      singletonClauseIndices(formula1).foreach ( ix => 
	singletonClauseUnificationStep(formula1, formula2, ix, isContentLiteral)
      )

      singletonClauseIndices(formula2).foreach ( ix => 
	singletonClauseUnificationStep(formula2, formula1, ix, isContentLiteral)
      )

      // Now do resolution on L1 of C1 and L2 of C2 only if 
      // * either C1 or C2 is singleton, 
      // * L2 is the only formula-2 literal that L1 unifies with, and vice versa --
      //   or L1 and L2 are fully grounded, and one is the negation of the other
      // * either L1, L2 are content literals, or they are fully grounded
      // run this while there is change in either formula.
      
      val isContentOrGrounded = (l:Literal) => l.isContent || l.isGrounded
      
      var change:Boolean = false
      do {
	change = 
	  singletonClauseIndices(formula1).exists ( ix => 
	    singletonClauseUnificationStep(formula1, formula2, ix, isContentOrGrounded)) || 
	singletonClauseIndices(formula2).exists ( ix =>
	    singletonClauseUnificationStep(formula2, formula1, ix, isContentOrGrounded))
      } while (change)
    } catch {
      case e:EmptyClauseException => return None
    }
      
    new Some(formula1, formula2)
  }

  // invert the second of the two formulas.
  // currently only works when we have a single clause
  def negateFormula(f:CNF) :CNF = {
    if (f.clauses.size > 1) {
      throw new UnexpectedFormatOfFormulaException(f.toString)
    }
    
    val newclauses :List[List[String]] = f.clauses(0).literals.toList.map ( oldL =>
      List(negateStringLiteral(oldL.toString))
    )

    new CNF(newclauses)
  }

  // filter for singleton clause indices in formula f
  def singletonClauseIndices( f:CNF) :List[Int] = {
    f.clauseIndices.toList.filter ( ix => f.clauses(ix).isSingleton )
  }

  // given a literal in a formula f1, find if it has a usable match in the given formula f2:
  // either a literal that is the exact negation of the literal, and both are grounded,
  // or a unique unification match
  def usableMatch(literal1:Literal, f1:CNF, f2:CNF, literalOK: (Literal) => Boolean) :Option[(Int,Literal,Substitution)] = {
    if (!literalOK(literal1)) { None }
    f2.groundedNegationMatch(literal1) match {
	// matching literal2 found in f2 such that literal is the negation of literal2
	// and are both grounded.
	// return empty substitution, as we are unifying two grounded literals
      case Some((c2Index, literal2)) => new Some((c2Index, literal2, newSubstitution))
	// no groundedNegationMatch found: then check for unique unifying literal
      case None => 
	f2.uniqueUnificationMatch(literal1) match { 
	  // if there is one, check that it only unifies with this particular literal in f1
	  case Some((c2Index, literal2, substitution)) =>
	    if (!literalOK(literal2)) { None }
	    else {
	      f1.uniqueUnificationMatch(literal2) match {
		  // if we get a positive answer, we know that it is for literal1
		case Some(_) => new Some((c2Index, literal2, substitution))
		case None => None
	      }
	    }
	  case None => None
	}
    }
  }

  // given a singleton clause and a second formula,
  // try to find a unique unification match for the literal in the singleton clause
  // in the second formula, and do a resolution step if it works
  // returns true if a resolution step was done
  def singletonClauseUnificationStep( f1:CNF, f2:CNF, c1Index:Int, testLit: (Literal) => Boolean) :Boolean = {
    val literal1 :Literal = f1.clauses(c1Index).literals.head
    usableMatch(literal1, f1, f2, testLit) match {
	// if we have found a usable match for literal1 in the form of literal2
	// of clause c2Index of f2,
	// we can do the resolution step.
      case Some((c2Index, literal2, substitution)) =>
	f2.resolveWithSingleton(c2Index, literal2, substitution)
	f1.resolveAsSingleton(c1Index, substitution)
	true
      case None => false
    }
  }
}

