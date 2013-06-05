package trc1.resolution

import scala.util.matching.Regex

import scalaz._
import std.list._
import std.option._
import std.string._
import syntax.monoid._
import optionSyntax._
import syntax.id._
import syntax.std.boolean._
import Validation._

/** Given a literal (without negation), separate predicate and arguments and return them as a pair (predicate, argList) predicate is a
  * string, and argList is a list of strings
  *
  * Given a possibly negated literal, take it apart into positive literal and potential negation
  */
trait TakeLiteralApart {
  // two regular expressions that take apart predicate and arguments
  val predArgRegex = new Regex("""^(.*?)\((.*?)\)$""", "pred", "argstring")
  val atomRegex = new Regex("""^([^(),]*)$""", "atom")
  // identify negated literal
  val negationRegex = new Regex("""^\-(.*?)$""", "pL")

  /** use the regular expressions to take apart literals and detect atoms */
  def separatePredArg(literalString: String):Pair[String, List[String]] = {
    val oArg = (predArgRegex findFirstMatchIn literalString) map { m => 
      (m group "pred") -> splitArgstring(m group "argstring") // do we have a predicate with arguments?
    }
    val oAtom = (atomRegex findFirstMatchIn literalString) map { m => 
      (m group "atom") -> List.empty[String] //or an atom?
    }
    (oArg orElse oAtom) getOrElse { 
      throw new UnexpectedFormatOfFormulaException("expected atom or literal, got " + literalString)
    }
  }

  /** splits a string that represents a sequence of comma-separated arguments; doesn't split where a comma is embedded in an argument, as
    * in f(X, Y)
    */
  def splitArgstring(argstring:String, priorBracketLevel:Int = 0, index:Int = 0, startOfArg:Int = 0): List[String] = if (index >= argstring.size) {
    List(argstring.substring(startOfArg).replaceAll("""\s+""", ""))
  } else {
    splitArgstringMatcher(argstring, priorBracketLevel, index, startOfArg)
  }
    
  def splitArgstringMatcher(argstring:String, priorBracketLevel:Int, index:Int, startOfArg:Int):List[String] = (argstring charAt index) match {
    case '(' =>  splitArgstring(argstring, priorBracketLevel + 1, index + 1, startOfArg)
    case ')' =>  splitArgstring(argstring, priorBracketLevel - 1, index + 1, startOfArg)
    case ',' =>  if (priorBracketLevel == 0) { 
      argstring.substring(startOfArg, index).replaceAll("""\s+""", "")::splitArgstring(argstring, priorBracketLevel, index + 1, index + 1)
    } else { 
      splitArgstring(argstring, priorBracketLevel, index + 1, startOfArg) 
    }
    case _ => splitArgstring(argstring, priorBracketLevel, index + 1, startOfArg)
  }
  

  /** Note that we assume only a single negation symbol, no repeated negation as we assume the formula is in CNF */
  def separateLiteralAndNegation(literalString:String):(String,Boolean) = (negationRegex findFirstMatchIn literalString) map { m =>
    (m group "pL") -> true
  } getOrElse { literalString -> false }

  def negateStringLiteral(literalString:String):String = {
    val (posL, lIsNegated) = separateLiteralAndNegation(literalString)
    (!lIsNegated) ?? "-" |+| posL
  }
}

/** finds and replaces variables */
trait VariableFindAndReplace {
  /** distinguishing variables from constants/function symbols: variables start with an uppercase letter */
  def isVariable(term:String):Boolean = term(0).isUpper 

  /** taking apart a term by splitting on "word boundaries" this will split at commas and parentheses.  Note that sequences of parentheses
    * will stay as they are
    */
  def takeTermApart(term:String):List[String] = (term split """\s*\b\s*""").toList filterNot { _.isEmpty }

  /** occurs check: does the given variable occur in the given term? */
  def occursCheck(variable:String, term:String):Boolean = takeTermApart(term) contains variable

  def applySubstitution(term:String, substitution:Substitution):String = takeTermApart(term) map { subterm =>
      if (substitution contains subterm) substitution(subterm) else subterm
  } mkString ""
}

/** compute a substitution using the naive recursive approach of Baader */
trait Unification extends TakeLiteralApart with VariableFindAndReplace {

  /** add a pair x |-> term to a substitution, and at the same time apply the new substitutions to all right-hand sides in the existing
    * substitution
    */
  def addSubstitution(t1:String, t2:String, substitution:Substitution):Substitution = {
    val t1ToT2:Substitution = Map(t1 -> t2)
    t1ToT2 ++ (substitution mapValues { applySubstitution(_, t1ToT2) })
  }

  def unifyTerms(t1O:String,t2O:String, substitution:Substitution):Option[Substitution] = {
    val t1:String = applySubstitution(t1O, substitution)
    val t2:String = applySubstitution(t2O, substitution)
    
    if (isVariable(t1)) {
      if (t1 == t2) some(substitution) else {
        (!occursCheck(t1, t2)) option { addSubstitution(t1, t2, substitution) }
      }
    } else if (isVariable(t2)) {
      (!occursCheck(t2, t1)) option { addSubstitution(t2, t1, substitution) }
    } else {
      val (pred1, args1) = separatePredArg(t1)
      val (pred2, args2) = separatePredArg(t2)
      (pred1 == pred2 && args1.size == args2.size) ?? computeSubstitution1(args1, args2, substitution)
    }
  }
    
  /** given two lists of terms, compute a substitution that unifies them.*/
  def computeSubstitution(l1:List[String], l2:List[String]):Option[Substitution] = if (l1.size != l2.size) {
    throw new Exception("error in computeSubstitution: term lists differ in length " + l1.toString + " " + l2.toString)
  } else computeSubstitution1(l1, l2)

  type OSub = Option[Substitution]

  import State._
  import syntax.state._
  import Traverse._
  import syntax.traverse._
  import syntax.monad._

  /** computes a new substitution by traversing the paired arguments lists - each argument pair will either add to the substitution, or
    * wipe out the substitution for the rest of the traversal.
    * @param l1 the first list of arguments
    * @param l2 the second list
    * @param substitution the substitution to add to in computing, empty by default
    * @return the substitution optionally computed by the traversal
    * @note this assumes that l1 and l2 are the same length.
    */
  def computeSubstitution1(l1:List[String], l2:List[String], substitution:Substitution=newSubstitution):Option[Substitution] = {
    val ut = (p:(String,String)) => (os:OSub) => os flatMap { unifyTerms(p._1, p._2, _) }
    (l1 zip l2) traverseS { p => modify(ut(p)) } exec { some(substitution) }
  }
} 
