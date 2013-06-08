package trc1
import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.core.Reduction
import com.nicta.scoobi.lib.Relational
import scala.io.Source
import scalaz.syntax.std.option._

import scala.math.Ordering

import logic.cnf.{ConvertToCNF, FolContainer}
import logic.fol
import logic.top.Variable



/** preproccesses rules by seperating ones that convert between single content words from more complex rules */
object RemoveSingleContentWordTFRules extends ScoobiApp {
  import ImplicitFormats._

  /** expects 4 args 
    * @param stopsPath an input file with a single stopword on each line
    * @param rulesPath an input file containing rules in the proper format
    * @param folOutPath the ouput path for the single-content-word rules in FOL format
    * @param leftOutPath the output path for the rules not removed by this tool
    */
  def run() {
    val stopsPath = args(0)
    val rulesPath = args(1)
    val folOutPath = args(2)
    val leftOutPath = args(3)
    //load up a set of stopwords
    val stops:DObject[Set[String]] = fromTextFile(stopsPath) map { Set(_) } reduce Reduction { _ ++ _ }
    //load rules into a dlist
    val dRules:DList[Rule]= fromTextFile(rulesPath) map { Rules.ruleFromString(_) }
    //apply predicate to rules to break into two dlists
    val (scwtfs, leftovers) = breakRules(stops, dRules)
    //convert the first one into a dlist of FOLs
    val fRules = scwtfs map { s => FolRules toString mkSCWTFFOL(s) }
    val leftStrings = leftovers map { Rules.ruleToString(_) }
    persist(fRules.toTextFile(folOutPath), leftStrings.toTextFile(leftOutPath))
  }

  /** Returns a pair of rule sets by splitting them on the isSCWTFRule predicate. */
  def breakRules(stops:DObject[Set[String]], dRules:DList[Rule]):(DList[Rule], DList[Rule]) = {
    val (in, out) = (stops join dRules) partition { (isSCWTFRule(_,_)).tupled }
    (in map { _._2 }, out map { _._2 })
  }
  
  /** Determines if a string contains only a single content word. */
  def isSingleContentWord(stops:Set[String], side:String):Boolean = ((side split ' ').length == 1) && !(stops contains Rules.stripVar(side))

  /** Determines if both sides of a rule only contain single content words. */
  def isSCWTFRule(stops:Set[String], rule:Rule):Boolean = isSingleContentWord(stops, rule.lhs) && isSingleContentWord(stops, rule.rhs)

  /** Converts a single-content-word string into an FOL expression */
  def prepSide(side:String):fol.Expr = {
    val cleaned = Rules.stripVar(side).replace("-", "C45").trim
    fol.VariableExpr(Variable(cleaned))
  }

  /** converts a single-content-word Rule into an FOL rule */
  def mkSCWTFFOL(rule:Rule):FolRule = {
    val v = Variable("X")
    val fve = fol.VariableExpr(v)
    val l = prepSide(rule.lhs) applyto fve
    val r = prepSide(rule.rhs) applyto fve
    val fRule = (l -> r) all v
    FolRule(fRule, List(rule.id), List(rule.weight), 1)
  }
}
