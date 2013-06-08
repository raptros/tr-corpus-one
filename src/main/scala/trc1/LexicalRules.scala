package trc1
import util.parsing.combinator._
import scala.util.matching.Regex

case class Rule(id:Int, lhs:String, rhs:String, weight:Double)

object Rules {
  val swapRule = "@R@".r
  val ruleExtract = new Regex("^([^@]*)(@R@)?", "rule", "r")
  
  /**...*/
  def ruleFromString(line:String):Rule = {
    val parts = (line split '\t')
    Rule(parts(0).toInt, parts(1), parts(2), parts(3).toDouble)
  }

  def ruleToString(rule:Rule):String = f"${rule.id}\t${rule.lhs}\t${rule.rhs}\t${rule.weight}%f"

  def stripVar(str:String):String = swapRule.replaceAllIn(str, "")
}


/** Makes a rule appliable - i.e. it can be applied to a sentence to turn an instance of the rule's lhs into the rhs. */
class RuleApplier(val rule:Rule) {
  val id = rule.id
  val rhs = rule.rhs
  val lhs = rule.lhs
  val weight = rule.weight

  //regex stuff.
  val swapLeft:Boolean = (Rules.swapRule findFirstIn lhs).nonEmpty
  val swapRight:Boolean = (Rules.swapRule findFirstIn rhs).nonEmpty
  val swap:Boolean = (swapLeft && !swapRight) || (!swapLeft && swapRight)
  def toRe(str:String) = {
    new Regex("^(.*)" + Rules.stripVar(str) + "(.*)$", "x", "y")
  }
  val lhsRE = toRe(lhs)
  val rhss = Rules.stripVar(rhs)
  
  /** applies this rule to a sentence producing a TranslatedSentence */
  def apply(orig:String):Option[TranslatedSentence] = for {
    m <- (lhsRE findFirstMatchIn orig)
    m2 = if (swap) {
      (m group "y") + rhss + (m group "x")
    } else {
      (m group "x") + rhss + (m group "y")
    }
  } yield TranslatedSentence(orig, m2, lhs + "->" + rhs, id, weight)
}

//case class TranslatedSentence(orig:String, trans:String, ruleId:Int)

/** represents a sentence that's been transformed by lexical rules.*/
case class TranslatedSentence(orig:String, trans:String, rule:String, ruleId:Int, weight:Double)

//case class LeftTransformedSentence(orig:List[List[String]], trans:String, ruleId:Int, weight:Double)

//case class BothTransformedSentence(orig:List[List[String]], trans:List[List[String]], ruleId:Int, weight:Double)
