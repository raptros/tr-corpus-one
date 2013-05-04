package trc1
import util.parsing.combinator._
import scala.util.matching.Regex

case class Rule(id:Int, lhs:String, rhs:String, weight:Double)

/** Makes a rule appliable - i.e. it can be applied to a sentence to turn an instance of the rule's lhs into the rhs. */
class RuleApplier(val rule:Rule) {
  val id = rule.id
  val rhs = rule.rhs
  val lhs = rule.lhs
  val weight = rule.weight

  //regex stuff.
  val swapLeft:Boolean = (swapRule findFirstIn lhs).nonEmpty
  val swapRight:Boolean = (swapRule findFirstIn rhs).nonEmpty
  val swap:Boolean = (swapLeft && !swapRight) || (!swapLeft && swapRight)
  def toRe(str:String) = {
    new Regex("^(.*)" + stripVar(str) + "(.*)$", "x", "y")
  }
  val lhsRE = toRe(lhs)
  val rhss = stripVar(rhs)
  
  /** applies this rule to a sentence producing a TranslatedSentence */
  def apply(orig:String):Option[TranslatedSentence] = (lhsRE findFirstMatchIn orig) map { m =>
    if (swap) {
      (m group "y") + rhss + (m group "x")
    } else {
      (m group "x") + rhss + (m group "y")
    }
  } map (TranslatedSentence(orig, _, lhs + "->" + rhs, id, weight))

}

/** represents a sentence that's been transformed by lexical rules.*/
//case class TranslatedSentence(orig:String, trans:String, ruleId:Int)
case class TranslatedSentence(orig:String, trans:String, rule:String, ruleId:Int, weight:Double)

case class LeftTransformedSentence(orig:List[List[String]], trans:String, ruleId:Int, weight:Double)

case class BothTransformedSentence(orig:List[List[String]], trans:List[List[String]], ruleId:Int, weight:Double)
