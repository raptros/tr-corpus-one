package trc1
import util.parsing.combinator._
import scala.util.matching.Regex

case class Rule(id:Int, lhs:String, rhs:String, weight:Double) {
  def stripVar(str:String):String = swapRule.replaceAllIn(str, "")
  val swapLeft = (swapRule findFirstIn lhs) nonEmpty
  val swapRight = (swapRule findFirstIn rhs) nonEmpty
  val swap = (swapLeft && !swapRight) || (!swapLeft && swapRight)
  def toRe(str:String) = {
    new Regex("^(.*)" + stripVar(str) + "(.*)$", "x", "y")
  }
  val lhsRE = toRe(lhs)
  //val rhsRE = toRe(rhs)
  val rhss = stripVar(rhs)
  /** applies this rule to a sentence producing a TranslatedSentence */
  def applyRule(orig:String):Option[TranslatedSentence] = (lhsRE findFirstMatchIn orig) map { m =>
    if (swap) {
      (m group "y") + rhss + (m group "x")
    } else {
      (m group "x") + rhss + (m group "y")
    }
  } map (TranslatedSentence(orig, _, id))
}

/*case class MatchedSentence(sentence:String, rules:List[Int]) {
  def combineFirst(ms:MatchedSentence):MatchedSentence = MatchedSentence(sentence, rules ++ ms.rules)
  def toString = "(" + sentence + "," + rules + ")"
}*/

case object MatchedSentenceExtractor extends JavaTokenParsers {
  def parseIt(l:String):Option[MatchedSentence] = {
    val parts = l.split(",List")
    val head = parts(0).tail
    val tail:Option[List[Int]] = parseAll(rules, parts(1).init) map (Some(_)) getOrElse(None)
    tail map (head -> _)
  }
  def rules:Parser[List[Int]] = "(" ~> repsep(int, ",") <~ ")" 
  def int = decimalNumber ^^ (_.toInt)
}

case class TranslatedSentence(orig:String, trans:String, ruleId:Int)
