package trc1
import util.parsing.combinator._

case class Rule(id:Int, lhs:String, rhs:String, weight:Double) {
  /** applies this rule to a sentence producing a TranslatedSentence */
  def applyRule(orig:String):Option[TranslatedSentence] = Some(TranslatedSentence(orig, orig, id))
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
