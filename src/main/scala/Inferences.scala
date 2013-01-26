package trc1

case class Rule(lhs:String, rhs:String, weight:Double)

object Rule {
  def fromString(line:String) = {
    val parts = line.split('\t')
    Rule(parts(0), parts(1), parts(2).toDouble)
  }
}

case class MatchedSentence(sentence:String, rules:List[Int])
