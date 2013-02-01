package trc1

case class Rule(id:Int, lhs:String, rhs:String, weight:Double)

case class MatchedSentence(sentence:String, rules:List[Int])
