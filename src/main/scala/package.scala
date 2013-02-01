package object trc1 {
  import com.nicta.scoobi.Scoobi._
  import java.io.{DataInput, DataOutput}
  implicit val ruleFmt:WireFormat[Rule] = mkCaseWireFormat(Rule, Rule.unapply _)
  //implicit val trieFmt:WireFormat[RuleTrieC] = mkCaseWireFormat(RuleTrieC, RuleTrieC.unapply _)
  implicit def trieFmt:WireFormat[RuleTrieC] = new WireFormat[RuleTrieC] {
    def toWire(rtc:RuleTrieC, out:DataOutput) = {
      TraversableFmt[List, Int].toWire(rtc.rulesHere, out)
      MapFmt[Map, Char, RuleTrieC].toWire(rtc.subs, out)
    }

    def fromWire(in:DataInput):RuleTrieC = {
      val rulesHere = TraversableFmt[List, Int].fromWire(in)
      val subs = MapFmt[Map, Char, RuleTrieC].fromWire(in)
      RuleTrieC(rulesHere, subs)
    }

    def show(rtc:RuleTrieC):String = rtc.toString

  }

  implicit val matchedSentFmt:WireFormat[MatchedSentence] = mkCaseWireFormat(MatchedSentence, MatchedSentence.unapply _)

  def ruleFromString(line:String):Rule = {
    val parts = line.split('\t')
    Rule(parts(0).toInt, parts(1), parts(2), parts(3).toDouble)
  }
}
