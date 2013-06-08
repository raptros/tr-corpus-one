package trc1

object ImplicitFormats {
  import com.nicta.scoobi.Scoobi._
  import java.io.{DataInput, DataOutput}
  import collection.immutable.TreeMap

  import logic.fol.{Expr => FExpr}
  import logic.top.Variable
  import logic.parsing.{BoxerFOLParser, BoxerFolFormat}
  import logic.resolution.InferenceRuleFinal

  implicit val ruleFmt:WireFormat[Rule] = mkCaseWireFormat(Rule, Rule.unapply _)
  implicit val translatedFmt:WireFormat[TranslatedSentence] = mkCaseWireFormat(TranslatedSentence, TranslatedSentence.unapply _)
  //implicit val leftTFFmt:WireFormat[LeftTransformedSentence] = mkCaseWireFormat(LeftTransformedSentence, LeftTransformedSentence.unapply _)
  //implicit val bothTFFmt:WireFormat[BothTransformedSentence] = mkCaseWireFormat(BothTransformedSentence, BothTransformedSentence.unapply _)
  implicit val folRuleFmt:WireFormat[FolRule] = mkCaseWireFormat(FolRule, FolRule.unapply _)
  implicit val irfFmt:WireFormat[InferenceRuleFinal] = mkCaseWireFormat(InferenceRuleFinal, InferenceRuleFinal.unapply _)
  implicit val irfHolderFmt:WireFormat[IRFHolder] = mkCaseWireFormat(IRFHolder, IRFHolder.unapply _)

  /** wire format for FOLs based on the string format conversion*/
  implicit def folEFmt:WireFormat[FExpr] = new WireFormat[FExpr] {
    def toWire(fole:FExpr, out:DataOutput) = {
      StringFmt.toWire(fole.toBoxerFolFormat, out)
    }

    def fromWire(in:DataInput):FExpr = {
      val inStr = StringFmt.fromWire(in)
      BoxerFOLParser.extractFol(inStr).get
    }
    def show(fole:FExpr):String = fole.toBoxerFolFormat
  }

  /** creating a wireformat for a recursive data structure turns out to be less than straightforward.*/
  implicit def trieFmt:WireFormat[RuleTrieC] = new WireFormat[RuleTrieC] {
    def toWire(rtc:RuleTrieC, out:DataOutput) = {
      TraversableFmt[List, Int].write(rtc.rulesHere, out)
      MapFmt[TreeMap, Char, RuleTrieC].write(rtc.subs, out)
    }

    def fromWire(in:DataInput):RuleTrieC = {
      val rulesHere = TraversableFmt[List, Int].read(in)
      val subs = MapFmt[TreeMap, Char, RuleTrieC].read(in)
      new RuleTrieC(rulesHere, subs)
    }

    def show(rtc:RuleTrieC):String = rtc.toString
  }
}
