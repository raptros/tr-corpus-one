package object trc1 {
  import com.nicta.scoobi.Scoobi._
  import java.io.{DataInput, DataOutput}
  import collection.immutable.TreeMap
  import utcompling.scalalogic.fol.expression._
  import utcompling.scalalogic.top.expression.Variable
  import logic.{BoxerFOLParser, BoxerFolFormat}
  import resolution.InferenceRuleFinal

  implicit val ruleFmt:WireFormat[Rule] = mkCaseWireFormat(Rule, Rule.unapply _)
  implicit val translatedFmt:WireFormat[TranslatedSentence] = mkCaseWireFormat(TranslatedSentence, TranslatedSentence.unapply _)
  implicit val leftTFFmt:WireFormat[LeftTransformedSentence] = mkCaseWireFormat(LeftTransformedSentence, LeftTransformedSentence.unapply _)
  implicit val bothTFFmt:WireFormat[BothTransformedSentence] = mkCaseWireFormat(BothTransformedSentence, BothTransformedSentence.unapply _)
  //implicit val folPairFMT:WireFormat[FOLPair] = mkCaseWireFormat(FOLPair, FOLPair.unapply _)
  implicit val folRuleFmt:WireFormat[FolRule] = mkCaseWireFormat(FolRule, FolRule.unapply _)
  implicit val irfFmt:WireFormat[InferenceRuleFinal] = mkCaseWireFormat(InferenceRuleFinal, InferenceRuleFinal.unapply _)
  implicit val irfHolderFmt:WireFormat[IRFHolder] = mkCaseWireFormat(IRFHolder, IRFHolder.unapply _)

  /** wire format for FOLs based on the string format conversion*/
  implicit def folEFmt:WireFormat[FolExpression] = new WireFormat[FolExpression] {
    def toWire(fole:FolExpression, out:DataOutput) = {
      StringFmt.toWire(fole.toBoxerFolFormat, out)
    }

    def fromWire(in:DataInput):FolExpression = {
      val inStr = StringFmt.fromWire(in)
      BoxerFOLParser.extractFol(inStr).get
    }
    def show(fole:FolExpression):String = fole.toBoxerFolFormat
  }
  //implicit val trieFmt:WireFormat[RuleTrieC] = mkCaseWireFormat(RuleTrieC, RuleTrieC.unapply _)

  import collection.generic.CanBuildFrom

  /*implicit def TreeMapFmt[CC[X, Y] <: TreeMap[X, Y], K, V](implicit wtK: WireFormat[K], wtV: WireFormat[V], bf: CanBuildFrom[_, (K, V),
    CC[K, V]]):WireFormat[CC[K, V]] = new TraversableFmt(bf())*/

  /** creating a wireformat for a recursive data structure turns out to be less than straightforward.*/
  implicit def trieFmt:WireFormat[RuleTrieC] = new WireFormat[RuleTrieC] {
    def toWire(rtc:RuleTrieC, out:DataOutput) = {
      TraversableFmt[List, Int].write(rtc.rulesHere, out)
      TraversableFmt[List, (Char, RuleTrieC)].write(rtc.subs.toList, out)
    }

    def fromWire(in:DataInput):RuleTrieC = {
      val rulesHere = TraversableFmt[List, Int].read(in)
      val subsT = TraversableFmt[List, (Char, RuleTrieC)].read(in)
      val subs = (subsT foldLeft TreeMap.empty[Char, RuleTrieC]) { _ + _ }
      new RuleTrieC(rulesHere, subs)
    }

    def show(rtc:RuleTrieC):String = rtc.toString

  }

  import scala.util.matching.Regex
  val swapRule = "@R@".r
  val ruleExtract = new Regex("^([^@]*)(@R@)?", "rule", "r")
  

  //implicit val matchedSentFmt:WireFormat[MatchedSentence] = mkCaseWireFormat(MatchedSentence, MatchedSentence.unapply _)
  type MatchedSentence = (String, List[Int])

  /**...*/
  def ruleFromString(line:String):Rule = {
    val parts = (line split '\t')
    Rule(parts(0).toInt, parts(1), parts(2), parts(3).toDouble)
  }

  def ruleToString(rule:Rule):String = rule match {
    case Rule(id, lhs, rhs, weight) => List(id.toString, lhs, rhs, weight.toString).mkString("\t")
  }

  /*val CHARS_LOW=' '.intValue
  val NUM_CHARS=('z'.intValue) - CHARS_LOW
  def getCharPos[A](char:Char, array:Array[A]):Option[A] = if (char.intValue < CHARS_LOW) None 
  else if (char.intValue > NUM_CHARS + CHARS_LOW) None 
  else array(char.intValue - CHARS_LOW)*/
  
  val words = """\b\b""".r
  /**turns a sentence into an iterator over strings where each string starts with the word after the first word in the string before.*/
   def atWords(s:String):Iterator[String] = for {
     tail <- (words split s).tails  //split at word boundaries
     ts = tail.mkString if (!ts.isEmpty && ts(0).isLetterOrDigit) //and filter out the ones that don't start at a word or are empty.
   } yield ts

  def stripVar(str:String):String = swapRule.replaceAllIn(str, "")


  val mSep = "<<<@>>>"
}
