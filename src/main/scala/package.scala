package object trc1 {
  import com.nicta.scoobi.Scoobi._
  import java.io.{DataInput, DataOutput}
  import collection.immutable.TreeMap
  implicit val ruleFmt:WireFormat[Rule] = mkCaseWireFormat(Rule, Rule.unapply _)
  implicit val translatedFmt:WireFormat[TranslatedSentence] = mkCaseWireFormat(TranslatedSentence, TranslatedSentence.unapply _)
  implicit val folPairFMT:WireFormat[FOLPair] = mkCaseWireFormat(FOLPair, FOLPair.unapply _)
  //implicit val trieFmt:WireFormat[RuleTrieC] = mkCaseWireFormat(RuleTrieC, RuleTrieC.unapply _)

  //implicit def TreeMapFmt[CC[X, Y] <: TreeMap[X, Y], K, V](implicit wtK: WireFormat[K], wtV: WireFormat[V], bf: CanBuildFrom[_, (K, V), CC[K, V]]):WireFormat[CC[K, V]] = new TraversableWireFormat(bf())

  /** creating a wireformat for a recursive data structure turns out to be less than straightforward.*/
  implicit def trieFmt:WireFormat[RuleTrieC] = new WireFormat[RuleTrieC] {
    def toWire(rtc:RuleTrieC, out:DataOutput) = {
      TraversableFmt[List, Int].toWire(rtc.rulesHere, out)
      MapFmt[TreeMap, Char, RuleTrieC].toWire(rtc.subs, out)
    }

    def fromWire(in:DataInput):RuleTrieC = {
      val rulesHere = TraversableFmt[List, Int].fromWire(in)
      val subs = MapFmt[TreeMap, Char, RuleTrieC].fromWire(in)
      RuleTrieC(rulesHere, subs)
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
    val parts = line.split('\t')
    Rule(parts(0).toInt, parts(1), parts(2), parts(3).toDouble)
  }

  /*val CHARS_LOW=' '.intValue
  val NUM_CHARS=('z'.intValue) - CHARS_LOW
  def getCharPos[A](char:Char, array:Array[A]):Option[A] = if (char.intValue < CHARS_LOW) None 
  else if (char.intValue > NUM_CHARS + CHARS_LOW) None 
  else array(char.intValue - CHARS_LOW)*/
  
  val words = """\b\b""".r
  /**turns a sentence into a list of strings where each string starts with the word after the first word in the string before.*/
  def atWords(s:String):List[String] = {
    //split at word boundaries
    val splitted = (words split s) 
    val filtered = splitted.tails.map(_ mkString) //then turn each tailing into a single string
    .withFilter(!_.isEmpty).withFilter(_(0).isLetterOrDigit) //and filter out the ones that don't start at a word or are empty.
    filtered toList
  }

  def stripVar(str:String):String = swapRule.replaceAllIn(str, "")


  val mSep = "<<<@>>>"
}
