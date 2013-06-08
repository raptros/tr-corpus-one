package trc1
import com.nicta.scoobi.Scoobi._
import scala.collection.immutable.Map
import collection.immutable.TreeMap

/** A character-based prefix trie that pulls up rules for prefixes assumes all strings are already normalized properly. This is an immutable
  * structure; all transformations return new tries built off of this one.
  */
class RuleTrieC(val rulesHere:List[Int], val subs:TreeMap[Char, RuleTrieC]) {
  import scalaz.syntax.std.option._

  def this() {
    this(Nil, TreeMap.empty)
  }


  /** Finds the set of rules for the longest matching prefix of str.
    */
  def findRule(str:String):List[Int] = str.toList match {
    case Nil => rulesHere
    case c::rest => (subs get c) some { _ findRule rest.mkString } none { if (c.isLetter) Nil else rulesHere }
  }

  /**creates a new map of subtries by building off the already present one.*/
  def newSubs(at:Char, rest:List[Char], id:Int):TreeMap[Char, RuleTrieC] = {
    val toUpdate = subs.getOrElse(at, new RuleTrieC)
    val updated = toUpdate.addRuleMap(rest.mkString, id)
    subs + (at -> updated)
  }

  /**adds a rule to the trie.*/
  def addRuleMap(rule:String, id:Int):RuleTrieC = rule.toList match {
    case Nil => new RuleTrieC(id::rulesHere, subs)
    case c::rest => new RuleTrieC(rulesHere, newSubs(c, rest, id))
  }

  /** adds only the left side of the rule, removing the var string (@R@) */
  def addRule(rule:Rule):RuleTrieC = addRuleMap(Rules.stripVar(rule.lhs), rule.id)

    import scala.language.postfixOps
  /** finds rules for every suffix of the input sentence.
    * @param sentence a string, processed to be in the same format as the rules.
    * @return a list of rule identifiers.
    */
  def findAllRules(sentence:String):List[Int] = atWords(sentence) flatMap { findRule(_) } toList

  /** updates the treemap by recursively combining tries.*/
  def updateM(m:TreeMap[Char, RuleTrieC], pair:(Char, RuleTrieC)):TreeMap[Char, RuleTrieC] = {
    val rtc2 = (this.subs get pair._1) some { _ + pair._2 } none { pair._2 }
    m + (pair._1 -> rtc2)
  }

  /** creates a new trie by combining two tries - assumes they are at the same prefix level*/
  def +(rtc:RuleTrieC):RuleTrieC = {
    val newMap = (rtc.subs foldLeft this.subs) { updateM(_, _) }
    new RuleTrieC(this.rulesHere ++ rtc.rulesHere, newMap)
  }

  val words = """\b\b""".r
  /**turns a sentence into an iterator over strings where each string starts with the word after the first word in the string before.*/
   def atWords(s:String):Iterator[String] = for {
     tail <- (words split s).tails  //split at word boundaries
     ts = tail.mkString if (!ts.isEmpty && ts(0).isLetterOrDigit) //and filter out the ones that don't start at a word or are empty.
   } yield ts

}
