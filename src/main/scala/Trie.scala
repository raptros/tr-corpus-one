package trc1
import com.nicta.scoobi.Scoobi._
import scala.collection.immutable.Map
import collection.immutable.TreeMap

/** A character-based prefix trie that pulls up rules for prefixes
  * assumes all strings are already normalized.
  * 
  */
case class RuleTrieC(rulesHere:List[Int], subs:TreeMap[Char, RuleTrieC]) {
  def this() = {
    this(Nil, TreeMap.empty)
  }


  /** Finds the set of rules for the longest matching prefix of str.
    */
  def findRule(str:String):List[Int] = str.toList match {
    case Nil => rulesHere
    case c::rest => (subs get c)
      .map(_ findRule rest.mkString)
      .getOrElse(if (c.isLetter) Nil else rulesHere)
  }

  def newSubs(at:Char, rest:List[Char], id:Int):TreeMap[Char, RuleTrieC] = {
    val toUpdate = subs.getOrElse(at, new RuleTrieC)
    val updated = toUpdate.addRuleMap(rest.mkString, id)
    subs + (at -> updated)
  }

  def addRuleMap(rule:String, id:Int):RuleTrieC = rule.toList match {
    case Nil => RuleTrieC(id::rulesHere, subs)
    case c::rest => RuleTrieC(rulesHere, newSubs(c, rest, id))
  }

  /** adds only the left side of the rule, removing the var string (@R@) */
  def addRule(rule:Rule):RuleTrieC = {
    addRuleMap(stripVar(rule.lhs), rule.id)
  }

  /** finds rules for every suffix of the input sentence.
    * @param sentence a string, processed to be in the same format as the rules.
    * @return a list of rule identifiers.
    */
  def findAllRules(sentence:String):List[Int] = {
    atWords(sentence) flatMap(findRule(_)) toList
  }
  def updateM(m:TreeMap[Char, RuleTrieC], pair:(Char, RuleTrieC)):TreeMap[Char, RuleTrieC] = {
    val rtc2 = (this.subs get pair._1) map (_ + pair._2) getOrElse (pair._2)
    m + (pair._1 -> rtc2)
  }

  def +(rtc:RuleTrieC):RuleTrieC = {
    val newMap = rtc.subs.foldLeft(this.subs)(updateM(_, _))
    new RuleTrieC(this.rulesHere ++ rtc.rulesHere, newMap)
  }
}

/*
object RuleTrieC {
  def apply(rule:Rule, id:Int) = {
    (new RuleTrieC).addRule(rule, id)
  }
}*/
