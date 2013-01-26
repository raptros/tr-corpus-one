package trc1
import scala.collection.mutable.{Map => MMap}
import com.nicta.scoobi.Scoobi._

/** A character-based prefix trie that pulls up rules for prefixes
  * assumes all strings are already normalized.
  * 
  */
case class RuleTrieC(rulesHere:List[Int], subs:Map[Char, RuleTrieC]) {
  def this() = {
    this(Nil, Map.empty)
  }

  /** Finds the set of rules for the longest matching prefix of str.
    */
  def findRule(str:String):List[Int] = str.toList match {
    case Nil => rulesHere
    case c::rest => (subs get c) map (_ findRule rest.mkString) getOrElse(rulesHere)
  }

  def newSubs(at:Char, rest:List[Char], id:Int):Map[Char, RuleTrieC] = {
    val toUpdate = subs.getOrElse(at, new RuleTrieC)
    val updated = toUpdate.addRuleMap(rest.mkString, id)
    subs + (at -> updated)
  }

  def addRuleMap(rule:String, id:Int):RuleTrieC = rule.toList match {
    case Nil => RuleTrieC(id::rulesHere, subs)
    case c::rest => RuleTrieC(rulesHere, newSubs(c, rest, id))
  }

  def stripVar(str:String):String = """@R@""".r.replaceAllIn(str, "")

  /** adds both the left and right sides of a Rule, removing the var string (@R@) */
  def addRule(rule:Rule, id:Int):RuleTrieC = {
    addRuleMap(stripVar(rule.lhs), id).addRuleMap(stripVar(rule.rhs), id)
  }

  /** finds rules for every suffix of the input sentence.
    * @param sentence a string, processed to be in the same format as the rules.
    * @return a list of rule identifiers.
    */
  def findAllRules(sentence:String):List[Int] = {
    sentence.tails flatMap(findRule(_)) toList
  }
}
