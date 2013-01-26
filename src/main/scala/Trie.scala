package trc1
import scala.collection.mutable.{Map => MMap}
import com.nicta.scoobi.Scoobi._

/** A character-based prefix trie that pulls up rules for prefixes
  * assumes all strings are already normalized.
  * 
  */
case class RuleTrieC(var rulesHere:List[Int], var subs:Map[Char, RuleTrieC]) {
  def this() = {
    this(Nil, Map.empty)
  }

  /** Finds the set of rules for the longest matching prefix of str.
    */
  def findRule(str:String):List[Int] = str.toList match {
    case Nil => rulesHere
    case c::rest => (subs get c) map (_ findRule rest.mkString) getOrElse(rulesHere)
  }

  def newSub(at:Char) = {
    val newTrie = new RuleTrieC
    subs = subs + (at -> newTrie)
    newTrie
  }

  def addRuleMap(rule:String, id:Int):Unit = rule.toList match {
    case Nil => rulesHere = id::rulesHere
    case c::rest => subs.getOrElse(c, newSub(c)).addRuleMap(rest.mkString, id)
  }

  def stripVar(str:String):String = """@R@""".r.replaceAllIn(str, "")

  /** adds both the left and right sides of a Rule, removing the var string (@R@) */
  def addRule(rule:Rule, id:Int):Unit = {
    (rule.lhs::rule.rhs::Nil) map (stripVar(_)) foreach (addRuleMap(_, id))
  }

  /** finds rules for every suffix of the input sentence.
    * @param sentence a string, processed to be in the same format as the rules.
    * @return a list of rule identifiers.
    */
  def findAllRules(sentence:String):List[Int] = {
    sentence.tails flatMap(findRule(_)) toList
  }
}
