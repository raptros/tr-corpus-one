
import scala.collection.mutable.{Map => MMap}

/** A character-based prefix trie that pulls up rules for prefixes
  * assumes all strings are already normalized.
  */
class RuleTrieC {
  var ruleHere:Option[Int] = None

  val subs = MMap.empty[Char, RuleTrieC]

  def findRule(str:String):Option[Int] = str.toList match {
    case Nil => ruleHere
    case c::rest => (subs get c) flatMap (_ findRule rest.mkString)
  }

  def addRuleMap(rule:String, id:Int):Unit = rule.toList match {
    case Nil => ruleHere = Some(id)
    case c::rest => subs.getOrElseUpdate(c, new RuleTrieC).addRuleMap(rest.mkString, id)
  }
}
