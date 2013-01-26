package trc1
import com.nicta.scoobi.Scoobi._

import scala.io.Source

object TrCorpus extends ScoobiApp {

  /** builds a rule-loading trie from the rules file.
    */
  def getRuleFinder(rulesPath:String) = {
    val rtc = new RuleTrieC
    //a lambda that takes a tuple containing a rule and an int
    //and calls into the rtc's add method
    val addRule = (rtc.addRule(_:Rule, _:Int)).tupled
    Source.fromFile(rulesPath)
      .getLines()
      .map(Rule.fromString(_))
      .zipWithIndex
      .foreach(addRule)
    rtc
  }

  def run() = {
    val rulesPath = args(0)
    val trie = getRuleFinder(rulesPath)
  }
}

