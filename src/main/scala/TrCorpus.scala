package trc1
import com.nicta.scoobi.Scoobi._

import scala.io.Source

@EnhanceStrings
object FindRules extends ScoobiApp {

  /** builds a rule-loading trie from the rules file.
    */
  def getRuleFinder(rulesPath:String) = {
    //val rtc = new RuleTrieC
    //a lambda that takes a tuple containing a rule and an int
    //and calls into the rtc's add method
    //val addRule = (rtc.addRule(_:Rule, _:Int)).tupled
    Source.fromFile(rulesPath)
      .getLines()
      .map(ruleFromString(_))
      .map((new RuleTrieC).addRule(_))
      .reduce(_+_)
  }

  def run() = {
    val rulesPath = args(0)
    val sentencesDir = args(1)
    val outFile = args(2)
    println("rulesPath: #rulesPath; sentencesDir: #sentencesDir; outFile: #outFile")
    //val trie = getRuleFinder(rulesPath)
    //println(trie)
    
    val rules:DList[String] = fromTextFile(rulesPath)
    val rulesObjs = rules map (ruleFromString(_))
    val dtries:DList[RuleTrieC] = rulesObjs map {
      r => (new RuleTrieC).addRule(r)
    }
    val dtrie:DObject[RuleTrieC] = dtries.reduce(_+_)

    //val dtrie = DObject(trie)
    val lines:DList[String] = fromTextFile(sentencesDir)
    val foundRules:DList[(String, List[Int])] = (dtrie join lines) map {
      case (trie, line) => (line, trie.findAllRules(line.toLowerCase))
    }
    persist(toTextFile(foundRules, outFile))
  }
}


@EnhanceStrings
object Recombine extends ScoobiApp {
  def run() = {
    val allSentsPath = args(0)
    val outPath = args(1)
    val allSents:DList[String] = fromTextFile(allSentsPath)
    val allMS:DList[MatchedSentence] = allSents flatMap (MatchedSentenceExtractor.parseIt(_))
    val grouped:DList[(String, Iterable[(String, List[Int])])] = allMS.groupBy(_._1)
    val combined:DList[MatchedSentence] = grouped.combine {
      (a:MatchedSentence, b:MatchedSentence) => (a._1, a._2 ++ b._2)
    }.map(p => (p._1, p._2._2))
    persist(toTextFile(combined, outPath))
  }
}
