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

@EnhanceStrings
object ApplyRules extends ScoobiApp {
  def loadRules(rulesPath:String):Array[Rule] = {
    Source.fromFile(rulesPath)
    .getLines()
    .map(ruleFromString(_))
    .toArray
  }
  
  def translate(pair:(Array[Rule], MatchedSentence)):List[TranslatedSentence] = pair match {
    case (rules, (sent, rls)) => rls flatMap (rl => (rules lift rl) flatMap (_.applyRule(sent)))
  }

  def run() = {
    val rulesPath = args(0)
    val matchedPath = args(1)
    val outPath = args(2)
    println("rulesPath: #rulesPath; matchedPath: #matchedPath; outPath: #outPath")
    val dRules:DObject[Array[Rule]] = DObject(loadRules(rulesPath))
    val matchedSentences:DList[MatchedSentence] = fromTextFile(matchedPath) flatMap (MatchedSentenceExtractor.parseIt(_))
    val translated:DList[TranslatedSentence] = (dRules join matchedSentences) flatMap (translate(_))
    persist(toDelimitedTextFile(translated, outPath, mSep))
  }
}

@EnhanceStrings
object CountRules extends ScoobiApp {
  def run() = {
    val inPath = args(0)
    val matchedSentences:DList[MatchedSentence] = fromTextFile(inPath) flatMap (MatchedSentenceExtractor.parseIt(_))
    val freqs:DList[(String, Int)] = matchedSentences.flatMap(_._2).map(_.toString -> 1).groupByKey.combine(_+_)
    val min:DObject[(String, Int)] = freqs.minBy(_._2)(Ordering.Int)
    val max:DObject[(String, Int)] = freqs.maxBy(_._2)(Ordering.Int)
    val count:DObject[Int] = freqs.size
    val total:DObject[Int] = freqs.map(_._2).sum
    val (lMin, lMax, lCount:Int, lTotal:Int) = persist(min, max, count, total)
    val lAvg = lTotal.toDouble / lCount
    println("min: #lMin, max: #lMax, avg: #lAvg")
  }
}

@EnhanceStrings
object MaxTransform extends ScoobiApp {
  def run() = {
    val rulesPath = args(0)
    val batchPath = args(1)
    val outPath = args(2)
    //get the rules trie
    val dtrie = getTrie(rulesPath)
    //match sentences
    val matched = matchSents(dtrie, batchPath)
    //load rules
    val dRules:DObject[Map[Int, Rule]] = loadRules(rulesPath)
    val ruleCount:Int = persist(dRules map (_ size))
    //apply the rules to transform the sentences
    val translated = applyRules(dRules, matched)
    persist(toDelimitedTextFile(translated, outPath, mSep))
    //group by rule
    val transByRule:DList[(Int, List[TranslatedSentence])] = translated.groupBy(_.ruleId) map (p => p._1 -> p._2.toList)
    val freqs:DList[(Int, Int)] = transByRule map (p => p._1 -> p._2.size)
    persist(toTextFile(freqs, outPath + "-freqs"))
    //now report the occurances at this stage
    val count:DObject[Int] = transByRule.size
    val min:DObject[(Int, Int)] = freqs.minBy(_._2)(Ordering.Int)
    val max:DObject[(Int, Int)] = freqs.maxBy(_._2)(Ordering.Int)
    val totalApplications:DObject[Int] = freqs.map(_._2).sum
    val (lCount, lMin, lMax, lTotal) = persist(count, min, max, totalApplications)
    val lAvg = lTotal.toDouble / lCount
    val missing = ruleCount - lCount
    println("count: #lCount, missing: #missing, min: #lMin, max: #lMax, avg: #lAvg")

  }

  def getTrie(rulesPath:String):DObject[RuleTrieC] = {
    fromTextFile(rulesPath)
    .map(ruleFromString(_))
    .map(r => (new RuleTrieC).addRule(r))
    .reduce(_+_)
  }
  
  def matchSents(dtrie:DObject[RuleTrieC], sentsPath:String):DList[MatchedSentence] = {
    val lines:DList[String] = fromTextFile(sentsPath)
    (dtrie join lines) map {
      case (trie, line) => (line, trie.findAllRules(line.toLowerCase))
    }
  }
  
  def applyRules(dRules:DObject[Map[Int, Rule]], matched:DList[MatchedSentence]):DList[TranslatedSentence] = {
    (dRules join matched) flatMap (lookupAndTranslate(_))
  }

  def loadRules(rulesPath:String):DObject[Map[Int, Rule]] = fromTextFile(rulesPath) 
  .map(ruleFromString(_)) //to rules
  .map(r => Map(r.id -> r)) //to maps
  .reduce(_ ++ _) //combine maps

  def lookupAndTranslate(pair:(Map[Int, Rule], MatchedSentence)):List[TranslatedSentence] = pair match {
    case (rules, (sent, rls)) => rls flatMap (rl => (rules get rl) flatMap (_.applyRule(sent)))
  }

}
