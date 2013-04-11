package trc1
import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.lib.Relational
import scala.io.Source


/** MaxTransform is meant to implement the complete pipeline for converting the lexical rules into FOL.
  *
  */
@EnhanceStrings
object MaxTransform extends ScoobiApp {
  /** the task pipeline */
  def run() = {
    val rulesPath = args(0)
    val batchPath = args(1)
    val outPath = args(2)
    //get the rules trie
    val dRules:DList[Rule]= fromTextFile(rulesPath) map (ruleFromString(_))
    val dRuleCount:DObject[Int] = dRules.size
    //match sentences
    val matched = matchSents(dRules, batchPath)
    //regroup the sentences 
    val sentsByRule = regroupMatched(matched)
    //apply the rules to transform the sentences
    val translated = applyRules(dRules, sentsByRule)// groupBy (_.orig)
    //now create FOL pairs from the translated sentences
    val folPairs = applyFOLExtractor(translated)
    persist(toDelimitedTextFile(folPairs, outPath, mSep))
  }
  
  /** Finds all the rules that match each sentence.
   * See  trie code for why this stuff works.
   */
  def matchSents(dRules:DList[Rule], sentsPath:String):DList[MatchedSentence] = {
    val dtrie = dRules.map(r => (new RuleTrieC).addRule(r)).reduce(_+_)
    val lines:DList[String] = fromTextFile(sentsPath)
    (dtrie join lines) map { 
      case (trie, line) => (line, trie.findAllRules(line.toLowerCase))
    }
  }

  /**reorganize the matched sentences so that every rule has a list of matching sentences.*/
  def regroupMatched(matched:DList[MatchedSentence]):DList[(Int, List[String])] = {
    val withRules:DList[(Int, String)] = matched flatMap {sPair => sPair._2 map (_ -> sPair._1)}
    val grouped:DList[(Int, Iterable[(Int, String)])] = withRules groupBy (_._1)
    grouped map {rPair => (rPair._1 -> rPair._2.map(_._2).toList)}
  }

  /** apply rules by matchin up the ids in rule list with the ids of the sentence lists.*/
  def applyRules(dRules:DList[Rule], sents:DList[(Int, List[String])]):DList[TranslatedSentence] = {
    val indexedRules:DList[(Int, Rule)] = dRules map (r => r.id -> r)
    val joint:DList[(Int, (Rule, List[String]))] = Relational.join(indexedRules, sents)
    joint flatMap {
      case (id, (rule, sents)) => applySingleRule(rule, sents)
    }
  }

  /** flatmaps a rule over a list of sentences to get transformed versions.*/
  def applySingleRule(rule:Rule, sents:List[String]):List[TranslatedSentence] = {
    val applier = new RuleApplier(rule)
    sents flatMap (applier(_))
  }

  /** converts sentences in the pairs into FOL formulae.*/
  def applyFOLExtractor(translateds:DList[TranslatedSentence]):DList[FOLPair] = {
    translateds flatMap (ts2fp(_))
  }

  /** convert to FOL */
  def ts2fp(ts:TranslatedSentence):Option[FOLPair] = for {
    oFOL <- ConvertWithMSEM(ts.orig)
    tFOL <- ConvertWithMSEM(ts.trans)
  } yield FOLPair(ts.orig, ts.trans, oFOL, tFOL, ts.rule, ts.ruleId)

      /*
    for {
    p <- translateds
    val (orig, tss) = p
    oFOL <- ConvertToFOL(orig).toIterable
    ts <- tss
    tFOL <- ConvertToFOL(ts.trans)
  } yield FOLPair(oFOL, tFOL, ts.ruleId)*/
}



//the rest of these are obsolete.

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

