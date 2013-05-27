package trc1
import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.core.Reduction
import com.nicta.scoobi.lib.Relational
import scala.io.Source
import logic.{ConvertToCNF,FolContainer}
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import resolution.{Resolution, InferenceRuleFinal, finalizeInference, compIRFs}

/** MaxTransform is meant to implement the complete pipeline for converting the lexical rules into FOL.
  *
  */
object MaxTransform extends ScoobiApp {
  /** the task pipeline */
  def run() {
    val rulesPath = args(0)
    val batchPath = args(1)
    val outPath = args(2)
    //get the rules 
    val dRules:DList[Rule]= fromTextFile(rulesPath) map (ruleFromString(_))
    val dRuleCount:DObject[Int] = dRules.size
    //match sentences
    val sents = loadSents(batchPath)
    val matched = matchSents(dRules, sents)
    //regroup the sentences 
    val sentsByRule = regroupMatched(matched)
    //apply the rules to transform the sentences
    val translated = applyRules(dRules, sentsByRule)// groupBy (_.orig)
    //now convert to fol and apply resolution to get rules
    val infRules = getRules(translated)
    //group the extracted rules and combine the groups
    val combined = combineIRFHs(infRules)
    //and save them
    val strings = combined map (IRFHolders.toString(_))
    strings.toTextFile(outPath).persist
  }

  def loadSents(sentsPath:String):DList[String] = fromTextFile(sentsPath) filter { l => l.split(' ').length < 32 }
  
  /** Finds all the rules that match each sentence.
   * See  trie code for why this stuff works.
   */
  def matchSents(dRules:DList[Rule], lines:DList[String]):DList[MatchedSentence] = {
    val dtrie = dRules map (r => (new RuleTrieC).addRule(r)) reduce Reduction(_ + _)
    (dtrie join lines) map { 
      case (trie, line) => (line, trie.findAllRules(line.toLowerCase))
    }
  }

  /**reorganize the matched sentences so that every rule has a list of matching sentences.*/
  def regroupMatched(matched:DList[MatchedSentence]):DList[(Int, List[String])] = {
    val withRules:DList[(Int, String)] = matched mapFlatten {sPair => sPair._2 map (_ -> sPair._1)}
    val grouped:DList[(Int, Iterable[(Int, String)])] = withRules groupBy (_._1)
    grouped map { rPair => (rPair._1 -> rPair._2.map(_._2).toList) }
  }

  /** apply rules by matchin up the ids in rule list with the ids of the sentence lists.*/
  def applyRules(dRules:DList[Rule], sents:DList[(Int, List[String])]):DList[TranslatedSentence] = {
    val indexedRules:DList[(Int, Rule)] = dRules map (r => r.id -> r)
    val joint:DList[(Int, (Rule, List[String]))] = Relational.join(indexedRules, sents)
    joint mapFlatten {
      case (id, (rule, sents)) => applySingleRule(rule, sents)
    }
  }

  /** flatmaps a rule over a list of sentences to get transformed versions.*/
  def applySingleRule(rule:Rule, sents:List[String]):List[TranslatedSentence] = {
    val applier = new RuleApplier(rule)
    sents flatMap (applier(_))
  }

  /** converts sentences in the pairs into FOL formulae.*//*
  def getRules(translateds:DList[TranslatedSentence]):DList[IRFHolder] = {
    val tGrouped = translateds groupBy (_.orig)
    val leftTF = tGrouped flatMap {(convertLeft(_,_)).tupled}
    val bothTF = leftTF flatMap (convertRight(_))
    bothTF flatMap (extractRule(_))
  }*/
  def getRules(translateds:DList[TranslatedSentence]):DList[IRFHolder] = translateds groupBy (_.orig) mapFlatten convToRuleFunc
  
  def convertToRule(orig:String, tss:Iterable[TranslatedSentence]) = for {
    lts <- convertLeft(orig, tss)
    bts <- convertRight(lts)
    rule <- extractRule(bts)
  } yield rule

  val convToRuleFunc = (convertToRule(_:String, _:Iterable[TranslatedSentence])).tupled

  def convertLeft(orig:String, tss:Iterable[TranslatedSentence]):Iterable[LeftTransformedSentence] = for {
    oTF <- getCNF(orig, "1").toIterable
    TranslatedSentence(_, trans, _, id, weight) <- tss
  } yield LeftTransformedSentence(oTF, trans, id, weight)

  def convertRight(lts:LeftTransformedSentence):Option[BothTransformedSentence] = getCNF(lts.trans, "2", true) map { 
    tTF => BothTransformedSentence(lts.orig, tTF, lts.ruleId, lts.weight)
  }

  def extractRule(bts:BothTransformedSentence):Option[IRFHolder] = Resolution.resolveToFindDifference(bts.orig, bts.trans) map {
    case(fLeft, fRight) => RuleTypeChange.bringIRF(fLeft, bts.ruleId, bts.weight)
  }
  
  def getCNF(sent:String, id:String, negate:Boolean=false):Option[List[List[String]]] = for {
    fol <- GetFOL(sent)
    cnf <- ConvertToCNF(if (negate) -fol else fol)(_ + id)
    lists <- try { Some(FolContainer.cnfToLists(cnf)) } catch { case (t:Throwable) => {println("failed lists conv on " + sent); None}}
  } yield lists

  def fToString(fol:Any, cnf:Any, lists:List[List[String]]):String = {
    /*fol.toString + mSep + cnf.toString + mSep +*/ (lists map (l => l map (s => "\"" + s +"\""))).toString
  }

  /** combines a dlist of irfhs into smallest */
  def combineIRFHs(irfhs:DList[IRFHolder]):DList[IRFHolder] = {
    val grouped:DList[((String,String),Iterable[IRFHolder])] = irfhs.groupBy(irfh => (irfh.r.lhs.mkString("&"), irfh.r.rhs.mkString("&")))
    grouped.combine(Reduction(IRFHolders.combine(_:IRFHolder, _:IRFHolder))).values
  }
}

object RemoveSingleContentWordTFRules extends ScoobiApp {
  def run() {
    val stopsPath = args(0)
    val rulesPath = args(1)
    val folOutPath = args(2)
    val leftOutPath = args(3)
    //load up a set of stopwords
    val stops:DObject[Set[String]] = fromTextFile(stopsPath) map (Set(_)) reduce Reduction(_++_)
    //load rules into a dlist
    val dRules:DList[Rule]= fromTextFile(rulesPath) map (ruleFromString(_))
    //apply predicate to rules to break into two dlists
    val (scwtfs, leftovers) = breakRules(stops, dRules)
    //convert the first one into a dlist of FOLs
    val fRules = scwtfs map (mkSCWTFFOL(_)) map (FolRules.toString(_))
    val leftStrings = leftovers map (ruleToString(_))
    persist(fRules.toTextFile(folOutPath), leftStrings.toTextFile(leftOutPath))
  }

  def breakRules(stops:DObject[Set[String]], dRules:DList[Rule]):(DList[Rule], DList[Rule]) = {
    val (in, out) = (stops join dRules) partition ((isSCWTFRule(_,_)).tupled)
    (in map (_._2), out map (_._2))
  }
  def isSingleContentWord(stops:Set[String], side:String):Boolean = (side.split(' ').length == 1) && !(stops contains stripVar(side))
  def isSCWTFRule(stops:Set[String], rule:Rule):Boolean = isSingleContentWord(stops, rule.lhs) && isSingleContentWord(stops, rule.rhs)

  def prepSide(side:String):FolExpression = {
    val cleaned = stripVar(side).replace("-", "C45").trim
    FolVariableExpression(Variable(cleaned))
  }

  def mkSCWTFFOL(rule:Rule):FolRule = {
    val v = Variable("X")
    val fve = FolVariableExpression(v)
    val l = prepSide(rule.lhs).applyto(fve)
    val r = prepSide(rule.rhs).applyto(fve)
    val fRule = (l -> r) all(v)
    FolRule(fRule, List(rule.id), List(rule.weight), 1)
  }
}
