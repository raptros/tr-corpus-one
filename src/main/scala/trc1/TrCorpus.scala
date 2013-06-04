package trc1
import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.core.{Reduction, Association1, Grouped}
import com.nicta.scoobi.lib.Relational
import scala.io.Source
import logic.{ConvertToCNF,FolContainer}
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import resolution.{Resolution, InferenceRuleFinal, finalizeInference, compIRFs}
import scala.util.Properties
import scalaz.syntax.std.option._

/** MaxTransform implements the complete pipeline for converting the lexical rules into FOL. */
object MaxTransform extends ScoobiApp {
  /** the task pipeline */
  def run() {
    val rulesPath = args(0)
    val batchPath = args(1)
    val outPath = args(2)
    //check that GetFOL will be able to run.
    val candcBasePath = Properties.envOrNone("CANDC_HOME").err("CANDC_HOME must be set in order for GetFOL to work!")
    (new GetFOL(candcBasePath)).checkPaths()
    //get the rules 
    val dRules:DList[Rule]= fromTextFile(rulesPath) map { ruleFromString(_) }
    //match sentences
    val sents = loadSents(batchPath)
    val matched = matchSents(dRules, sents)
    //regroup the sentences 
    val sentsByRule = regroupMatched(matched)
    //apply the rules to transform the sentences
    val translated = applyRules(dRules, sentsByRule)
    //now convert to fol and apply resolution to get rules
    val infRules = getRules(candcBasePath, translated)
    //group the extracted rules and combine the groups
    val combined = combineIRFHs(infRules)
    //and save them
    val strings = combined map { IRFHolders.toString(_) }
    strings.toTextFile(outPath).persist
  }

  def loadSents(sentsPath:String):DList[String] = fromTextFile(sentsPath) filter { l => l.split(' ').length < 50 }
  
  /** Finds all the rules that match each sentence. See  trie code for why this stuff works. */
  def matchSents(dRules:DList[Rule], lines:DList[String]):DList[(String, List[Int])] = {
    val dtrie = dRules map { (new RuleTrieC).addRule(_) } reduce { Reduction(_ + _) }
    for { (trie, line) <- dtrie join lines } yield line -> (trie findAllRules line.toLowerCase)
  }

  /** reorganize the matched sentences so that every rule has a list of matching sentences.*/
  def regroupMatched(matched:DList[(String, List[Int])]):DList[(Int, List[String])] = {
    val withRules:DList[(Int, String)] = matched mapFlatten { sPair => sPair._2 map { _ -> sPair._1 } }
    val grouped:DList[(Int, Iterable[(Int, String)])] = withRules groupBy { _._1 }
    grouped map { rPair => rPair._1 -> (rPair._2.toList map { _._2 }) }
  }

  /** apply rules by matchin up the ids in rule list with the ids of the sentence lists.*/
  def applyRules(dRules:DList[Rule], sents:DList[(Int, List[String])]):DList[TranslatedSentence] = {
    val indexedRules:Relational[Int, Rule] = Relational(dRules map { r => r.id -> r })
    val joint = indexedRules join sents
    joint mapFlatten {
      case (id, (rule, sents)) => applySingleRule(rule, sents)
    }
  }

  /** flatmaps a rule over a list of sentences to get transformed versions.*/
  def applySingleRule(rule:Rule, sents:List[String]):List[TranslatedSentence] = {
    val applier = new RuleApplier(rule)
    sents flatMap { applier(_) }
  }

  /** extracts rules from the sentence pairs */
  /*def getRules(translateds:DList[TranslatedSentence]):DList[IRFHolder] = translateds groupBy { _.orig } mapFlatten { 
    case (o, tss) => convertToRule(o, tss)
  }*/

  /** extracts rules from the sentence pairs */
  def getRules(candcBasePath:String, translateds:DList[TranslatedSentence]):DList[IRFHolder] = {
    val grouped = translateds groupBy { _.orig }
    val withPath = DObject(candcBasePath) join grouped
    withPath mapFlatten {
      case (path, (orig, tss)) => extractAll(new GetFOL(path), orig, tss)
    }
  }

  def extractAll(getFOL:GetFOL, orig:String, tss:Iterable[TranslatedSentence]) = for {
    origCNF <- getCNF(getFOL, orig, "1").toIterable
    TranslatedSentence(_, trans, _, ruleId, weight) <- tss
    transCNF <- getCNF(getFOL, trans, "2", true)
    (fLeft, _) <- Resolution.resolveToFindDifference(origCNF, transCNF)
  } yield RuleTypeChange.bringIRF(fLeft, ruleId, weight)

  import scalaz.Validation._
  import scala.language.postfixOps
  
  /** gets FOL by calling C&C and boxer, then converts that into CNF and gets list-of-list form */
  def getCNF(getFOL:GetFOL, sent:String, id:String, negate:Boolean=false):Option[List[List[String]]] = for {
    fol <- getFOL(sent)
    cnf <- ConvertToCNF(if (negate) -fol else fol) { _ + id }
    lists <- fromTryCatch { FolContainer.cnfToLists(cnf) } leftMap { (t:Throwable) => println("failed lists conv on " + sent); t } toOption
  } yield lists

  /** groups the inference rule holders keyed upon the left and right hand sides of the rule itself, then combines the rule holders to tally
    * up the generation counts
    */
  def combineIRFHs(irfhs:DList[IRFHolder]):DList[IRFHolder] = irfhs groupBy { 
    IRFHolders.toKey(_)
  } combine { 
    Reduction(IRFHolders.combine(_:IRFHolder, _:IRFHolder)) 
  } values
}

/** preproccesses rules by seperating ones that convert between single content words from more complex rules */
object RemoveSingleContentWordTFRules extends ScoobiApp {
  /** expects 4 args 
    * @param stopsPath an input file with a single stopword on each line
    * @param rulesPath an input file containing rules in the proper format
    * @param folOutPath the ouput path for the single-content-word rules in FOL format
    * @param leftOutPath the output path for the rules not removed by this tool
    */
  def run() {
    val stopsPath = args(0)
    val rulesPath = args(1)
    val folOutPath = args(2)
    val leftOutPath = args(3)
    //load up a set of stopwords
    val stops:DObject[Set[String]] = fromTextFile(stopsPath) map { Set(_) } reduce Reduction { _ ++ _ }
    //load rules into a dlist
    val dRules:DList[Rule]= fromTextFile(rulesPath) map { ruleFromString(_) }
    //apply predicate to rules to break into two dlists
    val (scwtfs, leftovers) = breakRules(stops, dRules)
    //convert the first one into a dlist of FOLs
    val fRules = scwtfs map { s => FolRules toString mkSCWTFFOL(s) }
    val leftStrings = leftovers map { ruleToString(_) }
    persist(fRules.toTextFile(folOutPath), leftStrings.toTextFile(leftOutPath))
  }

  /** Returns a pair of rule sets by splitting them on the isSCWTFRule predicate. */
  def breakRules(stops:DObject[Set[String]], dRules:DList[Rule]):(DList[Rule], DList[Rule]) = {
    val (in, out) = (stops join dRules) partition { (isSCWTFRule(_,_)).tupled }
    (in map { _._2 }, out map { _._2 })
  }
  
  /** Determines if a string contains only a single content word. */
  def isSingleContentWord(stops:Set[String], side:String):Boolean = ((side split ' ').length == 1) && !(stops contains stripVar(side))

  /** Determines if both sides of a rule only contain single content words. */
  def isSCWTFRule(stops:Set[String], rule:Rule):Boolean = isSingleContentWord(stops, rule.lhs) && isSingleContentWord(stops, rule.rhs)

  /** Converts a single-content-word string into an FOL expression */
  def prepSide(side:String):FolExpression = {
    val cleaned = stripVar(side).replace("-", "C45").trim
    FolVariableExpression(Variable(cleaned))
  }

  /** converts a single-content-word Rule into an FOL rule */
  def mkSCWTFFOL(rule:Rule):FolRule = {
    val v = Variable("X")
    val fve = FolVariableExpression(v)
    val l = prepSide(rule.lhs) applyto fve
    val r = prepSide(rule.rhs) applyto fve
    val fRule = (l -> r) all v
    FolRule(fRule, List(rule.id), List(rule.weight), 1)
  }
}
