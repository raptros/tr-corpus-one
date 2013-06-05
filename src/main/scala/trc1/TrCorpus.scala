package trc1
import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.core.Reduction
import com.nicta.scoobi.lib.Relational
import scala.io.Source
import logic.{ConvertToCNF,FolContainer}
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import resolution.{Resolution, InferenceRuleFinal}
import scalaz.syntax.std.option._

import scala.math.Ordering

/** MaxTransform implements the complete pipeline for converting the lexical rules into FOL. */
object MaxTransform extends ScoobiApp {

  type LLCNF = List[List[String]]
  type PairedCNF = (LLCNF, LLCNF, Int, Double)
  type RePaired = (LLCNF, LLCNF, List[Int], List[Double], Int)

  /** the task pipeline */
  def run() {
    val rulesPath = args(0)
    val batchPath = args(1)
    val outPath = args(2)
    //check that GetFOL will be able to run.
    val candcBasePath = getEnv("CANDC_HOME").err("CANDC_HOME must be set in order for GetFOL to work!")
    (new GetFOL(candcBasePath)).checkPaths()
    uploadLibJarsFiles()
    val bytes = configuration.getBytesPerReducer
    configuration.setBytesPerReducer(bytes/2)
    //get the rules 
    val dRules:DList[Rule]= fromTextFile(rulesPath) map { ruleFromString(_) }
    //match sentences
    val sents = loadSents(batchPath)
    val matched = matchSents(candcBasePath, dRules, sents)
    //regroup the sentences 
    val sentsByRule = regroupMatched(matched)
    //apply the rules to transform the sentences
    val dRules2:DList[Rule] = fromTextFile(rulesPath) map { ruleFromString(_) }
    val translated = applyRules(candcBasePath, dRules2, sentsByRule)
    //and then restructure
    val repaired = uniquePairs(translated)
    val resolved = doResolve(repaired)
    //group the extracted rules and combine the groups
    val ruleStrings = combineIRFHs(resolved)
    //and save them
    ruleStrings.toTextFile(outPath).persist
  }

  def loadSents(sentsPath:String):DList[String] = fromTextFile(sentsPath) filter { l => l.split(' ').length < 50 }
  
  /** Finds all the rules that match each sentence. See  trie code for why this stuff works. */
  def matchSents(path:String, dRules:DList[Rule], lines:DList[String]):DList[Matched] = {
    val dtrie = dRules map { (new RuleTrieC).addRule(_) } reduce { Reduction(_ + _) }
    (DObject(path) join (dtrie join lines)) mapFlatten { computeMatched(_) }
  }

  type Matched = (String, LLCNF, List[Int])
  def computeMatched(v:(String, (RuleTrieC, String))):Option[Matched] = for {
    (path, (trie, line)) <- Some(v)
    matches = trie findAllRules line.toLowerCase if (matches.nonEmpty)
    cnf <- getCNF(GetFOL(path), line, "1", false) 
  } yield (line, cnf, matches)

  /** reorganize the matched sentences so that every rule has a list of matching sentences.*/
  def regroupMatched(matched:DList[Matched]):DList[(Int, Iterable[(String, LLCNF)])] = {
    val withRules:DList[(Int, (String, LLCNF))] = matched mapFlatten { 
      case (s, orig, ids) => ids map { _ -> (s -> orig) } 
    }
    withRules.groupByKey
  }

  /** apply rules by matchin up the ids in rule list with the ids of the sentence lists.*/
  def applyRules(path:String, dRules:DList[Rule], sents:DList[(Int, Iterable[(String, LLCNF)])]):DList[PairedCNF] = {
    val indexedRules:Relational[Int, Rule] = Relational(dRules map { r => r.id -> r })
    val joints = DObject(path) join (indexedRules join sents)
    joints mapFlatten {
      case (path, (id, (rule, sents))) => applySingleRule(GetFOL(path), new RuleApplier(rule), sents)
    }
  }

  def applySingleRule(getFOL:GetFOL, applier:RuleApplier, sents:Iterable[(String,LLCNF)]):Iterable[PairedCNF] = for {
    (sent, origCNF) <- sents
    TranslatedSentence(_, trans, _, id, weight) <- applier(sent)
    transCNF <- getCNF(getFOL, trans, "2", true)
  } yield (origCNF, transCNF, id, weight)

  import scalaz.Validation._
  //import scala.language.postfixOps
  
  /** gets FOL by calling C&C and boxer, then converts that into CNF and gets list-of-list form */
  def getCNF(getFOL:GetFOL, sent:String, id:String, negate:Boolean=false):Option[List[List[String]]] = for {
    fol <- getFOL(sent)
    cnf <- ConvertToCNF(if (negate) -fol else fol) { _ + id }
    cnfValid = fromTryCatch { FolContainer.cnfToLists(cnf) }
    _ = cnfValid.swap foreach { (t:Throwable) => println("failed lists conv on " + sent)  }
    lists <-  cnfValid.toOption
  } yield lists

  implicit val llcnfOrd:Ordering[LLCNF] = new Ordering[LLCNF] {
    def compare(l:LLCNF, r:LLCNF) = Ordering.Iterable[Iterable[String]].compare(l, r)
  }

  val red3 = Reduction.list[Int] zip3(Reduction.list[Double], Reduction.Sum.int)

  def uniquePairs(pairs:DList[PairedCNF]):DList[RePaired] = (keyed(pairs).groupByKey combine red3) map { 
    case ((orig, trans), (ids, weights, count)) => (orig, trans, ids, weights, count) 
  }
  
  def keyed(pairs:DList[PairedCNF]) = pairs map { case (o, t, i, w) => (o -> t) -> (List(i), List(w), 1)  }

  /** runs the resolver over the cnf pairs. */
  def doResolve(cnfps:DList[RePaired]):DList[IRFHolder] = cnfps mapFlatten { cnfp =>
    val (orig, trans, ids, weights, count) = cnfp
    val oRes = Resolution.resolveToFindDifference(orig, trans)
    oRes map { case (fLeft, _) => IRFHolder(fLeft.inferenceFin, ids, weights, count) }
  }

  /** groups the inference rule holders keyed upon the left and right hand sides of the rule itself, then combines the rule holders to tally
    * up the generation counts
    */
  def combineIRFHs(irfhs:DList[IRFHolder]):DList[String] = irfhs groupBy { 
    IRFHolders.toKey(_)
  } combine { 
    Reduction(IRFHolders.combine(_:IRFHolder, _:IRFHolder)) 
  } map { 
    case (_, irfh) =>  IRFHolders.toString(irfh)
  }
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
