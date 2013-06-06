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
  type Interm = ((LLCNF, String), (List[Int], List[Double], Int))

  type GetFOLInfo = (String, Int)

  /** the task pipeline */
  def run() {
    val rulesPath = args(0)
    val batchPath = args(1)
    val outPath = args(2)
    //get env variables for where parser can be found and how many are running
    val parserCount = getEnv("CANDC_INSTANCE_COUNT") map { _.toInt } getOrElse { 1 }
    val candcBasePath = getEnv("CANDC_HOME").err("CANDC_HOME must be set in order for GetFOL to work!")
    //check that GetFOL will be able to run.
    val gfi = (candcBasePath, parserCount)
    GetFOL(gfi).checkPaths()
    //uploadLibJarsFiles() //not necessary
    val bytes = configuration.getBytesPerReducer
    //ramming speed! (seriously though we have to be aggressive about splitting into multiple reducers, i think).
    configuration.setBytesPerReducer(bytes/8)
    //get the rules 
    val dRules:DList[Rule]= fromTextFile(rulesPath) map { ruleFromString(_) }
    //match sentences
    val sents = loadSents(batchPath)
    val matched = matchSents(gfi, dRules, sents)
    //regroup the sentences 
    val sentsByRule = regroupMatched(matched)
    //apply the rules to transform the sentences
    val dRules2:DList[Rule] = fromTextFile(rulesPath) map { ruleFromString(_) }
    val translated = applyRules(dRules2, sentsByRule)
    //and then restructure
    val repaired = uniquePairs(gfi, outPath, translated)
    val resolved = doResolve(repaired)
    //group the extracted rules and combine the groups
    val ruleStrings = combineIRFHs(resolved)
    //and save them
    ruleStrings.toTextFile(outPath).persist
  }

  def loadSents(sentsPath:String):DList[String] = fromTextFile(sentsPath) filter { l => l.split(' ').length < 50 }
  
  /** Finds all the rules that match each sentence. See  trie code for why this stuff works. */
  def matchSents(gfi:GetFOLInfo, dRules:DList[Rule], lines:DList[String]):DList[Matched] = {
    val dtrie = dRules map { (new RuleTrieC).addRule(_) } reduce { Reduction(_ + _) }
    (DObject(gfi) join (dtrie join lines)) mapFlatten { computeMatched(_) }
  }

  type Matched = (String, LLCNF, List[Int])
  def computeMatched(v:(GetFOLInfo, (RuleTrieC, String))):Option[Matched] = for {
    (gfi, (trie, line)) <- Some(v)
    matches = trie findAllRules line.toLowerCase if (matches.nonEmpty)
    cnf <- getCNF(GetFOL(gfi), line, "1", false) if (isSingletonClauses(cnf))
  } yield (line, cnf, matches)

  /** reorganize the matched sentences so that every rule has a list of matching sentences.*/
  def regroupMatched(matched:DList[Matched]):DList[(Int, Iterable[(String, LLCNF)])] = {
    val withRules:DList[(Int, (String, LLCNF))] = matched mapFlatten { 
      case (s, orig, ids) => ids map { _ -> (s -> orig) } 
    }
    withRules.groupByKey
  }

  /** apply rules by matchin up the ids in rule list with the ids of the sentence lists.*/
  def applyRules(dRules:DList[Rule], sents:DList[(Int, Iterable[(String, LLCNF)])]):DList[Interm] = {
    val indexedRules:Relational[Int, Rule] = Relational(dRules map { r => r.id -> r })
    val joints = (indexedRules join sents)
    joints mapFlatten {
      case (id, (rule, sents)) => applySingleRule(new RuleApplier(rule), sents)
    }
  }

  def applySingleRule(applier:RuleApplier, sents:Iterable[(String,LLCNF)]):Iterable[Interm] = for {
    (sent, origCNF) <- sents
    TranslatedSentence(_, trans, _, id, weight) <- applier(sent)
  } yield (origCNF -> trans) -> (List(id), List(weight), 1) //why 1? it's a count. relax.

  import scalaz.Validation._
  //import scala.language.postfixOps
  
  /** gets FOL by calling C&C and boxer, then converts that into CNF and gets list-of-list form */
  def getCNF(getFOL:GetFOL, sent:String, id:String, negate:Boolean=false):Option[LLCNF] = for {
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

  //def joinPath(base:String, next:String):String = (new File(base, next)).getPath

  def uniquePairs(gfi:GetFOLInfo, outPath:String, pairs:DList[Interm]):DList[RePaired] = {
    //val doneCNF = (DObject(path) join pairs) map { doCNF(_) } filter { _.nonEmpty } map { _.get }
    //(doneCNF.groupByKey combine red3) map { case ((o, t), (i, w, c)) => (o, t, i, w, c) }
    val pre = pairs checkpoint(outPath + "_checkpoint_pairs")
    val doneCNFs = (DObject(gfi) join pre) mapFlatten { doCNF(_) } //checkpoint(outPath + "_checkpoint_cnfs")
    (doneCNFs.groupByKey combine red3) map { case ((o, t), (i, w, c)) => (o, t, i, w, c) }
  }

  def doCNF(v:(GetFOLInfo, ((LLCNF, String), (List[Int], List[Double], Int)))) = for {
    (gfi, ((orig, trans), (ids, weights, count))) <- Some(v)
    cnf <- getCNF(GetFOL(gfi), trans, "2", true) if (isSingletonClauses(cnf))
  } yield (orig, cnf) -> (ids, weights, count)

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

  def isSingletonClauses(cnf:LLCNF):Boolean = (cnf.length == 1) || (cnf forall { c => c.length == 1 })
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
