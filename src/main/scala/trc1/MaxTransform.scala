package trc1
import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.core.Reduction
import com.nicta.scoobi.lib.Relational
import scalaz.syntax.std.option._

import scala.math.Ordering

import logic.cnf.{ConvertToCNF, FolContainer}
import logic.fol
import logic.top.Variable
import logic.resolution.{Resolution, InferenceRuleFinal}

/** MaxTransform implements the complete pipeline for converting the lexical rules into FOL. */
object MaxTransform extends ScoobiApp {
  import ImplicitFormats._

  type LLCNF = List[List[String]]
  type PairedCNF = (LLCNF, LLCNF, Int, Double)
  type RePaired = (LLCNF, LLCNF, List[Int], List[Double], Int)
  type Trip = (List[Int], List[Double], Int)
  type Interm = ((LLCNF, String), Trip)

  type GetFOLInfo = (String, Int)

  /** the task pipeline */
  def run() {
    val rulesPath = args(0)
    val batchPath = args(1)
    val outPath = args(2)
    prepare()
    //get the rules 
    val dRules:DList[Rule]= fromTextFile(rulesPath) map { Rules.ruleFromString(_) }
    //match sentences
    val sents = loadSents(batchPath)
    val matched = matchSents(dRules, sents)
    //regroup the sentences 
    val sentsByRule = regroupMatched(matched)
    //apply the rules to transform the sentences
    val dRules2:DList[Rule] = fromTextFile(rulesPath) map { Rules.ruleFromString(_) }
    val translated = applyRules(dRules2, sentsByRule)
    //and then restructure
    val repaired = uniquePairs(outPath, translated)
    val resolved = doResolve(outPath, repaired)
    //group the extracted rules and combine the groups
    val ruleStrings = combineIRFHs(resolved)
    //and save them
    ruleStrings.toTextFile(outPath).persist
  }

  def prepare() {
    //get env variables for where parser can be found and how many are running
    val parserCount = getEnv(CANDC_INSTANCE_COUNT) map { _.toInt } getOrElse { 1 }
    val candcBasePath = getEnv(CANDC_HOME).err("CANDC_HOME must be set in order for GetFOL to work!")
    configuration.set("mapred.child.env", s"${CANDC_HOME}=${candcBasePath},${CANDC_INSTANCE_COUNT}=${parserCount}")
    configuration.set("mapred.max.map.failures.percent", "100000")
    //configuration.set("mapred.max.map.failures.percentkip.attempts.to.start.skipping", "10")
    configuration.set("mapred.max.tracker.blacklists", "1000")
    configuration.set("mapred.max.tracker.failures", "1000")
    configuration.set("mapred.map.max.attempts", "2000")
    //minutes times seconds times milliseconds
    val timeout = 2 * 60 * 1000
    configuration.set("mapred.task.timeout", s"${timeout}")
    //check that GetFOL will be able to run.
    GetFOL.checkPaths()
    //uploadLibJarsFiles() //not necessary
    val bytes = configuration.getBytesPerReducer
    //ramming speed! (seriously though we have to be aggressive about splitting into multiple reducers, i think).
    configuration.setBytesPerReducer(bytes/8)
  }

  def loadSents(sentsPath:String):DList[String] = fromTextFile(sentsPath) filter { l => l.split(' ').length < 50 }
  
  /** Finds all the rules that match each sentence. See  trie code for why this stuff works. */
  def matchSents(dRules:DList[Rule], lines:DList[String]):DList[Matched] = {
    val dtrie = dRules map { (new RuleTrieC).addRule(_) } reduce { Reduction(_ + _) }
    (dtrie join lines) mapFlatten { computeMatched(_) }
  }

  type Matched = (String, LLCNF, List[Int])
  def computeMatched(v:(RuleTrieC, String)):Option[Matched] = for {
    (trie, line) <- Some(v)
    matches = trie findAllRules line.toLowerCase if (matches.nonEmpty)
    cnf <- getCNF(line, "1", false) if (isSingletonClauses(cnf))
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
  def getCNF(sent:String, id:String, negate:Boolean=false):Option[LLCNF] = for {
    fol <- GetFOL(sent)
    cnf <- ConvertToCNF(if (negate) -fol else fol) { _ + id }
    cnfValid = fromTryCatch { FolContainer.cnfToLists(cnf) }
    _ = cnfValid.swap foreach { (t:Throwable) => println("failed lists conv on " + sent)  }
    lists <-  cnfValid.toOption
  } yield { println(s"have lists ${lists}"); lists }

  implicit val llcnfOrd:Ordering[LLCNF] = new Ordering[LLCNF] {
    def compare(l:LLCNF, r:LLCNF) = Ordering.Iterable[Iterable[String]].compare(l, r)
  }

  val red3 = Reduction.list[Int] zip3(Reduction.list[Double], Reduction.Sum.int)

  def uniquePairs(outPath:String, pairs:DList[Interm]):DList[RePaired] = {
    //val doneCNF = pairs map { doCNF(_) } filter { _.nonEmpty } map { _.get }
    //(doneCNF.groupByKey combine red3) map { case ((o, t), (i, w, c)) => (o, t, i, w, c) }
    //val pre = pairs checkpoint(outPath + "_checkpoint_pairs")
    //val doneCNFs = pre mapFlatten { doCNF(_) } //checkpoint(outPath + "_checkpoint_cnfs")
    //(doneCNFs.groupByKey combine red3) map { case ((o, t), (i, w, c)) => (o, t, i, w, c) }
    val ready:DList[Interm] = (pairs.groupByKey combine red3)// checkpoint(outPath + "_grouped_combined")
    ready mapFlatten { doCNF(_) } map { case ((o, t), (i, w, c)) => (o, t, i, w, c) }// checkpoint(outPath + "_doneCNFS")
  }

  def doCNF(v:((LLCNF, String), Trip)) = for {
    ((orig, trans), trip) <- Some(v)
    cnf <- getCNF(trans, "2", true) if (isSingletonClauses(cnf))
  } yield (orig, cnf) -> trip

  /** runs the resolver over the cnf pairs. */
  def doResolve(outPath:String, cnfps:DList[RePaired]):DList[IRFHolder] = cnfps mapFlatten { cnfp =>
    val (orig, trans, ids, weights, count) = cnfp
    val oRes = Resolution.resolveToFindDifference(orig, trans)
    oRes map { case (fLeft, _) => IRFHolder(fLeft.inferenceFin, ids, weights, count) }
  } checkpoint(outPath + "_resolutionDone")

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
