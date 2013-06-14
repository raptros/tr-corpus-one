package trc1
import com.nicta.scoobi.Scoobi._
import com.nicta.scoobi.core.Reduction
import com.nicta.scoobi.lib.Relational

import scalaz.syntax.std.option._
import scalaz.syntax.id._
import scalaz.Validation
import scalaz.Validation._

import scala.math.Ordering

import logic.cnf.{ConvertToCNF, FolContainer}
import logic.fol
import logic.top.Variable
import logic.resolution.{Resolution, InferenceRuleFinal}

/** MaxTransform implements the complete pipeline for converting the lexical rules into FOL. */
object MaxTransform extends ScoobiApp {
  import ImplicitFormats._

  type LLCNF = List[List[String]]
  type Trip = (List[Int], List[Double], Int)
  type Matched = (String, LLCNF, List[Int])

  /** the task pipeline 
    * steps:
    * - prepare() (checks some stuff)
    * - load up the rules
    * - load and filter sentences
    * - match and parse (and filter) sentences
    * - regroup the matches
    * - apply the rules to transform the sentences
    * - convert the transformed sentences to cnf, perform resolution, and combine the results
    * - turn the results into strings, then write them out
    */
  def run() {
    val rulesPath = args(0)
    val batchPath = args(1)
    val outPath = args(2)
    prepare()
    val dRules:DList[Rule] = fromTextFile(rulesPath) map { Rules.ruleFromString(_) }
    val sents = loadSents(batchPath)
    val matched = matchSents(dRules, sents)
    val sentsByRule = regroupMatched(matched)
    val transformed = applyRules(dRules, sentsByRule)
    val resolved = uniqueRules(outPath, transformed)
    val ruleStrings = resolved map { (RuleTypeChange.irfhFromTuple _) andThen (IRFHolders.toString _) }
    ruleStrings.toTextFile(outPath).persist
  }

  def prepare() {
    //get env variables for where parser can be found and how many are running
    val parserCount = getEnv(CANDC_INSTANCE_COUNT) map { _.toInt } getOrElse { 1 }
    val candcBasePath = getEnv(CANDC_HOME).err("CANDC_HOME must be set in order for GetFOL to work!")
    //check that GetFOL will be able to run.
    GetFOL.checkPaths()
    configuration.set("mapred.child.env", s"${CANDC_HOME}=${candcBasePath},${CANDC_INSTANCE_COUNT}=${parserCount}")
  }

  /** loads the sentences into a dlist, and filters out any sentence longer than 50 words. */
  def loadSents(sentsPath:String):DList[String] = fromTextFile(sentsPath) filter { l => (l split ' ').length < 50 }
  
  /** Finds all the rules that match each sentence. See  trie code for why this stuff works. */
  def matchSents(dRules:DList[Rule], lines:DList[String]):DList[Matched] = {
    val dtrie = dRules map { (new RuleTrieC) addRule _ } reduce { Reduction { _ + _ } }
    (dtrie join lines) mapFlatten { computeMatched _ }
  }

  def computeMatched(v:(RuleTrieC, String)):Option[Matched] = for {
    (trie, line) <- Some(v)
    matches = trie findAllRules (line.toLowerCase) if (matches.nonEmpty)
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
  def applyRules(dRules:DList[Rule], sents:DList[(Int, Iterable[(String, LLCNF)])]):DList[(LLCNF, String, Int, Double)] = {
    val indexedRules:Relational[Int, Rule] = Relational(dRules map { r => r.id -> r })
    val joints = (indexedRules join sents)
    joints mapFlatten {
      case (id, (rule, sents)) => applySingleRule(new RuleApplier(rule), sents)
    }
  }

  def applySingleRule(applier:RuleApplier, sents:Iterable[(String,LLCNF)]):Iterable[(LLCNF, String, Int, Double)] = for {
    (sent, origCNF) <- sents
    TranslatedSentence(_, trans, _, id, weight) <- applier(sent)
  } yield (origCNF, trans, id, weight)

  implicit val irfOrd:Ordering[InferenceRuleFinal] = Ordering.by[InferenceRuleFinal, (String, String)] { irf => IRFHolders.rToKey(irf) }

  val red3 = Reduction.list[Int] zip3(Reduction.list[Double], Reduction.Sum.int)

  /** extracts the cnf for the right side, runs resolution on the pair, and then combines the produced rule with a reduction that tallies up
    * production counts.
    */
  def uniqueRules(outPath:String, pairs:DList[(LLCNF, String, Int, Double)]):DList[(InferenceRuleFinal, Trip)] = {
    val pre = pairs checkpoint(outPath + "_checkpoint_pairs")
    val doneRules = pre mapFlatten { doCNF _ } mapFlatten { doResolve _ }
    (doneRules.groupByKey combine red3)
  }

  /** converts the transformed sentence into a CNF */
  def doCNF(v:(LLCNF, String, Int, Double)):Option[(LLCNF, LLCNF, Int, Double)]= for {
    (orig, trans, id, weight) <- Some(v)
    cnf <- getCNF(trans, "2", true) if (isSingletonClauses(cnf))
  } yield (orig, cnf, id, weight)

  /** performs resolution on the pair of llcnfs and produces the result with the appropriate info structures */
  def doResolve(cnfp:(LLCNF, LLCNF, Int, Double)):Option[(InferenceRuleFinal, Trip)] = for {
    (orig, trans, id, weight) <- Some(cnfp)
    (fLeft, _) <- Resolution.resolveToFindDifference(orig, trans)
  } yield fLeft.inferenceFin -> (List(id), List(weight), 1)
  
  /** gets FOL by calling C&C and boxer, then converts that into CNF and gets list-of-list form */
  def getCNF(sent:String, id:String, negate:Boolean=false):Option[LLCNF] = {
    val llcnfv = getValidCNF(sent, id, negate)
    llcnfv.swap foreach { m => println(s"""getCNF("${sent}", $id, $negate): $m""") }
    llcnfv.toOption
  }

  /** gets the cnf form in the context of successes and failures. */
  def getValidCNF(sent:String, id:String, negate:Boolean):Validation[String, LLCNF] = for {
    fol <- GetFOL(sent)
    cnf <- ConvertToCNF(if (negate) -fol else fol) { _ + id }
    lists <- fromTryCatch { FolContainer.cnfToLists(cnf) } leftMap { t => 
      s"failed list conversion - ${t.getClass} with msg: ${t.getMessage}"
    }
  } yield lists

  def isSingletonClauses(cnf:LLCNF):Boolean = (cnf.length == 1) || (cnf forall { c => c.length == 1 })
}
