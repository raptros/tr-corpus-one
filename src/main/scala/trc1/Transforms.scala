package trc1
import scala.sys.process._

import logic._
import logic.top.Variable
import logic.fol

import logic.parsing._

import resolution.{InferenceRule, InferenceRuleFinal, finalizeInference}

import scalaz._
import std.option._
import optionSyntax._
import syntax.apply._
import syntax.id._
import syntax.std.boolean._

import collection.mutable.ListBuffer

import java.io.File

import scala.util.Random

/**a bunch of things to exchange between various rule types.*/
object RuleTypeChange {
  type Quant = (String, Iterable[String])

  def bringIRF(ir:InferenceRule, id:Int, weight:Double):IRFHolder = IRFHolder(ir.inferenceFin, List(id), List(weight), 1)

  def parseQuick(s:String):fol.Expr = (BoxerFOLParser extractExpr s).err("something went wrong tryingg to extract ${s} to FOL!")

  def quantify(q:Quant, e:fol.Expr):fol.Expr = q match {
    case ("exists", variables) => e exists (variables map { Variable(_) })
    case ("forall", variables) => e all (variables map { Variable(_) })
  }
  
  def conj(side:List[String]):fol.Expr = side map { parseQuick(_) } reduce { _ & _ }

  def mkFOLE(quantifiers:List[Quant], lhs:List[String], rhs:List[String]):fol.Expr = 
    (quantifiers foldRight (conj(lhs) -> conj(rhs))) { quantify(_, _) }

  def irfhToFol(irfh:IRFHolder):FolRule = irfh match { 
    case IRFHolder(InferenceRuleFinal(quants, lhs, rhs), rules, weights, count) => {
      FolRule(mkFOLE(quants, lhs, rhs), rules, weights, count)
    }
  }

  //still needs implementing.
  def mkIRF(fole:fol.Expr):Option[InferenceRuleFinal] = None

  def folToIrfh(fr:FolRule):Option[IRFHolder] = fr match {
    case FolRule(fole, rules, weights, count) => mkIRF(fole) map { IRFHolder(_, rules, weights, count) }
  }
}


/** calls c and c and then boxer to get fol expressions*/
object GetFOL { //(val candcBasePath:String, val instanceCount:Int=1) {
  val instanceCount = sys.env.get(CANDC_INSTANCE_COUNT) map { _.toInt } getOrElse { 1 }

  val candcBasePath = sys.env.get(CANDC_HOME).err("CANDC_HOME must be set in order for GetFOL to work!")

  val candcBase = new File(candcBasePath)
  val soapClient = new File(candcBase, "bin/soap_client")
  val boxer = new File(candcBase, "bin/boxer")


  val boxerArgList = List(
    "stdin" -> "", 
    "box" -> false, 
    "semantics" -> "fol", 
    "flat" -> false, 
    "resolve" -> true, 
    "elimeq" -> true, 
    "format" -> "prolog",
    "instantiate" -> true)
  
  lazy val boxerArgs = mkArgString(boxerArgList)
  lazy val boxerCmd = s"${boxer.getPath} ${boxerArgs}"
  //lazy val soapClientArgs = mkArgString(soapClientArgList)
  //lazy val soapClientCmd = s"${soapClient.getPath} ${soapClientArgs}"
  
  val basePort = 12200

  def soapClientCmd = {
    //randomly select a port - each instance runs on a separate port.
    val port = basePort + Random.nextInt(instanceCount)
    val soapClientArg = s"--url http://localhost:${port}"
    s"${soapClient.getPath} ${soapClientArg}"
  }

  /** checks that all the necessary paths are available and readable. 
    * any app that will use GetFOL should call this at setup time.
    */
  def checkPaths() {
    candcBase.canRead unless { sys.error(s"CANDC_HOME must be set to readable location; ${candcBase.getPath} is not readable!") }
    soapClient.canRead unless { sys.error(s"${soapClient.getPath} isn't readable! make sure the SOAP system is installed and check the permissions.") }
    boxer.canRead unless { sys.error(s"${boxer.getPath} can't be read! check your boxer installation and the permissions.") }
  }

  def apply(sentence:String):Option[fol.Expr] = {
    import Console.err
    val echo = "echo " + sentence
    //external command runs
    //i don't like doing this, but I think it'll be much faster.
    ///val lb = ListBuffer.empty[FolExpression]
    var s = none[fol.Expr]
    val onL:String => Unit = _ |> { BoxerFOLParser extractFol _ } foreach { f => s = s orElse some(f) }
    val pl = ProcessLogger(onL, e => ())
    (echo #| soapClientCmd #| boxerCmd) ! pl
    s
  }

  def mkArgString(args:List[(String, Any)]):String = args map { case (opt, arg) => s"--${opt} ${arg}" } mkString " "
}

/*
object GetFOL {
  def apply(candcBasePath:String):GetFOL = new GetFOL(candcBasePath)

  def apply(gfi:(String, Int)):GetFOL = {
    val (basePath, port) = gfi
    new GetFOL(basePath, port)
  }
}
*/
