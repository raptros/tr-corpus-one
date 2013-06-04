package trc1
import scala.sys.process._
import logic._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression._
import resolution.{InferenceRule, InferenceRuleFinal, finalizeInference}
import scalaz.std.option._
import scalaz.syntax.apply._
import scalaz.syntax.std.option._
import scalaz.syntax.std.boolean._
import scala.util.Properties

import scala.language.postfixOps
import java.io.File

/**a bunch of things to exchange between various rule types.*/
object RuleTypeChange {
  val flp = new parse.FolLogicParser
  
  def bringIRF(ir:InferenceRule, id:Int, weight:Double):IRFHolder = IRFHolder(finalizeInference(ir), List(id), List(weight), 1)

  def mkFOLE(quantifiers:List[(String, Iterable[String])], lhs:List[String], rhs:List[String]):FolExpression = {
    val conj = (side:List[String]) => side map { flp.parse _ } reduce { _ & _ }
    val quantify = (q:(String, Iterable[String]), e:FolExpression) => q match {
      case ("exists", variables) => e exists (variables map { Variable(_) })
      case ("forall", variables) => e all (variables map { Variable(_) })
    }
    val start = (conj(lhs) -> conj(rhs)).asInstanceOf[FolExpression]
    (quantifiers foldRight start) { quantify }
  }

  def irfhToFol(irfh:IRFHolder):FolRule = irfh match { 
    case IRFHolder(InferenceRuleFinal(quants, lhs, rhs), rules, weights, count) => {
      FolRule(mkFOLE(quants, lhs, rhs), rules, weights, count)
    }
  }

  //still needs implementing.
  def mkIRF(fole:FolExpression):Option[InferenceRuleFinal] = None

  def folToIrfh(fr:FolRule):Option[IRFHolder] = fr match {
    case FolRule(fole, rules, weights, count) => mkIRF(fole) map { IRFHolder(_, rules, weights, count) }
  }
}


/** calls c and c and then boxer to get fol expressions*/
object GetFOL {
  val candcBase = new File(Properties.envOrNone("CANDC_HOME").err("CANDC_HOME must be set in order for GetFOL to work!"))
  val soapClient = new File(candcBase, "bin/soap_client")
  val boxer = new File(candcBase, "bin/boxer")

  val soapClientArgList = List("url" -> "http://localhost:9000")

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
  lazy val soapClientArgs = mkArgString(soapClientArgList)
  lazy val soapClientCmd = s"${soapClient.getPath} ${soapClientArgs}"
  
  /** checks that all the necessary paths are available and readable. 
    * any app that will use GetFOL should call this at setup time.
    */
  def checkPaths() {
    candcBase.canRead unless { sys.error(s"CANDC_HOME must be set to readable location; ${candcBase.getPath} is not readable!") }
    soapClient.canRead unless { sys.error(s"${soapClient.getPath} isn't readable! make sure the SOAP system is installed and check the permissions.") }
    boxer.canRead unless { sys.error(s"${boxer.getPath} can't be read! check your boxer installation and the permissions.") }
  }

  def apply(sentence:String):Option[FolExpression] = {
      val echo = "echo " + sentence
      //external command runs
      val lStream:Stream[String] = (echo #| soapClientCmd #| boxerCmd lines_!)
      BoxerFOLParser.findFol(lStream) 
  }

  def mkArgString(args:List[(String, Any)]):String = args map { case (opt, arg) => s"--${opt} ${arg}" } mkString " "
}

