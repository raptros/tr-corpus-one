package trc1
import scala.sys.process._
import logic._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression._
import resolution.{InferenceRule, InferenceRuleFinal, finalizeInference}
import scalaz.std.option._
import scalaz.syntax.apply._
import scalaz.syntax.std.option._

import scala.language.postfixOps

/** case class representing a pair of sentence versions in FOL. */
//case class FOLPair(fol1:String, fol2:String, id:Int)
//case class FOLPair(orig:String, translated:String, fol1:String, fol2:String, rule:String, id:Int)

/**a bunch of things to exchange between various rule types.*/
object RuleTypeChange {
  val flp = new parse.FolLogicParser
  
  def bringIRF(ir:InferenceRule, id:Int, weight:Double):IRFHolder = IRFHolder(finalizeInference(ir), List(id), List(weight), 1)

  def mkFOLE(quantifiers:List[(String, Iterable[String])], lhs:List[String], rhs:List[String]):FolExpression = {
    val conj = (side:List[String]) => side.map(v => flp.parse(v)).reduce(_ & _)
    val quantify = (q:(String, Iterable[String]), e:FolExpression) => q match {
      case ("exists", variables) => e.exists(variables map (Variable(_)))
      case ("forall", variables) => e.all(variables map (Variable(_)))
    }
    val start = (conj(lhs)->conj(rhs)).asInstanceOf[FolExpression]
    quantifiers.foldRight(start){ quantify }
  }

  def irfhToFol(irfh:IRFHolder):FolRule = irfh match { 
    case IRFHolder(InferenceRuleFinal(quants, lhs, rhs), rules, weights, count) => {
      FolRule(mkFOLE(quants, lhs, rhs), rules, weights, count)
    }
  }

  //still needs implementing.
  def mkIRF(fole:FolExpression):Option[InferenceRuleFinal] = None

  def folToIrfh(fr:FolRule):Option[IRFHolder] = fr match {
    case FolRule(fole, rules, weights, count) => mkIRF(fole) map (IRFHolder(_, rules, weights, count))
  }
}


/** calls c and c and then boxer to get fol expressions*/
object GetFOL {

  //val candcBase = "/home/02297/coynea90/install/candc"
  val candcBase = "/home/aidan/Install/candc"
  val soapClient = s"${candcBase}/bin/soap_client --url http://localhost:9000"
  val boxer = s"${candcBase}/bin/boxer --stdin --box false --semantics fol --flat false --resolve true --elimeq true --format prolog --instantiate true"

  def apply(sentence:String):Option[FolExpression] = {
      val echo = "echo " + sentence
      //external command runs
      val lStream:Stream[String] = (echo #| soapClient #| boxer lines_!)
      BoxerFOLParser.findFol(lStream) 
  }
}

