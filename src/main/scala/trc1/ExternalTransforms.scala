package trc1
import scala.sys.process._
import logic._
import utcompling.scalalogic.fol.expression._

/** case class representing a pair of sentence versions in FOL. */
//case class FOLPair(fol1:String, fol2:String, id:Int)
case class FOLPair(orig:String, translated:String, fol1:String, fol2:String, rule:String, id:Int)


case class FolRule(fol:FolExpression, rules:List[Int], weights:List[Double], count:Int)

@EnhanceStrings
object FolRules {
  def toString(fr:FolRule):String = fr match {
    case FolRule(fol, rules, weights, count) => List(
      fol.toBoxerFolFormat,
      rules.mkString(","), weights.mkString(","),
      count.toString).mkString("\t")
  }

  def fromString(line:String):Option[FolRule] = line.split("\t") match {
    case Array(fols, rules, weights, count) => for {
      fol <- BoxerFOLParser.extractFol(fols)
    } yield FolRule(fol, rules.split(",").toList.map(_.toInt), weights.split(",").toList.map(_.toDouble), count.toInt)
    case _ => None
  }
}

/** Uses the stuff Dan and Islam have built to produce FOL.
  *
  */
@EnhanceStrings
object GetFOL {

  //val candcBase = "/home/02297/coynea90/install/candc"
  val candcBase = "/home/aidan/Install/candc"
  val soapClient = "#{candcBase}/bin/soap_client --url http://localhost:9000"
  val boxer = "#{candcBase}/bin/boxer --stdin --box false --semantics fol --flat false --resolve true --elimeq true --format prolog --instantiate true"

  def apply(sentence:String):Option[FolExpression] = {
      val echo = "echo " + sentence
      //external command runs
      val lStream:Stream[String] = (echo #| soapClient #| boxer lines_!)
      BoxerFOLParser.findFol(lStream) 
  }
}

