package trc1
import scala.sys.process._
import logic.{ConvertToCNF,BoxerFOLParser}
import utcompling.scalalogic.fol.expression._

/** case class representing a pair of sentence versions in FOL. */
//case class FOLPair(fol1:String, fol2:String, id:Int)
case class FOLPair(orig:String, translated:String, fol1:String, fol2:String, rule:String, id:Int)

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

