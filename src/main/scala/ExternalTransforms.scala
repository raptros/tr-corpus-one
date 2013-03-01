package trc1

@EnhanceStrings
object ConvertToFOL {
  import scala.sys.process._

  val candcBase = "/home/02297/coynea90/install/candc"
  val soapClient = "#{candcBase}/bin/soap_client --url http://localhost:9000"
  val boxer = "#{candcBase}/bin/boxer --stdin --box true --semantics fol"

  def apply(sentence:String):Option[String] = requestFOL(sentence)

  //next thing is for when i get the stuff islam's been working on set up
  //val utTrans = ""

  def requestFOL(sentence:String):Option[String] = {
    try {
      val lStream:Stream[String] = (sentence #> soapClient #| boxer lines)
      lStream.lastOption
    } catch {
      case _ => None
    }
  }


  def convertToFOL(sentence:String):Option[String] = None
}


case class FOLPair(fol1:String, fol2:String, id:Int)
