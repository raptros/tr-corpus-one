package trc1

@EnhanceStrings
object ConvertToFOL {
  import scala.sys.process._

  //val candcBase = "/home/02297/coynea90/install/candc"
  val candcBase = "/home/aidan/Install/candc"
  val soapClient = "#{candcBase}/bin/soap_client --url http://localhost:9000"
  val boxer = "#{candcBase}/bin/boxer --stdin --box true --semantics fol"

  def apply(sentence:String):Option[String] = requestFOL(sentence)

  //next thing is for when i get the stuff islam's been working on set up
  //val utTrans = ""

  /** use the process library to do parsing/boxer/etc. */
  def requestFOL(sentence:String):Option[String] = {
    try {
      val echo = "echo " + sentence
      //external command runs
      val lStream:Stream[String] = (echo #| soapClient #| boxer lines_!)
      lStream.lastOption
    } catch {
      case _ => None
    }
  }
}


/** case class representing a pair of sentence versions in FOL. */
//case class FOLPair(fol1:String, fol2:String, id:Int)
case class FOLPair(orig:String, translated:String, fol1:String, fol2:String, rule:String, id:Int)


/** Uses the stuff Dan and Islam have built to produce FOL.
  *
  */
@EnhanceStrings
object ConvertWithMSEM {
  import scala.sys.process._
  import mlnsem.ProduceFOL

  //val candcBase = "/home/02297/coynea90/install/candc"
  val candcBase = "/home/aidan/Install/candc"
  val soapClient = "#{candcBase}/bin/soap_client --url http://localhost:9000"
  val boxer = "#{candcBase}/bin/boxer --stdin --box false --semantics drs --flat false --resolve true --elimeq true --format prolog --instantiate true"

  /** see mlnsem subpackage.*/
  val folConv = new ProduceFOL


  def apply(sentence:String):Option[String] = requestFOL(sentence)

  //next thing is for when i get the stuff islam's been working on set up
  //val utTrans = ""


  /** use the process library to do parsing/boxer/etc. */
  def requestFOL(sentence:String):Option[String] = {
    //try {
      val echo = "echo " + sentence
      //external command runs
      val lStream:Stream[String] = (echo #| soapClient #| boxer lines_!)
      folConv.toFOL(lStream.mkString("\n")).map(_.toString).headOption
   /* } catch {
      case _ => None
    }*/
  }
}
