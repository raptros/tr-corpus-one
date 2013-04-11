package trc1.mlnsem
import utcompling.scalalogic.discourse.candc.call._
import utcompling.scalalogic.discourse.candc.call.impl._
import sys.process._
import java.io.ByteArrayInputStream
import opennlp.scalabha.util.FileUtils

/** A fake boxer call implementation that does nothing. See MyBDI
  */
class BoxerImpl2(val binary:String, defaultArgs:Map[String, String] = Map()) extends Boxer {
  def callBoxer(candcOut:String, args:Map[String, String]=Map(), verbose:Boolean = false):String = {
    val argsList = (this.defaultArgs ++ defaultArgs ++ args).toList.flatMap {
      p => List(p._1, p._2) 
    }
    val input = new ByteArrayInputStream(candcOut.getBytes("UTF-8"))
    val output = (Process(binary, "--stdin"::argsList) #< input).lines_!
    output.mkString("\n")
  }
}

object BoxerImpl2 {
  def findBinary(binDir:Option[String]=None, envar:Option[String]=Some("CANDCHOME"), defaultArgs:Map[String, String]=Map(), verbose:Boolean=false) = {
    new BoxerImpl2("", defaultArgs =defaultArgs)
  }
}


/** A fake C and C parser call implementation that does nothing. See MyBDI
  */
class CandcImpl2(val binary: String, val defaultArgs: Map[String, String]) 
extends Candc {
  /*
  def this(binary: String, defaultArgs: Map[String, String]=Map()) = {
    this(binary, pathjoin(binary.dropRight(5), "../models"), defaultArgs)
  }*/

  def batchParseMultisentence(inputs: Seq[Seq[String]], args: Map[String, String] = Map(), discourseIds: Option[Seq[String]] = None, model: Option[String] = None, verbose: Boolean = false): String = {
   // val newDiscourseIds = discourseIds.getOrElse((0 until inputs.length).map(_.toString))
   ""
  }

  def parse1(input:String):String = {
    ""
  }
}

object CandcImpl2 {
  //
  def findBinary(binDir: Option[String] = None, envar: Option[String] = Some("CANDCHOME"), defaultArgs: Map[String, String] = Map(), verbose: Boolean = false) = {
    new CandcImpl2("", defaultArgs = defaultArgs)
  }
}
