package trc1.mlnsem
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.mlnsemantics.modal._
import utcompling.scalalogic.discourse.candc.boxer.expression._
import utcompling.scalalogic.drt.expression.DrtExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.interpreter.impl._
import utcompling.scalalogic.discourse.impl.BoxerDiscourseInterpreter
import utcompling.scalalogic.discourse.candc.parse.output.impl._
import utcompling.scalalogic.discourse.impl._
import utcompling.mlnsemantics.polarity._
import scala.collection.mutable.MapBuilder

/** MyBDI allows calling into the BoxerDiscourseInterpreter as needed by this code.
  * Note that it requires modifying BDI to not have things marked private.
  */
class MyBDI
extends BoxerDiscourseInterpreter[BoxerExpression](
  boxerExpressionInterpreter = new PassthroughBoxerExpressionInterpreter(),
  candc = CandcImpl2.findBinary(),
  boxer = BoxerImpl2.findBinary(),
  verbose = false
) {
  override def parseBoxerOutput(boxerOut: String): Map[String, Option[BoxerExpression]] = {
    val drsDict = new MapBuilder[String, Option[BoxerExpression], Map[String, Option[BoxerExpression]]](Map[String, Option[BoxerExpression]]())
    val singleQuotedRe = """^'(.*)'$""".r

    val lines = boxerOut.split("\n").iterator
    val IdLineRe = """^id\((\S+),\s*(\d+)\)\.$""".r
    val SemLineRe = """^sem\((\d+),$""".r
    for (line <- lines.map(_.trim)) {
      line match {
        case IdLineRe(discourseId, drsId) =>
          //lines.next.trim match { case SemLineRe(drsId2) => require(drsId == drsId2, "%s != %s".format(drsId, drsId2)) }
          //lines.next.trim match { case l if l.startsWith("[word(") => }
          //lines.next.trim match { case l if l.startsWith("[pos(") => }
          //lines.next.trim match { case l if l.startsWith("[") => }
          //lines.next.trim match { case l if l.startsWith("sem(") => }
          val drsInput = lines.next.trim.stripPrefix("sem(1,").stripSuffix(").")

          val cleanDiscourseId = singleQuotedRe.findFirstMatchIn(discourseId).map(_.group(1)).getOrElse(discourseId)
          val parsed = this.parseOutputDrs(drsInput, cleanDiscourseId)
          drsDict += cleanDiscourseId -> Some(this.boxerExpressionInterpreter.interpret(parsed))
        case _ => { }
      }
    }
    return drsDict.result
  }
} 


