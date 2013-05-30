package trc1
import scala.sys.process._
import logic._
import utcompling.scalalogic.fol.expression._
import collection.immutable.{Set => ISet}
import resolution.{InferenceRule, InferenceRuleFinal, finalizeInference}
import scalaz.std.option._
import scalaz.syntax.apply._
import scalaz.syntax.std.option._

/** trait representing things that hang on to rule representations.*/
trait RuleHolder[A] {
  def r:A
  def rules:List[Int]
  def weights:List[Double]
  def count:Int
}

case class FolRule(r:FolExpression, rules:List[Int], weights:List[Double], count:Int) extends RuleHolder[FolExpression]

case class IRFHolder(r:InferenceRuleFinal, rules:List[Int], weights:List[Double], count:Int) extends RuleHolder[InferenceRuleFinal]

/** base trait for objects that allow reading and writing rule holders*/
trait RuleHolders[A, B <: RuleHolder[A]] {
  /** how to turn the rule rep into a string.*/
  def rToString(r:A):String
  
  /** how to read a rule rep from a string*/
  def rFromString(l:String):Option[A]

  /**how to construct the holder from the RuleHolder fields*/
  def mkRH(r:A, rules:List[Int], weights:List[Double], count:Int):B

  def mkRH(r:A, rules:Array[Int], weights:Array[Double], count:Int):B = mkRH(r, rules.toList, weights.toList, count)

  /** assumes that they are already equal */
  def combine(l:B, r:B):B = mkRH(l.r, l.rules ++ r.rules, l.weights ++ r.weights, l.count + r.count)

  /** produces a string version of the holder*/
  def toString(rh:RuleHolder[A]):String = List(rToString(rh.r), (rh.rules mkString ","),
    (rh.weights mkString ","), rh.count.toString) mkString "\t"

  /**reads a string into the appropriate holder*/
  def fromString(line:String):Option[B] = line.split("\t") match {
    case Array(l, rules, weights, count) => rFromString(l) map { r =>
      mkRH(r, (rules split ",") map { _.toInt }, (weights split ",") map { _.toDouble }, count.toInt)
    }
    case _ => None
  }
}

/** implementation for the FolRule, which uses FolExpressions and boxer's fol format to represent rules.*/
object FolRules extends RuleHolders[FolExpression, FolRule] {
  def rToString(r:FolExpression):String = r.toBoxerFolFormat
  def rFromString(l:String):Option[FolExpression] = BoxerFOLParser.extractFol(l)
  def mkRH(r:FolExpression, rules:List[Int], weights:List[Double], count:Int):FolRule = {
    FolRule(r, rules, weights, count)
  }
}
/** implementation for IRFHolder, which uses the InferenceRule format for rule representation */
object IRFHolders extends RuleHolders[InferenceRuleFinal, IRFHolder] {
  def mkQuantString(quants:List[(String, Iterable[String])]):String = { 
    (for { (q, vs) <- quants } yield q + "." + (vs mkString ",")) mkString ":"
  }

  def getQuant(sQuant:String):Option[(String, Set[String])] = sQuant.split('.') match {
    case Array(q, rest) => Some(q -> (rest split ',').toSet)
    case _ => None
  }
  
  def getQuants(sQuants:String):Option[List[(String, Set[String])]] = {
    val quants1 = (sQuants split ':') map { getQuant(_) }
    if (quants1 exists { _.isEmpty }) None else Some(quants1.toList.flatten)
  }

  def rToString(r:InferenceRuleFinal):String = r match {
    case InferenceRuleFinal(quants, lhs, rhs) => List(mkQuantString(quants), 
      (lhs mkString "&"), (rhs mkString "&")) mkString "|"
  }
  def rFromString(l:String):Option[InferenceRuleFinal] = (l split '|') match {
    //one of those weird scalaz things here: ^(o1, o2, o3) ( (n1, n2, n3) => something )
    case Array(sQuants, sLhs, sRhs) => ^(getQuants(sQuants), Some((sLhs split '&').toList), Some((sRhs split '&').toList)) {
      InferenceRuleFinal
    }
  }
  def mkRH(r:InferenceRuleFinal, rules:List[Int], weights:List[Double], count:Int):IRFHolder = {
    IRFHolder(r, rules, weights, count)
  }
}

