package trc1
import scala.sys.process._

import logic.parsing._
import logic.fol

import logic.resolution.{InferenceRule, InferenceRuleFinal}

import collection.immutable.{Set => ISet}
import scalaz.std.option._
import scalaz.syntax.apply._
import scalaz.syntax.std.option._
import scalaz.syntax.std.boolean._

/** trait representing things that hang on to rule representations.*/
trait RuleHolder[A] {
  def r:A
  def rules:List[Int]
  def weights:List[Double]
  def count:Int
}

/** stores a rule represented by an Fol expression */
case class FolRule(r:fol.Expr, rules:List[Int], weights:List[Double], count:Int) extends RuleHolder[fol.Expr]

/** stores a rule represented by the quantifiers, lhs, rhs triple (in a case class) */
case class IRFHolder(r:InferenceRuleFinal, rules:List[Int], weights:List[Double], count:Int) extends RuleHolder[InferenceRuleFinal]

/** base trait for objects that allow reading and writing rule holders*/
trait RuleHolders[A, B <: RuleHolder[A]] {
  /** defines how to turn the rule rep into a string.*/
  def rToString(r:A):String
  
  /** defines how to read a rule rep from a string*/
  def rFromString(l:String):Option[A]

  /** defines how to construct the holder from the RuleHolder fields*/
  def mkRH(r:A, rules:List[Int], weights:List[Double], count:Int):B

  /** constructs a rule holder using arrays. */
  def mkRH(r:A, rules:Array[Int], weights:Array[Double], count:Int):B = mkRH(r, rules.toList, weights.toList, count)

  /** assumes that they are already equal */
  def combine(l:B, r:B):B = mkRH(l.r, l.rules ++ r.rules, l.weights ++ r.weights, l.count + r.count)

  /** produces a string version of the holder*/
  def toString(rh:RuleHolder[A]):String = {
    val parts = rToString(rh.r) :: (rh.rules mkString ",") :: (rh.weights mkString ",") :: rh.count.toString :: Nil
    parts mkString "\t"
  }

  /** reads a string into the appropriate holder */
  def fromString(line:String):Option[B] = line.split("\t") match {
    case Array(l, rules, weights, count) => rFromString(l) map { r =>
      mkRH(r, (rules split ",") map { _.toInt }, (weights split ",") map { _.toDouble }, count.toInt)
    }
    case _ => None
  }

  /** how a rule holder should be keyed on*/
  type KeyType 
  def toKey(rh:RuleHolder[A]):KeyType  = rToKey(rh.r)

  def rToKey(r:A):KeyType
}

/** implementation for the FolRule, which uses FolExpressions and boxer's fol format to represent rules.
  * most of the work is done elsewhere
  */
object FolRules extends RuleHolders[fol.Expr, FolRule] {
  def rToString(r:fol.Expr):String = r.toBoxerFolFormat
  
  def rFromString(l:String):Option[fol.Expr] = BoxerFOLParser.extractFol(l)

  def mkRH(r:fol.Expr, rules:List[Int], weights:List[Double], count:Int):FolRule = {
    FolRule(r, rules, weights, count)
  }

  type KeyType = String

  def rToKey(r:fol.Expr):String = rToString(r)
}

/** implementation for IRFHolder, which uses the InferenceRule format for rule representation 
  * format: quants|lhs|rhs.
  * quants is a list, separated by colon, where each quant is
  * quantname.(vars), and vars are comma separated
  * the lhs and rhs are lists separated by ampersands, containing various types of atomic level terms
  */
object IRFHolders extends RuleHolders[InferenceRuleFinal, IRFHolder] {
  def mkQuantString(quants:List[(String, Iterable[String])]):String = quants map { 
    case (q, vs) => q + "." + (vs mkString ",")
  } mkString ":"

  def getQuant(sQuant:String):Option[(String, Set[String])] = (sQuant split '.') match {
    case Array(q, rest) => Some(q -> (rest split ',').toSet)
    case _ => None
  }
  
  /** each quantifier expression is separated by a colon */
  def getQuants(sQuants:String):Option[List[(String, Set[String])]] = {
    val quants1 = (sQuants split ':') map { getQuant(_) }
    (quants1 forall { _.nonEmpty }) option (quants1.toList.flatten)
  }

  /** puts together the three fields with pipe symbols */
  def rToString(r:InferenceRuleFinal):String = {
    val parts = List(mkQuantString(r.quantifiers), (r.lhs mkString "&"), (r.rhs mkString "&")) 
    parts mkString "|"
  }

  /** breaks apart the fields, then reads them in */
  def rFromString(l:String):Option[InferenceRuleFinal] = (l split '|') match {
    //one of those weird scalaz things here: ^^(o1, o2, o3) ( (n1, n2, n3) => something )
    case Array(sQuants, sLhs, sRhs) => ^^(getQuants(sQuants), Some((sLhs split '&').toList), Some((sRhs split '&').toList)) {
      InferenceRuleFinal
    }
    case _ => None
  }

  def mkRH(r:InferenceRuleFinal, rules:List[Int], weights:List[Double], count:Int):IRFHolder = 
    IRFHolder(r, rules, weights, count)

  type KeyType = (String, String)

  def rToKey(r:InferenceRuleFinal) = (r.lhs mkString "&") -> (r.rhs mkString "&")
}

