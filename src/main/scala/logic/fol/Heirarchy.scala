package logic.fol

import logic.top.Variable
import logic.{base => be}

import scalaz.std._
import list._, string._, anyVal._
import scalaz.syntax.id._

case class VariableExpr(variable:Variable) extends Expr with be.VariableExpr

case class Negated(term:Expr) extends Expr with be.Negated {
  override def toString = Tokens.NOT + term

  override def mkPretty:List[String] = (term.mkPretty) matchOrZero {
    case (head :: tail) => (Tokens.NOT + head) :: addSpaces(tail)
  }
}

case class Lambda(variable:Variable, term:Expr) extends VariableBinder(Tokens.LAMBDA) with be.Lambda

abstract class Quantified(operator:String) extends VariableBinder(operator)

case class All(variable:Variable, term:Expr) extends Quantified(Tokens.ALL)

case class Exists(variable:Variable, term:Expr) extends Quantified(Tokens.EXISTS)


abstract class Binary(val operator:String) extends Expr with be.Binary

case class Equality(first:Expr, second:Expr) extends Binary(Tokens.EQ)

abstract class BooleanExpr(operator:String) extends Binary(operator) {
  override def mkPretty:List[String] = {
    val firstP = first.mkPretty
    val secondP = second.mkPretty
    val fPart = addSpaces(firstP dropRight 1)
    val sPart = s" ${firstP.last} ${operator}" :: secondP map { " " + _ }
    Tokens.surroundList(fPart ++ sPart)
  }
}

case class If(first:Expr, second:Expr) extends BooleanExpr(Tokens.IMP)

case class Iff(first:Expr, second:Expr) extends BooleanExpr(Tokens.IFF)

abstract class Junction(operator:String) extends BooleanExpr(operator) {
  override def toString = Tokens.surround(allConnectedSameLevel mkString (" "+operator+" "))

  override def mkPretty:List[String] = {
    val partsR = allConnectedSameLevel.reverse map { _.mkPretty.reverse }
    val tailRDone = (partsR.tail) flatMap {
      case Nil => Nil
      case (headPR :: tailPR) => s" $headPR $operator" :: addSpaces(tailPR)
    }
    val mid = addSpaces(partsR.head) ++ tailRDone
    Tokens.surroundList(mid.reverse)
  }
}

case class Or(first:Expr, second:Expr) extends Junction(Tokens.OR)

case class And(first:Expr, second:Expr) extends Junction(Tokens.AND)

