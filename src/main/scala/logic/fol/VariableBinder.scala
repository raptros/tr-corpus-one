package logic.fol

import logic.top.Variable
import logic.{base => be}

abstract class VariableBinder(val operator:String) extends Expr with be.VariableBinder {
  override def toString: String = {
    val (vars, baseterm) = getAllSameScopeBoundVariables
    val vString = vars map { _.name } mkString " "
    s"${operator} ${vString}${Tokens.DOT}${baseterm}"
  }

  override def mkPretty:List[String] = {
    val (vars, baseterm) = getAllSameScopeBoundVariables
    val vString = vars sortWith { _ < _ } map { _.name } mkString " "
    val hereString = s"${operator} ${vString}${Tokens.DOT}"
    hereString::addSpaces(baseterm.mkPretty)
  }
}
