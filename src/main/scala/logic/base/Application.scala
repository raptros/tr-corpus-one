package logic.base

import logic.top.Variable

import scalaz._
import syntax.std.boolean._

trait Application extends Expr { 
  val function:T
  val argument:T

  /** Uncurry this application expression */
  def uncurry:(T, List[T]) = (function) match {
    case ae:Application => {
      val (f, a):(T, List[T]) = ae.uncurry
      (f, a :+ argument)
    }
    case _ => (function, List(argument))
  }

  /** Return uncurried base-function.*/
  val pred:T = uncurry._1

  /** Return uncurried arg-list */
  val args:List[T] = uncurry._2

  /** Is this expression an atom (as opposed to a lambda expression applied  to a term)? */
  override def isAtom = pred.isInstanceOf[VariableExpr]

  override def constants: Set[Variable] = {
    val functionConstants = (!function.isInstanceOf[VariableExpr]) ?? function.constants
    functionConstants ++ argument.constants
  }

  override def predicates:Set[Variable] = {
    val functionPreds = function match {
      case v:VariableExpr =>  (v.variable.isConstant) ?? Set(v.variable)
      case _ => function.predicates
    }
    functionPreds ++ argument.predicates
  }

  override def toString:String = {
    //either: uncurry the arguments and find the base function, or leave the function uncurried
    val (func, argStr) = if (isAtom) pred -> (args mkString ", ") else function -> argument.toString
    val functionStr = if (shouldParenthesizeFunction(func)) Tokens.surround(func.toString) else func.toString
    s"${functionStr}${Tokens.surround(argStr)}"
  }

  protected def shouldParenthesizeFunction(function:T): Boolean = function match {
    case ble:Lambda => ble.term match {
      case (baeT:Application) => !baeT.function.isInstanceOf[VariableExpr]
      case _ => !ble.term.isInstanceOf[Binary]
    }
    case bae:Application => true
    case _ => false
  }

}
