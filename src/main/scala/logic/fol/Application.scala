package logic.fol

import logic.top.Variable
import logic.{base => be}

case class Application(function:Expr, argument:Expr) extends Expr with be.Application {
  def visit[S](func:Expr => S, combinator:List[S] => S) = combinator(List(func(function), func(argument)))

  override def simplify:Expr = {
    val func = function.simplify
    val arg = argument.simplify
    func match {
      case le:Lambda => le.term.replace(le.variable, arg, false, true).simplify
      case _ => Application(func, arg)
    }
  }
}
