package logic.base

import logic.top.Variable

import scalaz._
import syntax.std.boolean._
import syntax.monoid._

trait VariableExpr extends Expr { 

  val variable: Variable

  override def visit[S](function:T => S, combinator:List[S] => S) = 
    throw new Exception("VariableExpression.visit() is not defined")

  override def visitStructured[S](function: T => S, combinator: List[Any] => S) =
    combinator(List(variable))

  override def replace(other:Variable, expression:T, replace_bound:Boolean=false, alphaConv:Boolean = true) =
    if (variable == other) expression else this

  override def free = (!variable.isConstant) ?? Set(variable) 

  override def constants = (variable.isConstant) ?? Set(variable) 

  override def predicates = Set.empty[Variable]

  override def simplify =  this

  override def toString = variable.name
}
