package logic.base

import annotation.tailrec
import logic.top.Variable

import scalaz.syntax.std.boolean._

trait VariableBinder extends Expr { self =>
  val operator: String
  val variable: Variable
  val term: T

  /**
   * Rename all occurrences of the variable introduced by this variable binder in the expression to @C{newvar}.
   */
  def alphaConvert(newvar:Variable):T = newT(List(newvar, term.replace(variable, makeVariableExpression(newvar), true)))

  override def visit[S](function: T => S, combinator: List[S] => S) = combinator(List(function(term)))

  override def visitStructured[S](function: T => S, combinator: List[Any] => S) = combinator(List(variable, function(term)))

  def replaceOnBound(v:Variable, e:T, replaceBound:Boolean, doAlphaConv:Boolean):T = if (!replaceBound) this else {
    val eBVE = e.isInstanceOf[VariableExpr] option { e.asInstanceOf[VariableExpr] }
    require(eBVE.nonEmpty, s"${e} is not an AbstractVariableExpression")
    newT(List(eBVE.get.variable, term.replace(v, e, true, doAlphaConv)))
  }

  def replaceOtherwise(v:Variable, e:T, replaceBound:Boolean, doAlphaConv:Boolean):T = {
    // if the bound variable appears in the expression, then it must be alpha converted to avoid a conflict
    val converted = if (doAlphaConv && (e.free contains variable))
      alphaConvert(Variable.unique(variable)).asInstanceOf[VariableBinder]
    else this
    // replace in the term
    newT(List(converted.variable, term.replace(v, e, replaceBound, doAlphaConv)))
  }

  override def replace(v:Variable, e:T, replaceBound:Boolean = false, doAlphaConv:Boolean = true):T =  
    if (variable == v) replaceOnBound(v, e, replaceBound, doAlphaConv) else replaceOtherwise(v, e, replaceBound, doAlphaConv)

  override def free = term.free - variable

  @tailrec 
  final def collectSame(found:List[Variable], cur:T):(List[Variable], T) = cur match {
    case e:VariableBinder if (e.operator == operator) => collectSame(e.variable::found, e.term)
    case _ => (found -> cur)
  }

  def getAllSameScopeBoundVariables:(List[Variable], T) = collectSame(Nil, this)
}
