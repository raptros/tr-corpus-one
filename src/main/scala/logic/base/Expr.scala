package logic.base

import reflect.runtime.universe._
import logic.top.Variable

import scalaz._
import Traverse._
import std.list._
import std.tuple._
import syntax.traverse._
import syntax.std.boolean._

trait Expr { self =>
  type T <: Expr { type T = self.T }
  
  import scala.language.implicitConversions
  val selfAsT = this.asInstanceOf[T]

  implicit def asT(t:self.type):T = t.asInstanceOf[T]

  def applyto(other:T):T
  
  def visit[S](function: T => S, combinator: List[S] => S):S

  def newT(parts:List[Any]):T = {
    val m = runtimeMirror(getClass.getClassLoader)
    val mirroredSelf = m.reflect(this)
    val mirroredTClass = m.reflectClass(mirroredSelf.symbol)
    val constructT = mirroredSelf.symbol.toType.declaration(nme.CONSTRUCTOR).asMethod
    val mirroredConstructorT = mirroredTClass.reflectConstructor(constructT)
    mirroredConstructorT(parts:_*).asInstanceOf[T]
  }
  
  def makeVariableExpression(variable:Variable):T
  
  def apply(args:T*):T = apply(args.toList)

  def apply(args:List[T]):T = (args foldLeft asT(this)) { (l:T, r:T) => l applyto r }

  def visitStructured[S](function:T => S, combinator:List[Any] => S):S = visit(function, combinator)

  /**
  * Replace every instance of 'variable' with 'expression'
  * @param variable: C{Variable} The variable to replace
  * @param expression: C{Expression} The expression with which to replace it
  * @param replace_bound: C{boolean} Should bound variables be replaced?
  */
  def replace(variable:Variable, expression: T, replaceBound: Boolean = false, alphaConvert: Boolean = true): T = {
    val replacer:T => T = _.replace(variable, expression, replaceBound, alphaConvert)
    visitStructured(replacer, newT _)
  }

  def varFlatten(func:T => Set[Variable]):Set[Variable] = visit[Set[Variable]](func, _.flatten.toSet)

  /**
  * Return a set of all the free (non-bound) variables.  This includes
  * both individual and predicate variables, but not constants.
  */
  def free:Set[Variable] = varFlatten { _.free }

  /**
  * Return a set of individual constants (non-predicates).
  */
  def constants:Set[Variable] = varFlatten { _.constants }

  /**
  * Return a set of predicates (constants, not variables).
  */
  def predicates:Set[Variable] = varFlatten { _.predicates }

  /**
  * Return beta-converted expression
  */
  def simplify:T = visitStructured[T]( _.simplify, newT _ )

  def getIndividualVars(e:T):Set[Variable] = e match {
    case v:VariableExpr => (v.variable.isIndVar) ?? Set(v.variable)
    case _ => e varFlatten { getIndividualVars(_) }
  }
  
  def getZippedVars(e:T):List[(Variable, Int)] = getIndividualVars(e).toList.sorted.zipWithIndex


  /**
  * Rename auto-generated unique variables
  */
  def normalize: T = (getZippedVars(asT(this)) foldLeft asT(this)) { case (res, (v, i)) =>
    val newVar = if (v.isEventVar) Variable(s"e0${i+1}") else if (v.isIndVar) Variable(s"z${i+1}") else v
    res.replace(v, makeVariableExpression(newVar), true)
  }

  /**
  * Is this expression an atom?
  */
  def isAtom = false
}
