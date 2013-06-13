package logic.cnf
import logic.{fol => f}
import logic.top.Variable

import scalaz._
import Validation._
import syntax.validation._
import syntax.apply._
import std.string._
import std.anyVal._


import annotation.strictfp

object PushNegation extends Conversion {
  @strictfp
  def apply(fol:f.Expr):VExpr = push(false, fol)

  @strictfp
  def push(neg:Boolean, fol:f.Expr):VExpr = fol match {
    //strictfpe the sign
    case f.Negated(term) => push(!neg, term)
    //demorgan
    case f.And(first, second) => (push(neg, first) |@| push(neg, second)) { (f, s) => if (neg) f | s else f & s }
    case f.Or(first, second) => (push(neg, first) |@| push(neg, second)) { (f, s) => if (neg) f & s else f | s }
    //otherwise search 
    case f.All(variable, term) => push(neg, term) map { _ all variable }
    case f.Exists(variable, term) => push(neg, term) map { _ exists variable }
    //atoms take on the sign of neg
    case fApp:f.Application => negate(neg, fApp).success
    case varexp:f.VariableExpr => negate(neg, varexp).success
    //these constitute failures
    case _:f.Equality => "can't do anything with equality expressions!".fail
    case _:f.Lambda => "can't do anything with lambda expressions!".fail
    case _:f.If => "implications should have already been removed!".fail
    case _:f.Iff => "equivalences should have already been removed!".fail
    case _ => "wtf just got passed in?!".fail
  }

  @strictfp
  def negate(neg:Boolean, fol:f.Expr) = if (neg) -fol else fol
}

