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

object Skolemize extends Conversion {
  @strictfp
  def apply(fol:f.Expr):VExpr = sk(fol, Nil)

  @strictfp
  def sk(fol:f.Expr, vars:List[Variable]):VExpr = fol match {
    //every universal quantifiers adds a parameter to the funcs for the existentials below
    case f.All(variable, term) => sk(term, variable::vars) map { _ all variable }
    //replace every instance of variable with the constant function of the universals above
    case f.Exists(variable, term) => sk(term.replace(variable, mkFunc(variable, vars)), vars)
    //just search for quantifiers
    case f.And(first, second) => (sk(first, vars) |@| sk(second, vars)) { _ & _ }
    case f.Or(first, second) => (sk(first, vars) |@| sk(second, vars)) { _ | _ }
    //atoms. won't be any quantifiers below this.
    case neg:f.Negated => neg.success
    case fApp:f.Application => fApp.success
    case varexp:f.VariableExpr => varexp.success
    //these constitute failures
    case _:f.Equality => "can't do anything with equality expressions!".fail
    case _:f.Lambda => "can't do anything with lambda expressions!".fail
    case _:f.If => "implications should have already been removed!".fail
    case _:f.Iff => "equivalences should have already been removed!".fail
    case _ => "wtf just got passed in?!".fail
  }

  @strictfp
  def mkFunc(existVar:Variable, varList:List[Variable]):f.Expr = {
    val evve:f.Expr = f.VariableExpr(existVar)
    (varList map { evve makeVariableExpression _ } foldLeft evve) { _ applyto _ } 
  }
}


