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

class FixVariableNames(fix:String => String) extends Conversion {
  @strictfp
  def apply(fol:f.Expr):VExpr = fol match {
    //these are where renaming actually will occur.
    case f.All(variable, term) => rename(variable, term) { _ all _ }
    case f.Exists(variable, term) => rename(variable, term) { _ exists _ }
    //continue cases
    case f.If(first, second) => (apply(first) |@| apply(second)) { _ --> _ }
    case f.Iff(first, second) => (apply(first) |@| apply(second)) { _ <-> _ }
    case f.And(first, second) => (apply(first) |@| apply(second)) { _ & _ }
    case f.Or(first, second) => (apply(first) |@| apply(second)) { _ & _ }
    case f.Negated(term) => apply(term) map { -_ }
    //everything at this point is atoms, which makes for the base case
    case fApp:f.Application => fApp.success
    case varexp:f.VariableExpr => varexp.success
    //these constitute failures
    case _:f.Equality => "can't do anything with equality expressions!".fail
    case _:f.Lambda => "can't do anything with lambda expressions!".fail
    case _ => "wtf just got passed in?!".fail
  }

  @strictfp
  def rename(variable:Variable, term:f.Expr)(combine:(f.Expr, Variable) => f.Expr):VExpr = {
    val newVar = Variable(fix(variable.name))
    val newTermV = apply(term.replace(variable, f.VariableExpr(newVar)))
    newTermV map { nt =>  combine(nt, newVar) }
  }
}
 
