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

object ElimImplication extends Conversion {
  @strictfp
  def apply(fol:f.Expr):VExpr = fol match {
    //the crux.
    case f.If(first, second) => (apply(first) |@| apply(second)) { (f, s) => -f | s }
    case f.Iff(first, second) => (apply(first) |@| apply(second)) { (f, s) => (-f | s) & (-s | f) }
    //just keep searching
    case f.All(variable, term) => apply(term) map { _ all variable }
    case f.Exists(variable, term) => apply(term) map { _ exists variable }
    case f.And(first, second) => (apply(first) |@| apply(second)) { _ & _ }
    case f.Or(first, second) => (apply(first) |@| apply(second)) { _ | _ }
    case f.Negated(term) => apply(term) map { -_ }
    //everything at this point is atoms, which makes for the base case
    case fApp:f.Application => fApp.success
    case varexp:f.VariableExpr => varexp.success
    //these constitute failures
    case _:f.Equality => "can't do anything with equality expressions!".fail
    case _:f.Lambda => "can't do anything with lambda expressions!".fail
    case _ => "wtf just got passed in?!".fail
  }
}

