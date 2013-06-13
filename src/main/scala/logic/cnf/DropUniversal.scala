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

object DropUniversal extends Conversion {
  @strictfp
  def apply(fol:f.Expr):VExpr = fol match {
    //these get dropped
    case f.All(variable, term) => apply(term)
    //just search for universals to drop
    case f.And(first, second) => (apply(first) |@| apply(second)) { _ & _ }
    case f.Or(first, second) =>  (apply(first) |@|  apply(second)) { _ | _ }
    //atoms. won't be any universals in these
    case neg:f.Negated => neg.success
    case fApp:f.Application => fApp.success
    case varexp:f.VariableExpr => varexp.success
    //these constitute failures
    case _:f.Exists => "formula must be skolemized before universals can be dropped!".fail
    case _:f.Equality => "can't do anything with equality expressions!".fail
    case _:f.Lambda => "can't do anything with lambda expressions!".fail
    case _:f.If => "implications should have already been removed!".fail
    case _:f.Iff => "equivalences should have already been removed!".fail
    case _ => "wtf just got passed in?!".fail
  }
}



