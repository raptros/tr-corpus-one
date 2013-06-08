package logic.fol

import logic.top.Variable
import logic.{base => be}

object Atom extends be.Atom { self =>
  type T = Expr
  type A = Application 
  protected def makeVariableExpr(v:Variable):T = VariableExpr(v)
}
