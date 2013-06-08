package logic.base

import logic.top.Variable

import scalaz.syntax.std.boolean._

trait Atom  { self =>
  //A must be a subtype of Application
  type A <: Application

  //and T must be an Expr with the same T as itself
  type T <: Expr { type T = self.T }

  def apply(pred:Variable, args:Variable*):T = 
    (pred +: args) map { makeVariableExpr(_) } reduceLeft { _ applyto _ }

  protected def makeVariableExpr(v:Variable):T

  def unapplySeq(ae: A): Option[(Variable, Seq[Variable])] = for {
    (pred, args) <- Some(ae.uncurry) if ae.isAtom
    (predBVE, argsLBVE) <- pred.isInstanceOf[VariableExpr] option { 
      pred.asInstanceOf[VariableExpr] -> args.asInstanceOf[List[VariableExpr]] 
    }
  } yield predBVE.variable -> (argsLBVE map { _.variable })
}
