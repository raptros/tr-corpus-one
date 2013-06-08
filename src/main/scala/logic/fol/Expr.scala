package logic.fol

import logic.top.Variable
import logic.top.Expression
import logic.{base => be}

import logic.util._

abstract class Expr extends Expression with be.Expr {
  type T = Expr

  def unary_- : Expr = Negated(this)
  
  def &(other:Expr):Expr = And(this, other)
  
  def |(other:Expr):Expr = Or(this, other)
  
  def ->(other:Expr):Expr = If(this, other)
  
  def <->(other: Expr):Expr = Iff(this, other)

  def all(vars:Variable*):Expr = this all vars
  
  def all(vars:TraversableOnce[Variable]):Expr = (vars foldRight this) { All(_, _) }
  
  def exists(vars:Variable*):Expr = this exists vars
  
  def exists(vars:TraversableOnce[Variable]):Expr = (vars foldRight this) { Exists(_, _) }
  
  def lambda(vars:Variable*):Expr = this lambda vars
  
  def lambda(vars:TraversableOnce[Variable]):Expr = (vars foldRight this) { Lambda(_, _) } 

  override def applyto(other:Expr):Expr = Application(this, other)

  override def makeVariableExpression(variable:Variable):Expr = VariableExpr(variable)

  def pprint() {
    println(pretty)
  }

  def pretty = mkPretty mkString "\n"

  def mkPretty:List[String] = List(toString)

  def addSpaces(ls:List[String]) = ls map { l => " " + l }
}
