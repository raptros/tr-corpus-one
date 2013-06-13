package logic.cnf
import logic.{fol => f}
import logic.top.Variable



import scalaz._
import Validation._
import syntax.validation._
import syntax.apply._
import syntax.id._
import syntax.bind._
import syntax.monad._
import syntax.std.function1._
import std.anyVal._
import std.string._


trait Conversion {
  type VExpr = Validation[String, f.Expr]
  def apply(fol:f.Expr):VExpr
}

/**
  * Takes an f.Expr and transforms it into Conjunction Normal Form.
  * Steps: 
  * -eliminate implications
  * -push negations to atom level
  * -skolemize
  * -drop universals
  * -distribute all disjunctions over conjunctions
  */
object ConvertToCNF {
  type VExpr = Validation[String, f.Expr]
 
  def apply(fol:f.Expr)(fix:(String => String)):Option[f.Expr] = {
    val cnf = (runners(fix) foldLeft success[String,f.Expr](fol)) { _ flatMap _ }
    cnf.swap foreach { m => println(s"conversion to cnf failed because: ${m}") }
    cnf.toOption
  }

  type Convert = f.Expr => VExpr

  import scala.language.implicitConversions
  implicit def toF(c:Conversion):Convert = (fol:f.Expr) => c(fol)
    
  def runners(fix:String => String):List[Convert] = (new FixVariableNames(fix))::oRunners

  val oRunners:List[Convert] = List(ElimImplication, PushNegation, Skolemize, DropUniversal, DistributeOrOverAnd)

}
