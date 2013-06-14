package logic

package object cnf {
  import logic.fol
  import scalaz.Validation
  type VExpr = Validation[String, fol.Expr]
}
