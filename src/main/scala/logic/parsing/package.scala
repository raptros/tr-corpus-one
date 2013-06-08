package logic

package object parsing {
  import logic.fol
  import logic.top.Variable
 
  /** provides implicit converter to represent logic in fol string format, so that arbitrary expressions can later be read back in. */
  implicit class BoxerFolFormat(exp:fol.Expr) {
    //yes
    def toBoxerFolFormat(id:String):String = formatVariable("fol", Variable(id), exp) + "."

    def toBoxerFolFormat:String = toBoxerFolFormat("1")

    def toBoxerFolFormat(e:fol.Expr):String = e match {
      case fol.If(first, second) => formatF2("imp", first, second)
      case fol.Iff(first, second) => toBoxerFolFormat((first -> second) & (second -> first))
      case fol.All(variable, term) => formatVariable("all", variable, term)
      case fol.Exists(variable, term) => formatVariable("some", variable, term)
      case fol.Equality(first, second) => formatF2("eq", first, second) //though we will hopefully never get these
      case fol.And(first, second) => formatF2("and", first, second)
      case fol.Or(first, second) => formatF2("or", first, second)
      case fol.Negated(term) => formatF1("not", term)
      case fol.VariableExpr(Variable(name)) => name
      case app @ fol.Application(func, arg) => app.toString

      //lambda expressions can't be handled.
      //case FolLambdaExpression(variable, term) => FolLambdaExpression(variable, toBoxerFolFormat(term))
      
    }

    def formatF1(name:String, exp:fol.Expr):String = name + "(" + toBoxerFolFormat(exp) + ")"
    def formatF2(name:String, first:fol.Expr, second:fol.Expr):String = {
      name + "(" + toBoxerFolFormat(first) + "," + toBoxerFolFormat(second) + ")"
    }
    def formatVariable(name:String, variable:Variable, exp:fol.Expr) = name + "(" + variable.name + "," + toBoxerFolFormat(exp) + ")"
  }
}
