package trc1

package object logic {
  import utcompling.scalalogic.fol.expression._
  import utcompling.scalalogic.top.expression.Variable

  implicit class BoxerFolFormat(exp:FolExpression) {
    //yes
    def toBoxerFolFormat(id:String):String = formatVariable("fol", Variable(id), exp) + "."

    def toBoxerFolFormat:String = toBoxerFolFormat("1")

    def toBoxerFolFormat(e:FolExpression):String = e match {
      case FolIfExpression(first, second) => formatF2("imp", first, second)
      case FolIffExpression(first, second) => toBoxerFolFormat((first -> second) & (second -> first))
      case FolAllExpression(variable, term) => formatVariable("all", variable, term)
      case FolExistsExpression(variable, term) => formatVariable("some", variable, term)
      case FolAndExpression(first, second) => formatF2("and", first, second)
      case FolEqualityExpression(first, second) => formatF2("eq", first, second)
      case FolOrExpression(first, second) => formatF2("or", first, second)
      case FolNegatedExpression(term) => formatF1("not", term)
      case FolVariableExpression(Variable(name)) => name
      //lambda expressions can't be handled.
      //case FolLambdaExpression(variable, term) => FolLambdaExpression(variable, toBoxerFolFormat(term))
      case app @ FolApplicationExpression(func, arg) => app.toString //spray and pray!
    }

    def formatF1(name:String, exp:FolExpression):String = name + "(" + toBoxerFolFormat(exp) + ")"
    def formatF2(name:String, first:FolExpression, second:FolExpression):String = {
      name + "(" + toBoxerFolFormat(first) + "," + toBoxerFolFormat(second) + ")"
    }
    def formatVariable(name:String, variable:Variable, exp:FolExpression) = name + "(" + variable.name + "," + toBoxerFolFormat(exp) + ")"
  }
}
