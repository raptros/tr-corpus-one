package logic.parsing
import logic.fol
import logic.top.Variable

import scala.util.parsing.combinator.JavaTokenParsers

/**
  * @note instead of handling equality exprssions ever, just treat them as normal predicates!
  */
object BoxerFOLParser extends JavaTokenParsers {
  //grammer first
  def boxerFol:Parser[fol.Expr] = "fol" ~> "(" ~> variable ~> "," ~> expr <~ ")" <~ "."

  def expr:Parser[fol.Expr] = some | all | imp | iff | and | or | not | funct | varExp

  def some = "some" ~> "(" ~> (variable ~ ("," ~> expr)) <~ ")" ^^ {
    case v ~ e => e.exists(v)
  }
  def all = "all" ~> "(" ~> (variable ~ ("," ~> expr)) <~ ")" ^^ {
    case v ~ e => e.all(v)
  }
  def and = "and" ~> "(" ~> expr ~ ("," ~> expr) <~ ")" ^^ {
    case e1 ~ e2 => e1 & e2
  }
  def or = "or" ~> "(" ~> expr ~ ("," ~> expr) <~ ")" ^^ {
    case e1 ~ e2 => e1 | e2
  }
  def imp = "imp" ~> "(" ~> expr ~ ("," ~> expr) <~ ")" ^^ {
    case e1 ~ e2 => e1 -> e2
  }
  def iff = "iff" ~> "(" ~> expr ~ ("," ~> expr) <~ ")" ^^ {
    case e1 ~ e2 => e1 <-> e2
  }

  def not = (notNorm | notOdd) ^^ {
    case n => -n
  }
  def notNorm = "not" ~> "(" ~> expr <~ ")" 
  def notOdd = "-" ~> expr


  def funct = varExp ~ ("(" ~> rep1sep(varExp, ",") <~ ")") ^^ {
    case name ~ vars => (name::vars) map { _.asInstanceOf[fol.Expr] } reduceLeft { _ applyto _ }
  }

  def varExp = variable ^^ (fol.VariableExpr(_))

  def variable:Parser[Variable] = """\w+""".r ^^ {
    case v => Variable(v)
  }
  def quickparse(l:String):fol.Expr = parseAll(expr, l).get

  def extractExpr(stringExp:String):Option[fol.Expr] = try {
    parseAll(expr, stringExp) match {
      case Success(result, _) => Some(result)
      case Error(msg, _) => { None}
      case Failure(msg, _) => { None}
    }
  } catch {
    case (_:Throwable) => None
  }


  //utility functions
  def extractFol(line:String):Option[fol.Expr] = try {
    parseAll(boxerFol, line) match {
      case Success(result, _) => Some(result)
      case Error(msg, _) => { None}
      case Failure(msg, _) => { None}
    }
  } catch {
    case (_:Throwable) => None
  }

  import scala.language.postfixOps
  def findFol(lines:Seq[String]):Option[fol.Expr] = lines flatMap { extractFol(_) } headOption
}

