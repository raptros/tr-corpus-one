package trc1.logic
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable

import scala.util.parsing.combinator.JavaTokenParsers


object BoxerFOLParser extends JavaTokenParsers {
  //grammer first
  
  def fol:Parser[FolExpression] = "fol" ~> "(" ~> variable ~> "," ~> expr <~ ")" <~ "."

  def expr:Parser[FolExpression] = some | all | imp | iff | and | or | not | eq | funct 

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
  def not = "not" ~> "(" ~> expr <~ ")" ^^ {
    case e => -e
  }
  def funct = varExp ~ ("(" ~> rep1sep(varExp, ",") <~ ")") ^^ {
    case name ~ vars => (name::vars) map { _.asInstanceOf[FolExpression] } reduceLeft { _ applyto _ }
  }
  def eq = "eq" ~> "(" ~> varExp ~ ("," ~> varExp) <~ ")" ^^ {
    case v1 ~ v2 => FolEqualityExpression(v1,v2)
  }

  def varExp = variable ^^ (FolVariableExpression(_))

  def variable:Parser[Variable] = """\w+""".r ^^ {
    case v => Variable(v)
  }
  def quickparse(l:String):FolExpression = parseAll(expr, l).get

  //utility functions
  def extractFol(line:String):Option[FolExpression] = try {
    parseAll(fol, line) match {
      case Success(result, _) => Some(result)
      case Error(msg, _) => { None}
      case Failure(msg, _) => { None}
    }
  } catch {
    case (_:Throwable) => None
  }

  def findFol(lines:Seq[String]):Option[FolExpression] = lines flatMap { extractFol(_) } headOption
}

