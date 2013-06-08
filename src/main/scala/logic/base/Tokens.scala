package logic.base

trait Tokens {

    val LAMBDA = "\\"; val LAMBDA_LIST = List("\\")

    def surround(s:String) = s"${OPEN}${s}${CLOSE}"

    //Punctuation
    val DOT = "."
    val OPEN = "("
    val CLOSE = ")"
    val COMMA = ","

    //Operations
    val NOT = "-"; val NOT_LIST = List("not", "-", "!")
    val AND = "&"; val AND_LIST = List("and", "&", "^")
    val OR = "|"; val OR_LIST = List("or", "|")
    val IMP = "->"; val IMP_LIST = List("implies", "->", "=>")
    val LIMP = "<-"; val LIMP_LIST = List("<-", "<=")
    val IFF = "<->"; val IFF_LIST = List("iff", "<->", "<=>")
    val EQ = "="; val EQ_LIST = List("=", "==")
    val NEQ = "!="; val NEQ_LIST = List("!=")
    
    val tokenFilter = """^[-\\\.\[\](),!&^|>=<\+:]+$""".r

    protected def makeSymbolList(tokenList:Iterable[String]) = 
      tokenList filter { s => (tokenFilter findFirstIn s).isDefined }

}

object Tokens extends Tokens
