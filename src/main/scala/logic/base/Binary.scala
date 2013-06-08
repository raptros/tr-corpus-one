package logic.base

import logic.top.Variable
import annotation.{tailrec, switch}

trait Binary extends Expr { 
  val first:T
  val second:T
  val operator: String

  def visit[S](function: T => S, combinator: List[S] => S) =
    combinator(List(function(first), function(second)))

  override def toString = Tokens.surround(s"${first} ${operator} ${second}")
  
  def collectExpand(foundR:List[T], cur:T, expand:List[T]):(List[T], List[T]) = cur match {
    case e:Binary if (e.operator == operator) => foundR -> (e.first :: e.second :: expand)
    case _ => (cur::foundR) -> expand
  }

  @tailrec
  final def collectL(foundR:List[T], expand:List[T]):List[T] = (expand: @switch) match {
    case Nil => foundR.reverse
    case head :: tail => {
      val (nFoundR, nExpand) = collectExpand(foundR, head, tail)
      collectL(nFoundR, nExpand)
    }
  }

  def collect(start:T):List[T] = collectL(Nil, List(start))

  def allConnectedSameLevel:List[T] = collect(this)
}
