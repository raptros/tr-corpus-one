package logic

package object base {
  import logic.top.Variable

  import scalaz.Monoid
 
  implicit def varSetMonoid:Monoid[Set[Variable]] = new Monoid[Set[Variable]] {
    def zero = Set.empty[Variable]
    def append(s1:Set[Variable], s2: => Set[Variable]):Set[Variable] = s1 union s2
  }

  trait Lambda extends VariableBinder

  trait Negated extends Expr {
    val term: T
    def visit[S](function:T => S, combinator:List[S] => S) = combinator(List(function(term)))
  }

}

