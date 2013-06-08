package logic.cnf
import logic.{fol => f}
import logic.top.Variable

import scalaz.syntax.std.option._
import scalaz.syntax.std.function1._
import scalaz.syntax.id._
import collection.mutable.Buffer

/**
  * List-based representation of predicate logic!
  * (i.e. this should only be used after all quantifiers are removed in converting FOL to CNF)
  */
sealed abstract class FolContainer {
  /** distribute disjunction over conjunction to complete CNF conversion*/
  def toCNF:FolContainer

  /**convert back to an f.Expr (handy)*/
  def toFOLE:f.Expr
  
}

/**basis for or and and lists*/
sealed abstract class FolJunction extends FolContainer {
  def juncts:List[FolContainer] 
}


case class AtomicExpression(exp:f.Expr) extends FolContainer {
  def toCNF:FolContainer = this
  def toFOLE:f.Expr = exp
}

class AndList(val juncts:List[FolContainer]) extends FolJunction {
  def toCNF:FolContainer = AndList(juncts map { _.toCNF })

  def toFOLE:f.Expr = juncts map { _.toFOLE } reduceRight { _ & _ }
}

object AndList {
  def apply(jtemp:List[FolContainer]) = new AndList(juncts(jtemp))

  def unapply(al:AndList) = Some(al.juncts)

  def juncts(jtemp:List[FolContainer]) = {
    val (p1, p2) = jtemp partition { _.isInstanceOf[AndList] }
    val pullIn = p1.asInstanceOf[List[AndList]]
    (pullIn flatMap { _.juncts }) ++ p2
  }
}

class OrList(val juncts:List[FolContainer]) extends FolJunction {
  def toCNF:FolContainer = {
    //take the first conjunction from this disjunction, distribute the other terms into it,
    //and then call toCNF on the new conjunction.
    val conjs:List[AndList] = juncts flatMap { 
      case al:AndList => Some(al)
      case _ => None
    }
    val rest = juncts filterNot { _.isInstanceOf[AndList] }
    if (conjs.isEmpty) OrList(juncts) else inner(conjs, rest)
  }

  def inner(conjs:List[AndList], rest:List[FolContainer]):FolContainer = {
    val (conj :: tailConjs) = conjs
    val nJuncts = conj.juncts map { c => OrList(c :: (tailConjs ++ rest)) }
    AndList(nJuncts).toCNF
  } 

  def toFOLE:f.Expr = juncts map { _.toFOLE } reduceRight { _ | _ }
}

object OrList {
  def apply(jtemp:List[FolContainer]) = new OrList(juncts(jtemp))

  def unapply(ol:OrList) = Some(ol.juncts)

  def juncts(jtemp:List[FolContainer]) = {
    val (p1, p2) = jtemp partition { _.isInstanceOf[OrList] }
    val pullIn = p1.asInstanceOf[List[OrList]]
    (pullIn flatMap { _.juncts }) ++ p2
  }
}


object FolContainer {
  def apply(exp:f.Expr):FolContainer = exp match {
    case f.And(first, second) => AndList(List(FolContainer(first), FolContainer(second)))
    case f.Or(first, second) => OrList(List(FolContainer(first), FolContainer(second)))
    case x => AtomicExpression(x)
  }
  def cnfToLists(fol:f.Expr):List[List[String]] = apply(fol) match {
    case AtomicExpression(exp) => List(List(exp.toString))
    case AndList(juncts) => juncts map { cnfToListsInner(_) }
    case OrList(juncts) => List(juncts map { _.toFOLE.toString })
  }

  def cnfToListsInner(junct:FolContainer):List[String] = junct match {
    case AtomicExpression(exp) => List(exp.toString)
    case (fj:FolJunction) => fj.juncts map { _.toFOLE.toString }
  }


}
