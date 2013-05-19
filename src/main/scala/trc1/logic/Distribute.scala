package trc1.logic
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.top.expression.Variable
import scalaz.syntax.std.function1._
import scalaz.syntax.id._
import collection.mutable.Buffer

/**
 * List-based representation of predicate logic!
 * (i.e. this should only be used after all quantifiers are removed in converting FOL to CNF)
 */
sealed abstract class FolContainer {
  /** consolidate lists of junctions */
  //def consolidate:FolContainer
  /** distribute disjunction over conjunction to complete CNF conversion*/
  def toCNF:FolContainer
  /**convert back to an FolExpression (handy)*/
  def toFOLE:FolExpression
  
}

/**basis for or and and lists*/
abstract class FolJunction extends FolContainer {
  def juncts:List[FolContainer] 
  //type Shifting = (List[FolContainer], List[FolContainer])
  //def shiftall:List[FolContainer] = {
    
  //def shift(p:Shifting):Shifting
}

case class AtomicExpression(exp:FolExpression) extends FolContainer {
  def consolidate:FolContainer = this
  def toCNF:FolContainer = this
  def toFOLE:FolExpression = exp
}

case class AndList(jtemp:List[FolContainer]) extends FolJunction {
  val juncts:List[FolContainer] = {
    val (pullIn, nj2) = jtemp partition (_.isInstanceOf[AndList])
    pullIn.asInstanceOf[List[AndList]].map(_.juncts).flatten ++ nj2
  }

  def toCNF:FolContainer = AndList(juncts map (_.toCNF))

  def toFOLE:FolExpression = juncts map (_ toFOLE) reduceRight (_ & _)
}

case class OrList(jtemp:List[FolContainer]) extends FolJunction {
  val juncts:List[FolContainer] = {
    val (pullIn, nj2) = jtemp partition (_.isInstanceOf[OrList])
    pullIn.asInstanceOf[List[OrList]].map(_.juncts).flatten ++ nj2
  }

  def toCNF:FolContainer = {
    //take the first conjunction from this disjunction, distribute the other terms into it,
    //and then call toCNF on the new conjunction.
    val (conjs, rest) = juncts partition (_.isInstanceOf[AndList])
    conjs.headOption.asInstanceOf[Option[AndList]] map { conj =>
      val toDist = conjs.tail ++ rest //this is to be rejoined with an or as it comes from the or
      val mkOr = (container:FolContainer) => (OrList(container::toDist))
      AndList(conj.juncts map mkOr).toCNF
    } getOrElse {
      this //if there are no conjs this is already in cnf
    }
  }

  def toFOLE:FolExpression = juncts map (_ toFOLE) reduceRight (_ | _)
}

object FolContainer {
  def apply(exp:FolExpression):FolContainer = exp match {
    case FolAndExpression(first, second) => AndList(List(FolContainer(first), FolContainer(second)))
    case FolOrExpression(first, second) => OrList(List(FolContainer(first), FolContainer(second)))
    case x => AtomicExpression(x)
  }
  def cnfToLists(fol:FolExpression):List[List[String]] = apply(fol) match {
    case AtomicExpression(exp) => List(List(exp.toString))
    case AndList(juncts) => juncts map (cnfToListsInner(_))
    case OrList(juncts) => List(juncts map (_.toFOLE.toString))
  }

  def cnfToListsInner(junct:FolContainer):List[String] = junct match {
    case AtomicExpression(exp) => List(exp.toString)
    case (fj:FolJunction) => fj.juncts map (_.toFOLE.toString)
  }


}
