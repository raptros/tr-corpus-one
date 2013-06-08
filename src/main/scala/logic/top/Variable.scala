package logic.top

import logic.util.Counter

import scalaz.syntax.id._

case class Variable(val name:String) extends Ordered[Variable] {
  def compare(that:Variable):Int = name compare that.name
  def isIndVar = Variable.isIndVar(name)
  def isFuncVar = Variable.isFuncVar(name)
  def isEventVar = Variable.isEventVar(name)
  def isConstant = !isFuncVar && !isIndVar && !isEventVar
}

object Variable {
  private val counter = new Counter
  private val INDVAR_RE = """^[a-df-z]\d*$""".r
  private val FUNCVAR_RE = """^[A-Z]\d*$""".r
  private val EVENTVAR_RE = """^e\d*$""".r
  
  def isIndVar(expr:String):Boolean = INDVAR_RE.findFirstIn(expr).nonEmpty

  def isFuncVar(expr:String):Boolean = FUNCVAR_RE.findFirstIn(expr).nonEmpty

  def isEventVar(expr:String):Boolean = EVENTVAR_RE.findFirstIn(expr).nonEmpty

  def unique(pattern:Variable):Variable = unique(pattern, Set.empty[Variable])

  def unique(pattern:Variable, exclude:Set[Variable]):Variable = {
    val prefix = pattern.name match {
      case INDVAR_RE() => "z"
      case FUNCVAR_RE() => "F"
      case EVENTVAR_RE() => "e0"
      case _ => "z"
    }
    unique(prefix, exclude)
  }

  def mkNext(prefix:String):String = {
    val c = counter.get()
    s"${prefix}${c}"
  }
  
  def unique(prefix:String):Variable = unique(prefix, Set.empty[Variable])

  //FIXME the behavior of all this is WILDLY unpredicateble!
  def unique(prefix:String, exclude:Set[Variable]):Variable = {
    Variable(mkNext(prefix)) doWhile(v => Variable(mkNext(prefix)), (exclude contains _))
  }


  class generator(prefix: String, exclude: Set[Variable]) {
    def get() = Variable.unique(prefix, exclude)
  }
}
