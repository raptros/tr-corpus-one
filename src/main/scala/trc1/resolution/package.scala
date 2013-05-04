package trc1

package object resolution {
  import scala.collection._
  import scala.util.matching.Regex
  import scala.util.control.Exception._
  
  case class InferenceRuleFinal(quantifiers:List[(String, Set[String])],
    lhs:List[String], rhs:List[String])

  def finalizeInference(ir:InferenceRule):InferenceRuleFinal = {
    InferenceRuleFinal(ir.quantifiers, ir.lhs, ir.rhs)
  }

  def compIRFs(first:InferenceRuleFinal, second:InferenceRuleFinal):Boolean = {
    (first.lhs == second.lhs) && (first.rhs == second.rhs)
  }


  type Substitution = mutable.Map[String,String]
  def newSubstitution = mutable.Map[String,String]()
  def newSubstitutionInit(values:(String,String)*) :Substitution = {
    newSubstitution ++ values
  }
}
