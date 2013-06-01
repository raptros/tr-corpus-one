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

  /** a substitution is a mapping from variables to terms */
  type Substitution = Map[String,String]
  def newSubstitution = Map[String,String]()
  def newSubstitutionInit(values:(String,String)*) :Substitution = values.toMap

  /** deriving the empty class is an error - G is not supposed to follow from F without any rewriting rule */
  case class EmptyClauseException(data:String) extends Exception

  /** unexpected format in formula */
  case class UnexpectedFormatOfFormulaException(data:String) extends Exception

  /** an attempt to perform some action with some data where it cannot be done sensibly */
  case class ShouldNotBeHereException(where:String, what:String) extends Exception {
    val data = s"should not be here: ${where} with ${what}"
  }

}
