package trc1

package object resolution {
  import scala.collection._
  import scala.util.matching.Regex
  import scala.util.control.Exception._
  
  //import scalaz._
  //import Validation._

  case class InferenceRuleFinal(quantifiers:List[(String, Set[String])],
    lhs:List[String], rhs:List[String])

  def finalizeInference(ir:InferenceRule):InferenceRuleFinal = {
    InferenceRuleFinal(ir.quantifiers, ir.lhs, ir.rhs)
  }

  def compIRFs(first:InferenceRuleFinal, second:InferenceRuleFinal):Boolean = {
    (first.lhs == second.lhs) && (first.rhs == second.rhs)
  }

  //type ResVal[A] = Validation[ResFailure, A]

  //----------------------------------------------------------
  // a substitution is a mapping from variables to terms

  type Substitution = Map[String,String]
  def newSubstitution = Map[String,String]()
  def newSubstitutionInit(values:(String,String)*) :Substitution = values.toMap

  //----------------------------------------------------------
  // custom exception:
  // In our case, we don't want to derive the empty clause,
  // as that means that G follows from F logically, without 
  // application of any rewriting rule
  case class EmptyClauseException(data:String) extends Exception

  // another custom exception:
  // unexpected format in formula
  case class UnexpectedFormatOfFormulaException(data:String) extends Exception

}
