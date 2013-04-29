package trc1

package object resolution {
  import scala.collection._
  import scala.util.matching.Regex
  import scala.util.control.Exception._

  type Substitution = mutable.Map[String,String]
  def newSubstitution = mutable.Map[String,String]()
  def newSubstitutionInit(values:(String,String)*) :Substitution = {
    newSubstitution ++ values
  }
}
