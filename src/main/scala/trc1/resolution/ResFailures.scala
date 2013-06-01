package trc1.resolution


sealed abstract trait ResFailure {
  def msg:String
}

case class EmptyClauseFailure(msg:String) extends ResFailure

case class UnexpectedFormatOfFormulaFailure(msg:String) extends ResFailure

case class ShouldNotBeHereFailure(where:String, withWhat:String) extends ResFailure {
  val msg = "should not be here: " + where + " " + withWhat:String
}

