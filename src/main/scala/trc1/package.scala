package object trc1 {
  import com.nicta.scoobi.Scoobi._

  import logic.fol.{Expr => FExpr}
  import logic.top.Variable
  import logic.parsing.{BoxerFOLParser, BoxerFolFormat}
  import logic.resolution.InferenceRuleFinal

  val CANDC_HOME = "CANDC_HOME"
  val CANDC_INSTANCE_COUNT = "CANDC_INSTANCE_COUNT"


  val mSep = "<<<@>>>"

}
