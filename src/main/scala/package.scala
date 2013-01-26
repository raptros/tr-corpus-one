package object trc1 {
  import com.nicta.scoobi.Scoobi._
  implicit val trieFmt:WireFormat[RuleTrieC] = mkCaseWireFormat(RuleTrieC, RuleTrieC.unapply _)
}
