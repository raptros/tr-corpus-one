package logic.cnf
import logic.{fol => f}
import logic.top.Variable

/**
  * Takes an f.Expr and transforms it into Conjunction Normal Form.
  * Steps: 
  * -eliminate implications
  * -push negations to atom level
  * -skolemize
  * -drop universals
  * -distribute all disjunctions over conjunctions
  */
object ConvertToCNF extends FixVariableNames with ElimImplication with PushNegation with Skolemize with DropUniversal with
DistributeOrOverAnd {
  import scalaz.syntax.std.function1._
  import scalaz.syntax.id._
  def apply(fol:f.Expr)(fix:(String => String)):Option[f.Expr] = {
    //do not apply normalization - instead assume that at this point anything from 
    fol |> (fixVariableNames(fix)(_)) |> (elimImplication(_)) |> (pushNegation(_)) |> (skolemize(_)) |> (dropUniversal(_)) |> (distributeOrOverAnd(_))
  }
}

trait FixVariableNames {
  def fixVariableNames(fix:(String => String))(fol:f.Expr):f.Expr = {
    val fixV = fixVariableNames(fix)(_)
    val rename = renameVariable(fix)(_, _)
    fol match {
      case f.If(first, second) => fixV(first) -> fixV(second)
      case f.Iff(first, second) => fixV(first) <-> fixV(second)
      //just keep searching
      case f.All(variable, term) => rename(variable, term) match { case (nVar, nTerm) => fixV(nTerm) all nVar }
      case f.Exists(variable, term) => rename(variable, term) match { case (nVar, nTerm) => fixV(nTerm) exists nVar }
      case f.And(first, second) => fixV(first) & fixV(second)
      //case f.Equality(first, second) => f.Equality(fixV(first), fixV(second))
      case f.Or(first, second) => fixV(first) | fixV(second)
      case f.Negated(term) => -fixV(term)
      case (varexp:f.VariableExpr) => varexp
      //pretty sure these are useless
      case f.Lambda(variable, term) => f.Lambda(variable, fixV(term))
      case f.Application(func, arg) => func applyto fixV(arg)
    }
  }
  def renameVariable(fix:(String => String))(variable:Variable, term:f.Expr):(Variable, f.Expr) = {
    val newVar = Variable(fix(variable.name))
    (newVar, term.replace(variable, f.VariableExpr(newVar)))
  }
}

trait ElimImplication {
  def elimImplication(fol:f.Expr):f.Expr = fol match {
    //the crux.
    case f.If(first, second) => -elimImplication(first) | elimImplication(second)
    case f.Iff(first, second) => elimImplication(first -> second) & elimImplication(second -> first)
    //just keep searching
    case f.All(variable, term) => f.All(variable, elimImplication(term))
    case f.Exists(variable, term) => f.Exists(variable, elimImplication(term))
    case f.And(first, second) => elimImplication(first) & elimImplication(second)
    //case f.Equality(first, second) => f.Equality(elimImplication(first), elimImplication(second))
    case f.Or(first, second) => elimImplication(first) | elimImplication(second)
    case f.Negated(term) => -elimImplication(term)
    case f.VariableExpr(variable) => f.VariableExpr(variable)
    //pretty sure these are useless
    case f.Lambda(variable, term) => f.Lambda(variable, elimImplication(term))
    case f.Application(func, arg) => f.Application(elimImplication(func), elimImplication(arg))
  }
}

trait PushNegation {
  def findNegation(fol:f.Expr):f.Expr = fol match {
    case f.All(variable, term) => f.All(variable, findNegation(term))
    case f.Exists(variable, term) => f.Exists(variable, findNegation(term))
    //demorgan
    case f.And(first, second) => findNegation(first) & findNegation(second)
    case f.Or(first, second) => findNegation(first) | findNegation(second)
    //???
    //case f.Equality(first, second) => f.Equality(findNegation(first), findNegation(second))
    //start pushing down the negation
    case f.Negated(term) => moveNegation(term)
    //leave atoms alone
    case f.Application(func, arg) => f.Application(func, arg)
    case f.Lambda(variable, term) => f.Lambda(variable, term)
    case f.VariableExpr(variable) => f.VariableExpr(variable)
  }
  def moveNegation(fol:f.Expr):f.Expr = fol match {
    case f.All(variable, term) => f.Exists(variable, moveNegation(term)) //all to exists
    case f.Exists(variable, term) => f.All(variable, moveNegation(term)) //exists to all
    //demorgan
    case f.And(first, second) => moveNegation(first) | moveNegation(second) // and to or (by DeM)
    case f.Or(first, second) => moveNegation(first) & moveNegation(second) //or to and (by DeM)
    //???
    //case f.Equality(first, second) => f.Equality(moveNegation(first), moveNegation(second))
    //double negation elimination
    case f.Negated(term) => findNegation(term)
    //leave atoms alone
    case f.Lambda(variable, term) => -f.Lambda(variable, term)
    case f.Application(func, arg) => -f.Application(func, arg)
    case f.VariableExpr(variable) => -f.VariableExpr(variable)
  }

  def pushNegation(fol:f.Expr):f.Expr = findNegation(fol)
}

/** Applies skolemization to FOL expressions.
  * Assumptions: variable names standardized, implications removed, and negations pushed down.
  */
trait Skolemize {
  def skolemize(fol:f.Expr):f.Expr = skolemizeInner(fol, Nil)
    
  def skolemizeInner(fol:f.Expr, varList:List[Variable]):f.Expr = fol match {
    //ok here we need to start tracking the stuff
    case f.All(variable, term) => f.All(variable, skolemizeInner(term, variable::varList))
    case f.Exists(variable, term) => skolemizeInner(term.replace(variable, constructFunc(variable, varList)), varList)
    //just keep looking
    case f.And(first, second) => skolemizeInner(first, varList) & skolemizeInner(second, varList)
    case f.Or(first, second) => skolemizeInner(first, varList) | skolemizeInner(second, varList)
    //still not sure what to do about these. get rid of them?
    //case f.Equality(first, second) => f.Equality(skolemizeInner(first, varList), skolemizeInner(second, varList))
    //application needs to be worked on...
    case f.Application(func, exp) => func applyto skolemizeInner(exp, varList)
    //leave anything here at the atomic level alone
    case e:f.Lambda => e 
    case e:f.Negated => e //while generally we should search on, these will already be at bottom.
    case e:f.VariableExpr => e
  }

  def constructFunc(existVar:Variable, varList:List[Variable]):f.Expr = {
    val evve:f.Expr = f.VariableExpr(existVar)
    (varList map { evve makeVariableExpression _ } foldLeft evve) { _ applyto _ } 
  }
}

/** drops universals. assumes everything above has been done
  *
  */
trait DropUniversal {
  def dropUniversal(fol:f.Expr):f.Expr = fol match {
    case f.All(variable, term) => dropUniversal(term)
    case f.And(first, second) => dropUniversal(first) & dropUniversal(second)
    case f.Or(first, second) => dropUniversal(first) | dropUniversal(second)
    //still not sure what to do about these. get rid of them?
    //case f.Equality(first, second) => f.Equality(dropUniversal(first), dropUniversal(second))
    //leave anything here at the atomic level alone
    case e:f.Lambda => e 
    case e:f.Application => e
    case e:f.Negated => e //while generally we should search on, these will already be at bottom.
    case e:f.VariableExpr => e
  }
}

trait DistributeOrOverAnd {
  def distributeOrOverAnd(fol:f.Expr):Option[f.Expr] = try {
    Some(FolContainer(fol).toCNF.toFOLE)
  } catch {
    case (st:StackOverflowError) => {println("failed to convert " + fol.toString + " to cnf bc stack overflow"); None}
    case (t:Throwable) => {println("failed to convert " + fol.toString + " to cnf"); None}
  }
}

