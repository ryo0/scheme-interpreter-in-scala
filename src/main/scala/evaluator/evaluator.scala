package evaluator

import parser.ast.ast._

object evaluator {
  def evalExp(exp: Exp): Datum = {
    exp match {
      case Num(_) | Str(_) | Bool(_) =>
        exp.asInstanceOf[Datum]
      case IfExp(cond, t, f) =>
        evalIf(IfExp(cond, t, f))
    }
  }

  def evalIf(exp: IfExp): Datum = {
    if (evalExp(exp.cond) == Bool(true)) {
      evalExp(exp.trueExp)
    } else {
      exp.falseExp match {
        case Some(exp) => evalExp(exp)
        case None      => Bool(false)
      }
    }
  }
}
