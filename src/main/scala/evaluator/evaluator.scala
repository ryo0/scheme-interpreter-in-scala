package evaluator

import parser.ast.ast._

object evaluator {
  def evalProgram(program: Program, env: List[Map[Symbol, Datum]]): Datum = {
    val data = program.p.map { p: Form =>
      {
        p match {
          case exp: Exp => evalExp(exp, env)
//          case defStmt: DefineStatement =>
        }
      }
    }
    data.last
  }

  def extendEnv(params: List[Symbol],
                values: List[Datum],
                env: List[Map[Symbol, Datum]]): List[Map[Symbol, Datum]] = {
    var newEnv = Map[Symbol, Datum]()
    for (i <- params.indices) {
      newEnv = newEnv + (params(i) -> values(i))
    }
    newEnv :: env
  }

  def findValueFromEnv(symbol: Symbol, env: List[Map[Symbol, Datum]]): Option[Datum] = {
    var result: Option[Datum] = None
    for (envMap <- env) {
      val value = envMap.get(symbol)
      value match {
        case Some(_) =>
          result = value
        case _ =>
      }
    }
    result
  }

  def evalExp(exp: Exp, env: List[Map[Symbol, Datum]]): Datum = {
    exp match {
      case Num(_) | Str(_) | Bool(_) | Op(_) =>
        exp.asInstanceOf[Datum]
      case Symbol(n) =>
        findValueFromEnv(Symbol(n), env) match {
          case Some(n) => n
          case None    => throw new Exception("変数が見つかりません")
        }
      case IfExp(cond, t, f) =>
        evalIf(IfExp(cond, t, f), env)
      case LambdaExp(vars, body) =>
        Procedure(
          args => evalProgram(body, extendEnv(vars, args.map(arg => evalExp(arg, env)), env)))
      case ProcedureCall(operator, operands) =>
        val op             = evalExp(operator, env).asInstanceOf[Procedure]
        val evaledOperands = operands.map(o => evalExp(o, env))
        evalExp(op.p(evaledOperands), env)
    }
  }

  def evalIf(exp: IfExp, env: List[Map[Symbol, Datum]]): Datum = {
    if (evalExp(exp.cond, env) == Bool(true)) {
      evalExp(exp.trueExp, env)
    } else {
      exp.falseExp match {
        case Some(exp) => evalExp(exp, env)
        case None      => Bool(false)
      }
    }
  }
}
