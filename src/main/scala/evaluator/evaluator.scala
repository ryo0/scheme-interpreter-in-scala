package evaluator

import parser.ast.ast._

object evaluator {
//  def evalProgram(program: Program, env: List[Map[Symbol, Datum]]): Datum = {
//    program.p.map { p: Form =>
//      {
//        p match {
//          case exp: Exp => evalExp(exp, env)
////          case defStmt: DefineStatement =>
//        }
//      }
//    }
//  }
  def evalExp(exp: Exp, env: List[Map[Symbol, Datum]]): Datum = {
    exp match {
      case Num(_) | Str(_) | Bool(_) | Op(_) =>
        exp.asInstanceOf[Datum]
      case IfExp(cond, t, f) =>
        evalIf(IfExp(cond, t, f), env)
      case LambdaExp(vars, body) =>
        Procedure(env, vars, body)
    }
  }
//      case ProcedureCall(operator, operands) =>
//        // ((lambda(x y) (+ x y)) 1 2)
//        val p                          = evalExp(operator, env).asInstanceOf[Procedure]
//        val ops                        = operands.map(op => evalExp(op, env))
//        val newEnv: Map[Symbol, Datum] = Map()
//        for (i <- ops.indices) {
//          newEnv(p.params(i))
//        }
//        p.params.foreach(param => newEnv(param) = Num(0f))
//        ops.foreach(op => newEnv)
//
//        evalProgram(p.body, Map() :: env)
//    }
//  }

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
