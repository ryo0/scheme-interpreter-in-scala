package evaluator

import parser.ast.ast.{Datum, Symbol, _}

import scala.collection.mutable

object evaluator {
  val equalProc = Procedure(args =>
    if (args.head == args.tail.head) {
      Bool(true)
    } else {
      Bool(false)
  })

  def eval(program: Program): Datum = {
    val initEnv: List[mutable.Map[Symbol, Datum]] = List(
      mutable.Map(
        Symbol("car") -> Procedure(args => args.head.asInstanceOf[DataList].lst.head),
        Symbol("cdr") -> Procedure(args => DataList(args.head.asInstanceOf[DataList].lst.tail)),
        Symbol("cons") -> Procedure(
          args => DataList(args.head :: args.tail.head.asInstanceOf[DataList].lst)),
        Symbol("null?") -> Procedure(
          args =>
            if (args.head
                  .isInstanceOf[DataList]
                  .&&(args.head
                    .asInstanceOf[DataList]
                    .lst
                    .isEmpty)) {
              Bool(true)
            } else {
              Bool(false)
          }),
        Symbol("eq?")    -> equalProc,
        Symbol("equal?") -> equalProc,
        Symbol("=")      -> equalProc,
      )
    )
    evalProgram(program, initEnv)
  }

  def evalProgram(program: Program, env: List[mutable.Map[Symbol, Datum]]): Datum = {
    var currentEnv: List[mutable.Map[Symbol, Datum]] = env
    var result: Option[Datum]                        = None
    program.p.foreach { p: Form =>
      {
        p match {
          case exp: Exp =>
            result = Option(evalExp(exp, currentEnv))
          case defStmt: DefineStatement =>
            currentEnv = evalDefinition(defStmt, currentEnv)
            result = Option(Str("ok"))

        }
      }
    }
    result match {
      case Some(n) => n
      case None    => throw new Exception("evalProgramが空")
    }
  }

  def extendEnv(params: List[Symbol],
                values: List[Datum],
                env: List[mutable.Map[Symbol, Datum]]): List[mutable.Map[Symbol, Datum]] = {
    var newEnv = mutable.Map[Symbol, Datum]()
    for (i <- params.indices) {
      newEnv = newEnv + (params(i) -> values(i))
    }
    newEnv :: env
  }

  def findValueFromEnv(symbol: Symbol, env: List[mutable.Map[Symbol, Datum]]): Option[Datum] = {
    for (envMap <- env) {
      val value = envMap.get(symbol)
      value match {
        case Some(_) =>
          return value
        case _ =>
      }
    }
    None
  }

  def evalDefinition(defStmt: DefineStatement,
                     env: List[mutable.Map[Symbol, Datum]]): List[mutable.Map[Symbol, Datum]] = {
    env.head.put(defStmt.name, evalProgram(defStmt.body, env))
    env
  }

  def evalExp(exp: Exp, env: List[mutable.Map[Symbol, Datum]]): Datum = {
    val opMap = Map(
      Plus -> Procedure(args => {
        val first = args.head
        args.tail
          .map(arg => evalExp(arg, env).asInstanceOf[Num])
          .foldLeft(first.asInstanceOf[Num]) { (acc, x) =>
            Num(acc.n + x.n)
          }
      }),
      Minus -> Procedure(args => {
        val first = args.head
        args.tail
          .map(arg => evalExp(arg, env).asInstanceOf[Num])
          .foldLeft(first.asInstanceOf[Num]) { (acc, x) =>
            Num(acc.n - x.n)
          }
      }),
      Asterisk -> Procedure(args =>
        args.map(arg => evalExp(arg, env).asInstanceOf[Num]).foldLeft(Num(1f)) { (acc, x) =>
          Num(acc.n * x.n)
      }),
      Slash -> Procedure(args =>
        args.map(arg => evalExp(arg, env).asInstanceOf[Num]).foldLeft(Num(1f)) { (acc, x) =>
          Num(acc.n / x.n)
      }),
      Equal -> equalProc,
      GreaterThan -> Procedure(
        args => Bool(args.head.asInstanceOf[Num].n > args.tail.head.asInstanceOf[Num].n)),
      LessThan -> Procedure(
        args => Bool(args.head.asInstanceOf[Num].n < args.tail.head.asInstanceOf[Num].n))
    )

    exp match {
      case QuoteExp(body) =>
        body
      case Num(_) | Str(_) | Bool(_) =>
        exp.asInstanceOf[Datum]
      case DataList(_) =>
        exp.asInstanceOf[Datum]
      case Op(_op) =>
        opMap(_op)
      case Symbol(n) =>
        findValueFromEnv(Symbol(n), env) match {
          case Some(_n) => _n
          case None =>
            throw new Exception("変数が見つかりません")
        }
      case IfExp(cond, t, f) =>
        evalIf(IfExp(cond, t, f), env)
      case CondExp(condAndClauses, elseCause) =>
        evalCond(CondExp(condAndClauses, elseCause), env)
      case LambdaExp(vars, body) =>
        Procedure(
          args => evalProgram(body, extendEnv(vars, args.map(arg => evalExp(arg, env)), env)))
      case LetExp(binds, body) =>
        evalLet(LetExp(binds, body), env)
      case BeginExp(exps) =>
        evalBegin(BeginExp(exps), env)
      case AndExp(exps) =>
        evalAnd(AndExp(exps), env)
      case OrExp(exps) =>
        evalOr(OrExp(exps), env)
      case ProcedureCall(operator, operands) =>
        val op             = evalExp(operator, env).asInstanceOf[Procedure]
        val evaledOperands = operands.map(o => evalExp(o, env))
        evalExp(op.p(evaledOperands), env)
    }
  }

  def evalLet(exp: LetExp, env: List[mutable.Map[Symbol, Datum]]): Datum = {
    val newEnv = extendEnv(exp.bindings.map(bind => bind._1),
                           exp.bindings.map(bind => evalExp(bind._2, env)),
                           env)
    evalProgram(exp.body, newEnv)
  }

  def evalCond(exp: CondExp, env: List[mutable.Map[Symbol, Datum]]): Datum = {
    findFirstSome(exp.condAndClauses.map(cls => {
      if (Bool(true) == evalExp(cls._1, env)) {
        Option(cls._2.map(cl2 => evalExp(cl2, env)).last)
      } else {
        None
      }
    })) match {
      case Some(d) => d
      case None    => exp.elseCause.map(elseC => evalExp(elseC, env)).last
    }
  }

  def evalBegin(exp: BeginExp, env: List[mutable.Map[Symbol, Datum]]): Datum = {
    exp.exps.map(e => evalExp(e, env)).last
  }

  def evalAnd(exp: AndExp, env: List[mutable.Map[Symbol, Datum]]): Bool = {
    val result = exp.exps.map(e => evalExp(e, env)).filter(e => e == Bool(false))
    if (result.isEmpty) {
      Bool(true)
    } else {
      Bool(false)
    }
  }

  def evalOr(exp: OrExp, env: List[mutable.Map[Symbol, Datum]]): Bool = {
    val result = exp.exps.map(e => evalExp(e, env)).filter(e => e == Bool(true))
    if (result.isEmpty) {
      Bool(false)
    } else {
      Bool(true)
    }
  }

  def findFirstSome(ops: List[Option[Datum]]): Option[Datum] = {
    for (op <- ops) {
      op match {
        case Some(n) => return Some(n)
        case None    => None
      }
    }
    None
  }

  def evalIf(exp: IfExp, env: List[mutable.Map[Symbol, Datum]]): Datum = {
    if (evalExp(exp.cond, env) == Bool(true)) {
      evalExp(exp.trueExp, env)
    } else {
      exp.falseExp match {
        case Some(_exp) => evalExp(_exp, env)
        case None       => Bool(false)
      }
    }
  }
}
