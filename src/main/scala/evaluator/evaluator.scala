package evaluator

import parser.ast.ast.{Symbol, _}

object evaluator {
  val equalProc = Procedure(args =>
    if (args.head == args.tail.head) {
      Bool(true)
    } else {
      Bool(false)
  })

  val initEnv: List[Map[Symbol, Datum]] = List(
    Map(
      Symbol("car") -> Procedure(args => args.head.asInstanceOf[DataList].lst.head),
      Symbol("cdr") -> Procedure(args => DataList(args.head.asInstanceOf[DataList].lst.tail)),
      Symbol("cons") -> Procedure(
        args => DataList(args.head :: args.tail.head.asInstanceOf[DataList].lst)),
      Symbol("null?") -> Procedure(args =>
        if (args.head.asInstanceOf[DataList].lst.isEmpty) {
          Bool(true)
        } else {
          Bool(false)
      }),
      Symbol("eq?")    -> equalProc,
      Symbol("equal?") -> equalProc,
      Symbol("=")      -> equalProc,
    )
  )
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
    val opMap = Map(Plus -> { (x: Float, y: Float) =>
      x + y
    }, Minus -> { (x: Float, y: Float) =>
      x - y
    }, Asterisk -> { (x: Float, y: Float) =>
      x * y
    }, Slash -> { (x: Float, y: Float) =>
      x / y
    })
    exp match {
      case QuoteExp(body) =>
        body
      case Num(_) | Str(_) | Bool(_) =>
        exp.asInstanceOf[Datum]
      case DataList(_) =>
        exp.asInstanceOf[Datum]
      case Op(Equal) =>
        equalProc
      case Op(_op) =>
        val op = opMap(_op)
        _op match {
          case Plus | Minus =>
            Procedure(args => {
              val first = args.head
              args.tail
                .map(arg => evalExp(arg, env).asInstanceOf[Num])
                .foldLeft(first.asInstanceOf[Num]) { (acc, x) =>
                  Num(op(acc.n, x.n))
                }
            })
          case Asterisk | Slash =>
            Procedure(args =>
              args.map(arg => evalExp(arg, env).asInstanceOf[Num]).foldLeft(Num(1f)) { (acc, x) =>
                Num(op(acc.n, x.n))
            })
        }
      case Symbol(n) =>
        findValueFromEnv(Symbol(n), env) match {
          case Some(_n) => _n
          case None     => throw new Exception("変数が見つかりません")
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
        case Some(_exp) => evalExp(_exp, env)
        case None       => Bool(false)
      }
    }
  }
}
