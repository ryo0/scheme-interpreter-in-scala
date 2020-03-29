package evaluator

import parser.ast.ast.{Datum, Symbol, _}

import scala.collection.mutable

object evaluator {
  val equalProc = Procedure(args => {
    val a = args.head
    val b = args.tail.head
    if (a == b) {
      Bool(true)
    } else {
      a match {
        case exp: QuoteExp if b.isInstanceOf[DataList] =>
          equalList(exp.data.asInstanceOf[DataList], b.asInstanceOf[DataList])
        case _: DataList if b.isInstanceOf[QuoteExp] =>
          equalList(b.asInstanceOf[QuoteExp].data.asInstanceOf[DataList], a.asInstanceOf[DataList])
        case _ =>
          Bool(false)
      }
    }
  })

  def equalList(a: DataList, b: DataList): Bool = {
    Bool(a.lst == b.lst)
  }

  def eval(program: Program): Datum = {
    val initEnv: List[mutable.Map[Symbol, Datum]] = List(
      mutable.Map(
        Symbol("car") -> Procedure(p = args => {
          println("car")
          args.head match {
            case list: DataList =>
              if (list.lst.isEmpty) {
                return QuoteExp(DataList(List()))
              }
              println(list.lst.head)
              QuoteExp(list.lst.head)
            case quote: QuoteExp =>
              quote.data match {
                case list: DataList =>
                  if (list.lst.isEmpty) {
                    return QuoteExp(DataList(List()))
                  }
                  println(list.lst.head)
                  QuoteExp(list.lst.head)
                case _ =>
                  if (args.isEmpty) {
                    return QuoteExp(DataList(List()))
                  }
                  throw new Exception("car")
              }
          }

        }),
        Symbol("cdr") -> {
          Procedure(
            args => {
              if (args.isEmpty) {
                return QuoteExp(DataList(List()))
              }
              args.head match {
                case list: DataList =>
                  QuoteExp(DataList(list.lst.tail))
                case quote: QuoteExp =>
                  quote.data match {
                    case list: DataList =>
                      QuoteExp(DataList(list.lst.tail))
                    case _ =>
                      QuoteExp(DataList(args.tail))
                  }
              }
            }
          )
        },
        Symbol("cons") -> Procedure(args => {
          var head = args.head
          head match {
            case exp: QuoteExp =>
              head = exp.data
            case _ =>
          }
          args.tail.head match {
            case exp: QuoteExp =>
              QuoteExp(DataList(head :: exp.data.asInstanceOf[DataList].lst))
            case _ =>
              QuoteExp(DataList(head :: List(args.tail.head)))
          }
        }),
        Symbol("null?") -> Procedure(args =>
          args.head match {
            case list: DataList =>
              Bool(list.lst.isEmpty)
            case QuoteExp(body) =>
              body match {
                case list: DataList =>
                  if (list.lst.nonEmpty) {
                    Bool(false)
                  } else {
                    Bool(true)
                  }
                case _ =>
                  Bool(false)
              }
            case _ =>
              Bool(false)
        }),
        Symbol("eq?")    -> equalProc,
        Symbol("equal?") -> equalProc,
        Symbol("=")      -> equalProc,
        Symbol("number?") -> Procedure(args => {
          val n = args.head
          if (n.isInstanceOf[Num] || n
                .isInstanceOf[QuoteExp] && n.asInstanceOf[QuoteExp].data.isInstanceOf[Num]) {
            Bool(true)
          } else {
            Bool(false)
          }
        }),
        Symbol("symbol?") -> Procedure(args => {
          if (!args.exists(
                arg =>
                  arg.isInstanceOf[QuoteExp] && arg
                    .asInstanceOf[QuoteExp]
                    .data
                    .isInstanceOf[Symbol])) {
            Bool(false)
          } else {
            Bool(true)
          }
        }),
        Symbol("pair?") -> Procedure(args => {
          val a = args.head
          if (a.isInstanceOf[DataList]) {
            Bool(true)
          } else if (a.asInstanceOf[QuoteExp].data.isInstanceOf[DataList]) {
            Bool(true)
          } else {
            Bool(false)
          }
        }),
        Symbol("print") -> Procedure(args => {
          println(args)
          Str("\n")
        }),
        Symbol("error") -> Procedure(args => {
          throw new Exception("error: " + args)
        }),
        Symbol("list") -> Procedure(args => {
          QuoteExp(DataList(args.map {
            case exp: QuoteExp =>
              exp.data
            case arg =>
              arg
          }))
        })
      )
    )
    evalProgram(program, initEnv)
  }

  def evalProgram(program: Program, env: List[mutable.Map[Symbol, Datum]]): Datum = {
    var currentEnv: List[mutable.Map[Symbol, Datum]] = env
    var result: Option[Datum]                        = None
    program.p.foreach { p =>
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
        QuoteExp(body)
      case Num(_) | Str(_) | Bool(_) =>
        exp.asInstanceOf[Datum]
      case DataList(lst) =>
        DataList(lst)
      case Op(_op) =>
        opMap(_op)
      case Symbol(n) =>
        findValueFromEnv(Symbol(n), env) match {
          case Some(_n) => {
            _n
          }
          case None =>
            throw new Exception("変数が見つかりません: " + n + env)
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
      case SetExp(variable, value) =>
        evalSet(SetExp(variable, value), env)
      case AndExp(exps) =>
        evalAnd(AndExp(exps), env)
      case OrExp(exps) =>
        evalOr(OrExp(exps), env)
      case ProcedureCall(operator, operands) =>
        val op = evalExp(operator, env).asInstanceOf[Procedure]
        val evaledOperands = operands.map(o => {
          evalExp(o, env)
        })
        evalExp(op.p(evaledOperands), env)
      case Procedure(body) =>
        Procedure(body)
      case _ =>
        throw new Exception("exp:" + exp)

    }
  }

  def evalLet(exp: LetExp, env: List[mutable.Map[Symbol, Datum]]): Datum = {
    val newEnv = extendEnv(exp.bindings.map(bind => bind._1),
                           exp.bindings.map(bind => evalExp(bind._2, env)),
                           env)
    evalProgram(exp.body, newEnv)
  }

  def evalCond(exp: CondExp, env: List[mutable.Map[Symbol, Datum]]): Datum = {
    for (condAndCaluse <- exp.condAndClauses) {
      if (evalExp(condAndCaluse._1, env) != Bool(false)) {
        return condAndCaluse._2.map(c => evalExp(c, env)).last
      }
    }
    exp.elseCause.map(elseC => evalExp(elseC, env)).last
  }

  def evalBegin(exp: BeginExp, env: List[mutable.Map[Symbol, Datum]]): Datum = {
    exp.exps.map(e => evalExp(e, env)).last
  }

  def evalAnd(exp: AndExp, env: List[mutable.Map[Symbol, Datum]]): Bool = {
    for (e <- exp.exps) {
      if (evalExp(e, env) == Bool(false)) {
        return Bool(false)
      }
    }
    Bool(true)
  }

  def evalOr(exp: OrExp, env: List[mutable.Map[Symbol, Datum]]): Bool = {
    for (e <- exp.exps) {
      if (evalExp(e, env) == Bool(true)) {
        return Bool(true)
      }
    }
    Bool(false)
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
    if (evalExp(exp.cond, env) != Bool(false)) {
      evalExp(exp.trueExp, env)
    } else {
      exp.falseExp match {
        case Some(_exp) => evalExp(_exp, env)
        case None       => Bool(false)
      }
    }
  }

  def evalSet(exp: SetExp, env: List[mutable.Map[Symbol, Datum]]): Datum = {
    env.head.put(exp.variable, evalExp(exp.value, env))
    Str("ok")
  }
}
