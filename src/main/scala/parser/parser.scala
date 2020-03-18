package parser

import ast.ast._
import ast.ast.True
import tokenize.token.Tokens._
import tokenize.token.Tokens.TrueToken
import tokenize.token.Tokens.FalseToken

object parser {
  def parseTokensToNodes(tokens: List[Token]): List[Node] = {
    parseTokensToNodeSub(tokens, List())._1
  }

  def parseTokensToNodeSub(tokens: List[Token], acm: List[Node]): (List[Node], List[Token]) = {
    tokens match {
      case x :: xs =>
        x match {
          case LParen =>
            val (result, rest) = parseTokensToNodeSub(xs, List())
            parseTokensToNodeSub(rest, acm ::: List(Nodes(result)))
          case RParen =>
            (acm, xs)
          case _ =>
            parseTokensToNodeSub(xs, acm ::: List(Leaf(x)))
        }
      case x :: _ =>
        (Leaf(x) :: acm, List())
      case _ =>
        (acm, List())
    }
  }

  def parseProgram(nodes: List[Node]): Program = {
    Program(parseFormList(nodes))
  }

  def parseFormList(nodes: List[Node]): List[Form] = {
    nodes match {
      case first :: rest =>
        parseForm(first) :: parseFormList(rest)
      case first :: List() =>
        List(parseForm(first))
      case List() =>
        List()
    }
  }

  def parseForm(nodes: Node): Form = {
    nodes match {
      case Leaf(Define) =>
        throw new Exception("(define)は不正なコード")
      case Leaf(l) =>
        parseExp(Leaf(l))
      case Nodes(ns) =>
        ns match {
          case Leaf(Define) :: _ =>
            parseDefine(nodes)
          case _ =>
            parseExp(nodes)
        }

    }
  }

  def parseDefine(nodes: Node): DefineStatement = {
    nodes match {
      case Leaf(_) =>
        throw new Exception("error")
      case Nodes(ns) =>
        ns match {
          case Leaf(Define) :: Leaf(variable) :: rest =>
            // (define x (+ 1 2) (- 2 3))
            DefineStatement(parseVar(Leaf(variable)), parseProgram(rest))
          case Leaf(Define) :: Leaf(l) :: body :: _ =>
            // (define x 1)
            val variable = parseVar(Leaf(l))
            val bodyExp  = parseExp(body)
            DefineStatement(variable, Program(List(bodyExp)))
          case Leaf(Define) :: Nodes(Leaf(v) :: ps) :: rest =>
            // (define (x a) (define y 1) (+ a y))
            val variable = parseVar(Leaf(v))
            val params   = parseVarList(ps)
            val program  = parseProgram(rest)
            DefineStatement(variable, Program(List(LambdaExp(params, program))))
          case _ =>
            println(ns)
            throw new Exception("defineがなんかおかしい")
        }
    }
  }

  def parseExpList(nodes: List[Node]): List[Exp] = {
    nodes match {
      case x :: xs =>
        parseExp(x) :: parseExpList(xs)
      case x :: List() =>
        List(parseExp(x))
      case _ =>
        List()
    }
  }

  def parseExp(node: Node): Exp = {
    val symbolMap = Map(
      TrueToken     -> True,
      FalseToken    -> False,
      PlusToken     -> Op(Plus),
      MinusToken    -> Op(Minus),
      AsteriskToken -> Op(Asterisk),
      SlashToken    -> Op(Slash),
      EqualToken    -> Op(Equal),
      AndToken      -> Op(And),
      OrToken       -> Op(Or)
    )
    node match {
      case Leaf(l) =>
        l match {
          case NumToken(n) => Num(n)
          case StrToken(s) => Str(s)
          case VarToken(v) => Var(v)
          case _ =>
            val symbolExp = symbolMap.get(l)
            symbolExp match {
              case Some(sExp: Exp) => sExp
              case None            => throw new Exception("parseExpSub何かがおかしい" + l)
            }
        }

      case Nodes(ns) =>
        ns match {
          case Leaf(l) :: _ =>
            l match {
              case If =>
                parseIfExp(ns)
              case Lambda =>
                parseLambdaExp(ns)
              case Let =>
                parseLetExp(ns)
              case _ =>
                parseProcedureCall(ns)
            }
          case _ =>
            println(ns)
            throw new Exception("parseExpSub何かがおかしい")
        }
    }
  }

  def parseProcedureCall(nodes: List[Node]): ProcedureCall = {
    nodes match {
      case x :: xs =>
        val operator = parseExp(x)
        val operands = xs.map(x => parseExp(x))
        ProcedureCall(operator, operands)
      case x :: List() =>
        val operator = parseExp(x)
        ProcedureCall(operator, List())
      case _ =>
        throw new Exception("procedureCallがなんか不正")
    }
  }

  def parseIfExp(nodes: List[Node]): IfExp = {
    nodes match {
      case Leaf(If) :: predicate :: consequent :: alternative :: List() =>
        val condExp  = parseExp(predicate)
        val trueExp  = parseExp(consequent)
        val falseExp = parseExp(alternative)
        IfExp(condExp, trueExp, Some(falseExp))
      case Leaf(If) :: predicate :: consequent :: List() =>
        val condExp = parseExp(predicate)
        val trueExp = parseExp(consequent)
        IfExp(condExp, trueExp, None)
      case _ =>
        throw new Exception("if式がなんか不正")
    }
  }

  def parseVar(node: Node): Var = {
    node match {
      case Leaf(VarToken(v)) => Var(v)
      case _                 => throw new Exception("VarにLeaf(VarToken)以外が渡された")
    }
  }

  def parseVarList(nodes: List[Node]): List[Var] = {
    nodes match {
      case first :: rest =>
        parseVar(first) :: parseVarList(rest)
      case first :: List() =>
        List(parseVar(first))
      case _ =>
        List()
    }
  }

  def parseLambdaExp(nodes: List[Node]): LambdaExp = {
    nodes match {
      case Leaf(Lambda) :: Nodes(ns) :: body =>
        val ops   = parseVarList(ns)
        val pbody = parseProgram(body)
        LambdaExp(ops, pbody)
      case _ =>
        throw new Exception("Lambda Error")
    }
  }
  def parseBindings(ns: List[Node]): List[(Var, Exp)] = {
    def parseBindingsSub(ns: List[Node], acm: List[(Var, Exp)]): List[(Var, Exp)] = {
      ns match {
        case Nodes(nodes) :: List() =>
          nodes match {
            case Nodes(first) :: rest2 =>
              val t = parseBinding(first)
              parseBindingsSub(rest2, acm :+ t)
            case xs =>
              acm :+ parseBinding(xs)
          }
        case Nodes(nodes) :: rest =>
          parseBindingsSub(rest, acm ::: parseBindings(nodes))
        case xs =>
          acm :+ parseBinding(xs)
      }
    }
    parseBindingsSub(ns, List())
  }

  def parseBinding(ns: List[Node]): (Var, Exp) = {
    ns match {
      case Leaf(VarToken(v)) :: Nodes(nodes) :: _ =>
        (parseVar(Leaf(VarToken(v))), parseExp(Nodes(nodes)))
      case _ =>
        throw new Exception("binding error")
    }
  }

  def parseLetExp(nodes: List[Node]): LetExp = {
    // let ((a b) (c d)) body)
    nodes match {
      case Leaf(Let) :: Nodes(ns) :: body =>
        LetExp(parseBindings(ns), parseProgram(body))
      case _ =>
        println(nodes)
        throw new Exception("let error")
    }
  }
}
