package parser

import ast.ast._
import tokenize.token.Tokens._
import tokenize.token.Tokens.TrueToken
import tokenize.token.Tokens.FalseToken

object parser {
  def car(lst: List[Node]): Node = {
    lst.head
  }
  def cdr(lst: List[Node]): List[Node] = {
    lst.tail
  }

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
            DefineStatement(parseSymbol(Leaf(variable)), parseProgram(rest))
          case Leaf(Define) :: Nodes(Leaf(v) :: ps) :: rest =>
            // (define (x a) (define y 1) (+ a y))
            val variable = parseSymbol(Leaf(v))
            val params   = parseSymbolList(ps)
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
      case _ =>
        List()
    }
  }

  val symbolMap = Map(
    TrueToken        -> Bool(true),
    FalseToken       -> Bool(false),
    PlusToken        -> Op(Plus),
    MinusToken       -> Op(Minus),
    AsteriskToken    -> Op(Asterisk),
    SlashToken       -> Op(Slash),
    EqualToken       -> Op(Equal),
    AndToken         -> Op(And),
    OrToken          -> Op(Or),
    GreaterThanToken -> Op(GreaterThan),
    LessThanToken    -> Op(LessThan)
  )

  def parseExp(node: Node): Exp = {
    node match {
      case Leaf(l) =>
        l match {
          case NumToken(n) => Num(n)
          case StrToken(s) => Str(s)
          case VarToken(v) => Symbol(v)
          case _ =>
            val symbolExp = symbolMap.get(l)
            symbolExp match {
              case Some(sExp: Exp) => sExp
              case None            => throw new Exception("parseExp何かがおかしい" + l)
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
              case Cond =>
                parseCondExp(ns)
              case Quote =>
                parseQuoteExp(ns)
              case Set =>
                parseSetExp(ns)
              case Begin =>
                parseBeginExp(ns)
              case AndToken =>
                parseAndExp(ns)
              case OrToken =>
                parseOrExp(ns)
              case _ =>
                parseProcedureCall(ns)
            }
          case Nodes(nodes) :: _ =>
            parseProcedureCall(ns)
        }
    }
  }

  def parseProcedureCall(nodes: List[Node]): ProcedureCall = {
    nodes match {
      case x :: xs =>
        val operator = parseExp(x)
        val operands = parseExpList(xs)
        ProcedureCall(operator, operands)
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

  def parseSymbol(node: Node): Symbol = {
    node match {
      case Leaf(VarToken(v)) => Symbol(v)
      case _                 => throw new Exception("VarにLeaf(VarToken)以外が渡された" + node)
    }
  }

  def parseSymbolList(nodes: List[Node]): List[Symbol] = {
    nodes match {
      case first :: rest =>
        parseSymbol(first) :: parseSymbolList(rest)
      case _ =>
        List()
    }
  }

  def parseLambdaExp(nodes: List[Node]): LambdaExp = {
    nodes match {
      case Leaf(Lambda) :: Nodes(ns) :: body =>
        val ops   = parseSymbolList(ns)
        val pbody = parseProgram(body)
        LambdaExp(ops, pbody)
      case _ =>
        throw new Exception("Lambda Error")
    }
  }
  def parseBindings(node: Node): List[(Symbol, Exp)] = {
    def parseBindingsSub(node: Node, acm: List[(Symbol, Exp)]): List[(Symbol, Exp)] = {
      // ((a b) (c d))
      node match {
        case Nodes(ns) =>
          ns.map {
            case Nodes(Leaf(l) :: rest :: List()) =>
              (parseSymbol(Leaf(l)), parseExp(rest))
            case Leaf(l) =>
              throw new Exception("bindings error")
          }
      }
    }
    parseBindingsSub(node, List())
  }

  def parseLetExp(nodes: List[Node]): LetExp = {
    // (let ((a b) (c d)) body)
    nodes match {
      case Leaf(Let) :: bindings :: body =>
        LetExp(parseBindings(bindings), parseProgram(body))
      case _ =>
        println(nodes)
        throw new Exception("let error")
    }
  }

  def parseCondExp(nodes: List[Node]): CondExp = {
    val cdrNodes = cdr(nodes)
    parseCondExpSub(cdrNodes, CondExp(List(), List()))
  }

  def parseCondExpSub(nodes: List[Node], acm: CondExp): CondExp = {
    //    (cond (a 1)
    //          (b 2)
    //          (else 3)
    //          )
    nodes match {
      case Nodes(Leaf(Else) :: rest) :: List() =>
        CondExp(acm.condAndClauses, parseExpList(rest))
      case Nodes(ns) :: rest =>
        val cc     = parseCondClause(ns)
        val result = parseCondExpSub(rest, acm)
        CondExp(cc :: result.condAndClauses, result.elseCause)
      case _ =>
        acm
    }
  }

  def parseCondClause(nodes: List[Node]): (Exp, List[Exp]) = {
    //    (cond (a 1)
    //          (b 2)
    //          (else 3)
    //          )
    //    の中の(a 1)
    val carExp  = parseExp(car(nodes))
    val cdrExps = parseExpList(cdr(nodes))
    (carExp, cdrExps)
  }

  def parseSetExp(nodes: List[Node]): SetExp = {
    val cdrnodes = cdr(nodes)
    SetExp(parseSymbol(car(cdrnodes)), parseExp(car(cdr(cdrnodes))))
  }

  def parseQuoteExp(nodes: List[Node]): QuoteExp = {
    QuoteExp(parseDatum(car(cdr(nodes))))
  }

  def parseData(node: List[Node]): List[Datum] = {
    node match {
      case first :: rest =>
        parseDatum(first) :: parseData(rest)
      case first :: List() =>
        List(parseDatum(first))
      case _ =>
        List()
    }
  }

  def parseDatum(node: Node): Datum = {
    node match {
      case Leaf(l) =>
        l match {
          case NumToken(n) => Num(n)
          case StrToken(s) => Str(s)
          case VarToken(v) => Symbol(v)
          case Quote       => DataList(List())
          case TrueToken   => Bool(true)
          case FalseToken  => Bool(false)
          case _ =>
            val symbolExp = symbolMap.get(l)
            symbolExp match {
              case Some(sExp: Exp) => sExp
              case None            => throw new Exception("parseExp何かがおかしい" + l)
            }
        }
      case Nodes(ns) =>
        DataList(parseData(ns))
    }
  }

  def parseBeginExp(nodes: List[Node]): BeginExp = {
    BeginExp(parseExpList(cdr(nodes)))
  }

  def parseAndExp(nodes: List[Node]): AndExp = {
    AndExp(parseExpList(cdr(nodes)))
  }

  def parseOrExp(nodes: List[Node]): OrExp = {
    OrExp(parseExpList(cdr(nodes)))
  }
}
