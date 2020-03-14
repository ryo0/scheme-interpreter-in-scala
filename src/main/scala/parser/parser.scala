package parser

import ast.ast._
import ast.ast.True
import tokenize.token.Tokens._
import tokenize.token.Tokens.TrueToken
import tokenize.token.Tokens.FalseToken

object parser {
  def parseTokensToNodes(tokens: List[Token]): List[Nodes] = {
    parseTokensToNodeSub(tokens, List())._1
  }

  def parseTokensToNodeSub(tokens: List[Token], acm: List[Nodes]): (List[Nodes], List[Token]) = {
    tokens match {
      case x :: xs =>
        x match {
          case LParen =>
            val (result, rest) = parseTokensToNodeSub(xs, List())
            parseTokensToNodeSub(rest, acm ::: List(Node(result)))
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

  def parseNodesToExpList(nodes: List[Nodes]): List[Exp] = {
    nodes match {
      case x :: xs =>
        parseExp(x) :: parseNodesToExpList(xs)
      case x :: List() =>
        List(parseExp(x))
      case _ =>
        List()
    }
  }

  def parseExp(nodes: Nodes): Exp = {
    parseExpSub(nodes)._1
  }

  def parseExpSub(node: Nodes): (Exp, List[Nodes]) = {

    val symbolMap = Map(
      TrueToken     -> True,
      FalseToken    -> False,
      PlusToken     -> Plus,
      MinusToken    -> Minus,
      AsteriskToken -> Asterisk,
      SlashToken    -> SlashToken,
      EqualToken    -> Equal,
    )
    node match {
      case Leaf(l) =>
        l match {
          case NumToken(n) => (Num(n), List())
          case StrToken(s) => (Str(s), List())
          case VarToken(v) => (Var(v), List())
          case _ =>
            val symbolExp = symbolMap.get(l)
            symbolExp match {
              case Some(sExp: Exp) => (sExp, List())
              case None            => throw new Exception("parseExpSub何かがおかしい" + l)
            }
        }

      case Node(ns) =>
        ns match {
          case Leaf(l) :: _ =>
            l match {
              case If =>
                parseIfExp(ns)
              case _ =>
                parseProcedureCall(ns)
            }
          case _ =>
            throw new Exception("parseExpSub何かがおかしい")
        }
    }
  }

  def parseProcedureCall(nodes: List[Nodes]): (ProcedureCall, List[Nodes]) = {
    nodes match {
      case x :: xs =>
        val operator = parseExp(x)
        val operands = xs.map(x => parseExp(x))
        (ProcedureCall(operator, operands), xs)
      case x :: _ =>
        val (operator, _) = parseExpSub(x)
        (ProcedureCall(operator, List()), List())
      case _ =>
        throw new Exception("procedureCallがなんか不正")
    }
  }

  def parseIfExp(nodes: List[Nodes]): (IfExp, List[Nodes]) = {
    nodes match {
      case Leaf(If) :: predicate :: consequent :: alternative :: _ =>
        val (condExp, _)  = parseExpSub(predicate)
        val (trueExp, _)  = parseExpSub(consequent)
        val (falseExp, _) = parseExpSub(alternative)
        (IfExp(condExp, trueExp, falseExp), List())
      case _ =>
        throw new Exception("if式がなんか不正")
    }
  }
}
