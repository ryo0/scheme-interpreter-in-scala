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

  def parseNodesToExps(nodes: List[Nodes]): List[Exp] = {
    nodes match {
      case x :: xs =>
        parseExp(x) :: parseNodesToExps(xs)
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
    node match {
      case Leaf(l) =>
        l match {
          case TrueToken  => (True, List())
          case FalseToken => (False, List())
          case _ =>
            throw new Exception("parseExpSub何かがおかしい")
        }
      case Node(ns) =>
        ns match {
          case Leaf(l) :: restNodes =>
            l match {
              case If =>
                parseIfExp(ns)
            }
          case nodes =>
            throw new Exception("Procedure Callは未対応")
//            parseProcedureCall(nodes)
        }
    }
  }

//  def parseProcedureCall(nodes: List[Nodes]): (ProcedureCall, List[Nodes]) = {
//    println("procedure call")
//    println(nodes)
//    nodes match {
//      case x :: xs =>
//        val (operator, _) = parseExpSub(List(x))
//        val operands      = xs.map(x => parseExpSub(List(x))).map(x => x._1)
//        ProcedureCall(operator, operands)
//      case x :: _ =>
//        val (operator, _) = parseExpSub(List(x))
//        ProcedureCall(operator, List())
//      case _ =>
//        throw new Exception("procedureCallがなんか不正")
//    }
//  }/

  def parseIfExp(nodes: List[Nodes]): (IfExp, List[Nodes]) = {
    nodes match {
      case Leaf(If) :: cond :: truePart :: falsePart :: _ =>
        val (condExp, _)     = parseExpSub(cond)
        val (trueExp, _)     = parseExpSub(truePart)
        val (falseExp, rest) = parseExpSub(falsePart)
        (IfExp(condExp, trueExp, falseExp), rest)
      case _ =>
        throw new Exception("if式がなんか不正")
    }
  }
}
