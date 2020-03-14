package parser

import ast.ast._
import tokenize.token.Tokens._

object parser {
  def parseNodes(tokens: List[Token]): List[Nodes] = {
    parseNodeSub(tokens, List())._1
  }

  def parseNodeSub(tokens: List[Token], acm: List[Nodes]): (List[Nodes], List[Token]) = {
    tokens match {
      case x :: xs => {}
      x match {
        case LParen =>
          val (result, rest) = parseNodeSub(xs, List())
          parseNodeSub(rest, acm ::: List(Node(result)))
        case RParen =>
          (acm, xs)
        case _ =>
          parseNodeSub(xs, acm ::: List(Leaf(x)))
      }
    case x :: _ =>
        (Leaf(x) :: acm, List())
      case _ =>
        (acm, List())
    }
  }
}
