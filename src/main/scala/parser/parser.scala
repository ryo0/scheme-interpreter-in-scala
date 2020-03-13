package parser

import ast.ast._
import tokenize.token.Tokens._

object parser {
//  def parseNodes(nodes: List[Nodes],tokens: List[Token]): (List[Nodes], List[Token])= {
//    match tokens {
//      case List(x, xs) => {
//        match x {
//        case LParen =>
//
//        case RParen =>
//        case _ =>
//
//        }
//      }
//      case () =>
//        (nodes, List())
//    }
//  }

  def parseNodeSub(tokens: List[Token], acm: List[Nodes]): (List[Token], List[Nodes]) = {
    tokens match {
      case x :: xs => {
        x match {
          case LParen =>
            val (rest, result) = parseNodeSub(xs, List())
            parseNodeSub(rest, acm ::: List(Node(result)))
          case RParen =>
            (xs, acm)
          case _ =>
            parseNodeSub(xs, acm ::: List(Leaf(x)))
        }
      }
      case (x: Token) :: _ =>
        (List(), Leaf(x) :: acm)
      case _ =>
        (List(), acm)
    }
  }
}

// (a b c (d e)) (1 2)
// => List(Nodes(Leaf(a), Leaf(b), Leaf(c), Node(Leaf(d), Leaf(e))), Nodes(Leaf(1), Leaf(2)))
