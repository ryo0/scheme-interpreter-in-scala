package parser.ast

import tokenize.token.Tokens.Token

object ast {
  sealed class Nodes
  case class Leaf(l: Token)           extends Nodes
  case class Node(nodes: List[Nodes]) extends Nodes
}
