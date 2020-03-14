package parser.ast

import tokenize.token.Tokens.Token

object ast {
  sealed class Nodes
  case class Leaf(l: Token)           extends Nodes
  case class Node(nodes: List[Nodes]) extends Nodes

  sealed class Exp
  case class Num(n: Float)                                         extends Exp
  case class Var(v: String)                                        extends Exp
  case class Str(s: String)                                        extends Exp
  case class IfExp(cond: Exp, trueExp: Exp, falseExp: Option[Exp]) extends Exp
  case class ProcedureCall(operator: Exp, operands: List[Exp])     extends Exp
  object True                                                      extends Exp
  object False                                                     extends Exp
  object Plus                                                      extends Exp
  object Minus                                                     extends Exp
  object Asterisk                                                  extends Exp
  object Slash                                                     extends Exp
  object Equal                                                     extends Exp
  object LessThan                                                  extends Exp
  object GreaterThan                                               extends Exp
}
