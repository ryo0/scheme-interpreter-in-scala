package parser.ast

import tokenize.token.Tokens.Token

object ast {
  sealed class rawOp
  object Plus        extends rawOp
  object Minus       extends rawOp
  object Asterisk    extends rawOp
  object Slash       extends rawOp
  object Equal       extends rawOp
  object LessThan    extends rawOp
  object GreaterThan extends rawOp
  object And         extends rawOp
  object Or          extends rawOp

  sealed class Nodes
  case class Leaf(l: Token)           extends Nodes
  case class Node(nodes: List[Nodes]) extends Nodes

  case class Program(p: List[Form])
  sealed class Form
  case class DefineStatement(name: Var, body: Program)             extends Form
  sealed class Exp                                                 extends Form
  case class Num(n: Float)                                         extends Exp
  case class Var(v: String)                                        extends Exp
  case class Str(s: String)                                        extends Exp
  case class Op(op: rawOp)                                         extends Exp
  case class IfExp(cond: Exp, trueExp: Exp, falseExp: Option[Exp]) extends Exp
  case class LambdaExp(vars: List[Var], body: Program)             extends Exp
  case class ProcedureCall(operator: Exp, operands: List[Exp])     extends Exp
  object True                                                      extends Exp
  object False                                                     extends Exp
}
