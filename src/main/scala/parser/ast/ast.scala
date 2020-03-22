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

  sealed class Node
  case class Leaf(l: Token)           extends Node
  case class Nodes(nodes: List[Node]) extends Node

  case class Program(p: List[Form])
  sealed class Form
  case class DefineStatement(name: Var, body: Program)                             extends Form
  sealed class Exp                                                                 extends Form
  case class Op(op: rawOp)                                                         extends Exp
  case class IfExp(cond: Exp, trueExp: Exp, falseExp: Option[Exp])                 extends Exp
  case class LambdaExp(vars: List[Var], body: Program)                             extends Exp
  case class LetExp(bindings: List[(Var, Exp)], body: Program)                     extends Exp
  case class ProcedureCall(operator: Exp, operands: List[Exp])                     extends Exp
  case class CondExp(condAndClauses: List[(Exp, List[Exp])], elseCause: List[Exp]) extends Exp
  case class QuoteExp(data: Datum)                                                 extends Exp
  case class SetExp(variable: Var, value: Exp)                                     extends Exp
  sealed class Datum                                                               extends Exp
  object True                                                                      extends Datum
  object False                                                                     extends Datum
  case class Num(n: Float)                                                         extends Datum
  case class Var(v: String)                                                        extends Datum
  case class Str(s: String)                                                        extends Datum
  case class DataList(lst: List[Datum])                                            extends Datum
}
