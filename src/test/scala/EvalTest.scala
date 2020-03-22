import org.scalatest.FunSuite
import evaluator.evaluator._
import _root_.tokenize.Tokenizer.tokenize
import parser.ast.ast._
import parser.parser.parseTokensToNodes
import parser.parser.parseExp
class EvalTest extends FunSuite {
  test("eval If") {
    assert(evalExp(parseExp(parseTokensToNodes(tokenize("(if #t 1 2)")).head)) === Num(1f))
    assert(evalExp(parseExp(parseTokensToNodes(tokenize("(if #f 1 \"a\"")).head)) === Str("a"))
  }
}