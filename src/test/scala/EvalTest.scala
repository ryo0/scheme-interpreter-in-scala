import org.scalatest.FunSuite
import evaluator.evaluator._
import _root_.tokenize.Tokenizer.tokenize
import parser.ast.ast._
import parser.parser.parseTokensToNodes
import parser.parser.parseExp
import parser.parser.parseProgram
class EvalTest extends FunSuite {
  test("eval If") {
    assert(evalExp(parseExp(parseTokensToNodes(tokenize("(if #t 1 2)")).head), List()) === Num(1f))
    assert(
      evalExp(parseExp(parseTokensToNodes(tokenize("(if #f 1 \"a\"")).head), List()) === Str("a"))
  }

  test("eval apply procedure") {
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("((lambda(x y) (if #t x y)) 1 2)"))),
                  List()) === Num(1f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("((lambda(x y) (if #f x y)) 1 2)"))),
                  List()) === Num(2f))
  }
}
