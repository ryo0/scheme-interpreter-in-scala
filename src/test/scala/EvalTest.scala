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

  test("四則演算") {
    assert(evalProgram(parseProgram(parseTokensToNodes(tokenize("((lambda(x y) (+ x y)) 1 2)"))),
                       List()) === Num(3f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(
                    tokenize("(+ ((lambda(x y) (+ x y)) 1 2) ((lambda(x y) (+ x x y )) 5 5))"))),
                  List()) === Num(18f))
    assert(evalProgram(parseProgram(parseTokensToNodes(tokenize("(- 1 2)"))), List()) === Num(-1f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(/ 1 2)"))), List()) === Num(1 / 2f))
    assert(evalProgram(parseProgram(parseTokensToNodes(tokenize("(* 3 2)"))), List()) === Num(6f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(- 1 2 3)"))), List()) === Num(-4f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(/ 1 2 2)"))), List()) === Num(1 / 4f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(* 3 2 3)"))), List()) === Num(18f))
    assert(
      evalProgram(
        parseProgram(parseTokensToNodes(tokenize("(+ 3 (/ 3 5 (* 3 4 (- 4 34 6 2 1)) 5))"))),
        List()) === Num(11699 / 3900))
  }
}
