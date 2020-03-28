import org.scalatest.FunSuite
import evaluator.evaluator._
import _root_.tokenize.Tokenizer.tokenize
import parser.ast.ast._
import parser.parser.parseTokensToNodes
import parser.parser.parseExp
import parser.parser.parseProgram
class EvalTest extends FunSuite {
  test("eval If") {
    assert(evalExp(parseExp(parseTokensToNodes(tokenize("(if #t 1 2)")).head), initEnv) === Num(1f))
    assert(
      evalExp(parseExp(parseTokensToNodes(tokenize("(if #f 1 \"a\"")).head), initEnv) === Str("a"))
  }

  test("eval apply procedure") {
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("((lambda(x y) (if #t x y)) 1 2)"))),
                  initEnv) === Num(1f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("((lambda(x y) (if #f x y)) 1 2)"))),
                  initEnv) === Num(2f))
  }

  test("四則演算") {
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("((lambda(x y) (+ x y)) 1 2)"))),
                  initEnv) === Num(3f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(
                    tokenize("(+ ((lambda(x y) (+ x y)) 1 2) ((lambda(x y) (+ x x y )) 5 5))"))),
                  initEnv) === Num(18f))
    assert(evalProgram(parseProgram(parseTokensToNodes(tokenize("(- 1 2)"))), initEnv) === Num(-1f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(/ 1 2)"))), initEnv) === Num(1 / 2f))
    assert(evalProgram(parseProgram(parseTokensToNodes(tokenize("(* 3 2)"))), initEnv) === Num(6f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(- 1 2 3)"))), initEnv) === Num(-4f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(/ 1 2 2)"))), initEnv) === Num(1 / 4f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(* 3 2 3)"))), initEnv) === Num(18f))
  }
  test("car, cdr, cons, null?") {
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(car '(1 2))"))), initEnv) ===
        Num(1f))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(cdr '(1 2))"))), initEnv) ===
        DataList(List(Num(2f))))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(cdr '(1 2 3))"))), initEnv) ===
        DataList(List(Num(2f), Num(3f))))
    assert(
      evalProgram(parseProgram(parseTokensToNodes(tokenize("(cons 0 '(1 2 3))"))), initEnv) ===
        DataList(List(Num(0f), Num(1f), Num(2f), Num(3f))))
    assert(evalProgram(parseProgram(parseTokensToNodes(tokenize("(cdr (cons 0 '(1 2 3)))"))),
                       initEnv) ===
      DataList(List(Num(1f), Num(2f), Num(3f))))
    assert(evalProgram(parseProgram(parseTokensToNodes(tokenize("(car (cons 0 '(1 2 3)))"))),
                       initEnv) ===
      Num(0f))
  }
}
