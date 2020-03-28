import org.scalatest.FunSuite
import evaluator.evaluator._
import _root_.tokenize.Tokenizer.tokenize
import parser.ast.ast._
import parser.parser.parseTokensToNodes
import parser.parser.parseProgram
class EvalTest extends FunSuite {
  test("eval If") {
    assert(eval(parseProgram(parseTokensToNodes(tokenize("(if #t 1 2)")))) === Num(1f))
    assert(eval(parseProgram(parseTokensToNodes(tokenize("(if #f 1 \"a\"")))) === Str("a"))
  }

  test("eval apply procedure") {
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("((lambda(x y) (if #t x y)) 1 2)")))) === Num(
        1f))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("((lambda(x y) (if #f x y)) 1 2)")))) === Num(
        2f))
  }

  test("四則演算") {
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("((lambda(x y) (+ x y)) 1 2)")))) === Num(3f))
    assert(
      eval(parseProgram(parseTokensToNodes(
        tokenize("(+ ((lambda(x y) (+ x y)) 1 2) ((lambda(x y) (+ x x y )) 5 5))")))) === Num(18f))
    assert(eval(parseProgram(parseTokensToNodes(tokenize("(- 1 2)")))) === Num(-1f))
    assert(eval(parseProgram(parseTokensToNodes(tokenize("(/ 1 2)")))) === Num(1 / 2f))
    assert(eval(parseProgram(parseTokensToNodes(tokenize("(* 3 2)")))) === Num(6f))
    assert(eval(parseProgram(parseTokensToNodes(tokenize("(- 1 2 3)")))) === Num(-4f))
    assert(eval(parseProgram(parseTokensToNodes(tokenize("(/ 1 2 2)")))) === Num(1 / 4f))
    assert(eval(parseProgram(parseTokensToNodes(tokenize("(* 3 2 3)")))) === Num(18f))
  }
  test("car cdr cons null?") {
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(car '(1 2))")))) ===
        Num(1f))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(cdr '(1 2))")))) ===
        DataList(List(Num(2f))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(cdr '(1 2 3))")))) ===
        DataList(List(Num(2f), Num(3f))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(cons 0 '(1 2 3))")))) ===
        DataList(List(Num(0f), Num(1f), Num(2f), Num(3f))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(cdr (cons 0 '(1 2 3)))")))) ===
        DataList(List(Num(1f), Num(2f), Num(3f))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(car (cons 0 '(1 2 3)))")))) ===
        Num(0f))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(null? (cons 0 '(1 2 3)))")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(null? '())")))) ===
        Bool(true))

  }

  test("equal? eq? =") {
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(= 1 2)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(= 1 1")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(= '(1 2) 2")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(= '(1 2) '(1 2)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(= (cons 0 '(1 2)) '(1 2)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(= (cons 0 '(1 2)) '(0 1 2)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(equal? 1 2)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(equal? 1 1")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(equal? '(1 2) 2")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(equal? '(1 2) '(1 2)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(equal? (cons 0 '(1 2)) '(1 2)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(equal? (cons 0 '(1 2)) '(0 1 2)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(eq? 1 2)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(eq? 1 1")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(eq? '(1 2) 2")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(eq? '(1 2) '(1 2)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(eq? (cons 0 '(1 2)) '(1 2)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(eq? (cons 0 '(1 2)) '(0 1 2)")))) ===
        Bool(true))
  }

  test("define") {
    assert(eval(parseProgram(parseTokensToNodes(tokenize("(define x 1) (+ x 2)")))) === Num(3f))
    assert(
      eval(
        parseProgram(parseTokensToNodes(tokenize("(define (double x) (* x 2)) (double 11)")))
      ) === Num(22f))
    assert(
      eval(
        parseProgram(parseTokensToNodes(
          tokenize("(define (len lst) (if (null? lst) 0 (+ 1 (len (cdr lst))))) (len '(1 2 3))")))
      ) === Num(3f))
  }

  test("><") {
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(> 1 2)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(< 1 2)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(> 2 1)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(< 2 1)")))) ===
        Bool(false))
  }
}
