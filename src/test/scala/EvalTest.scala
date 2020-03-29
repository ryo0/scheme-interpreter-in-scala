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
        QuoteExp(Num(1f)))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(cdr '(1 2))")))) ===
        QuoteExp(DataList(List(Num(2f)))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(cdr '(1 2 3))")))) ===
        QuoteExp(DataList(List(Num(2f), Num(3f)))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(cons 0 '(1 2 3))")))) ===
        QuoteExp(DataList(List(Num(0f), Num(1f), Num(2f), Num(3f)))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(cdr (cons 0 '(1 2 3)))")))) ===
        QuoteExp(DataList(List(Num(1f), Num(2f), Num(3f)))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(car (cons 0 '(1 2 3)))")))) ===
        QuoteExp(Num(0f)))
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
    assert(
      eval(
        parseProgram(
          parseTokensToNodes(tokenize("(define x 1) (define (double x) (* x 2)) (double 11)")))
      ) === Num(22f))
    assert(
      eval(
        parseProgram(parseTokensToNodes(tokenize(
          "(define (plus a b c) (+ a b c)) (define (len lst) (if (null? lst) 0 (plus 1 (len (cdr lst)) (len (cdr lst))))) (len '(1 2 3))")))
      ) === Num(7f))
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

  test("let") {
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(let ((x 1) (y 2)) (+ x y))")))) ===
        Num(3f))
    assert(
      eval(parseProgram(
        parseTokensToNodes(tokenize("(let ((x (let ((x 1) (y 2)) (+ x y))) (y 2)) (+ x y))")))) ===
        Num(5f))
  }

  test("cond") {
    assert(eval(parseProgram(parseTokensToNodes(tokenize(
      "(define (len lst) (cond ((null? lst) 1 2 0) ((= \"aaa\" lst) 10000) (else \"aaa\" (+ 1 (len (cdr lst)))))) (len '(1 1 1 1 1))")))) ===
      Num(5f))
    assert(eval(parseProgram(parseTokensToNodes(tokenize(
      "(define (len lst) (cond ((null? lst) 1 2 0) ((= \"aaa\" lst) 10000) (else \"aaa\" (+ 1 (len (cdr lst)))))) (len \"aaa\"))))")))) ===
      Num(10000f))

    assert(eval(parseProgram(parseTokensToNodes(tokenize(
      "(define (len lst) (cond ((null? lst) (if (eq? '(1) lst) 1 0))  (else (+ 1 (len (cdr lst)))))) (len '(1 2 3)))))")))) ===
      Num(3f))
  }

  test("findFirstSome") {
    assert(findFirstSome(List(None, Some(Num(1f)))) === Some(Num(1f)))
    assert(findFirstSome(List(None, Some(Num(1f)), Some(Num(2f)))) === Some(Num(1f)))
    assert(findFirstSome(List(Some(Num(1f)), Some(Num(2f)))) === Some(Num(1f)))
    assert(findFirstSome(List(Some(Num(1f)), None, Some(Num(2f)))) === Some(Num(1f)))
    assert(findFirstSome(List(None, None, Some(Num(2f)))) === Some(Num(2f)))
    assert(findFirstSome(List(None, None)) === None)
  }

  test("begin") {
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(begin 1 (+ 1 1) (+ 1 (+ 1 1)))")))) ===
        Num(3f))
  }

  test("and") {
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(and #t #t)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(and #f #t)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(and #t #f)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(and #f #f)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(and #t #t #t)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(and #t #f #t)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(and #t #t #f)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(and #f #t #t)")))) ===
        Bool(false))
  }

  test("or") {
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(or #t #t)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(or #f #t)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(or #t #f)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(or #f #f)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(or #t #t #t)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(or #t #f #t)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(or #t #t #f)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(or #f #t #t)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(or #f #f #f)")))) ===
        Bool(false))
  }

  test("set!") {
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(define x 1) (set! x 100) x")))) ===
        Num(100f))
  }

  test("quote") {
    assert(
      eval(parseProgram(
        parseTokensToNodes(tokenize("(define (deriv a b) '*) (deriv '(x + 3) 'x) \"aa\"")))) ===
        Str("aa"))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("'+")))) ===
        QuoteExp(Op(Plus)))
  }

  test("lib") {
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(car '(x + 3))")))) ===
        QuoteExp(Symbol("x")))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(cdr '(x + 3))")))) ===
        QuoteExp(DataList(List(Op(Plus), Num(3f)))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(list 1 2 3")))) ===
        QuoteExp(DataList(List(Num(1f), Num(2f), Num(3f)))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(cons 1 '(2 3)")))) ===
        QuoteExp(DataList(List(Num(1f), Num(2f), Num(3f)))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(cons 'x '(2 3)")))) ===
        QuoteExp(DataList(List(QuoteExp(Symbol("x")), Num(2f), Num(3f)))))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(number? 'x")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(number? '(1 2)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(number? '1")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(number? 1")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(symbol? 1")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(symbol? 'x")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(pair? '(1 2 3)")))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(pair? 'x")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(null? 'x")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(null? \"x\"")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(null? '(1)")))) ===
        Bool(false))
    assert(
      eval(parseProgram(parseTokensToNodes(tokenize("(null? '())")))) ===
        Bool(true))
    assert(
      eval(parseProgram(
        parseTokensToNodes(tokenize("(define (addend s) (car s)) (addend '(1 2 3))")))) ===
        QuoteExp(Num(1f)))
    assert(eval(parseProgram(parseTokensToNodes(tokenize(
      "(define (cddr lst) (cdr (cdr lst))) (define (caddr lst) (car (cddr lst))) (define (augend s) (caddr s)) (augend '(1 2 3))")))) ===
      QuoteExp(Num(3f)))
    assert(
      eval(parseProgram(parseTokensToNodes(
        tokenize("""
                   |(define (cadr lst) (car (cdr lst)))
                   |
                   |(define (cddr lst) (cdr (cdr lst)))
                   |
                   |(define (caddr lst) (car (cddr lst)))
                   |
                   |(define (cdddr lst) (cdr (cddr lst)))
                   |
                   |(define (cadddr lst) (car (cdddr lst)))
        |(define (simple-sum? x)  (and (pair? x) (eq? (cadr x) '+)  (null? (cdddr x))))
        |(simple-sum? '(a + b))
      """.stripMargin)))) ===
        Bool(true))
    assert(
      eval(parseProgram(parseTokensToNodes(
        tokenize("""
                   |(define (cadr lst) (car (cdr lst)))
                   |
                   |(define (cddr lst) (cdr (cdr lst)))
                   |
                   |(define (caddr lst) (car (cddr lst)))
                   |
                   |(define (cdddr lst) (cdr (cddr lst)))
                   |
                   |(define (cadddr lst) (car (cdddr lst)))
                   |(define (simple-sum? x)  (and (pair? x) (eq? (cadr x) '+)  (null? (cdddr x))))
                   |(simple-sum? 'a))
                 """.stripMargin)))) ===
        Bool(false))
  }
}
