import org.scalatest.FunSuite
import _root_.tokenize.token.Tokens._
import _root_.tokenize.Tokenizer.tokenize
import parser.ast.ast._
import parser.parser.parseTokensToNodes
import parser.parser.parseExpList
import parser.parser.parseExp
import parser.parser.parseForm
import parser.parser.parseProgram
import parser.parser.parseBindings

class ParserTest extends FunSuite {
  test("parser.parseNodes") {
    assert(
      parseTokensToNodes(List(LParen, NumToken(1), RParen)) === List(
        Nodes(List(Leaf(NumToken(1f))))))
    assert(
      parseTokensToNodes(tokenize("(a b c (d e (f)))")) ===
        List(
          Nodes(
            List(
              Leaf(VarToken("a")),
              Leaf(VarToken("b")),
              Leaf(VarToken("c")),
              Nodes(
                List(Leaf(VarToken("d")), Leaf(VarToken("e")), Nodes(List(Leaf(VarToken("f"))))))
            )))
    )
    assert(
      parseTokensToNodes(tokenize("(1 (+ 1 2) 3) (4)")) ===
        List(Nodes(
               List(Leaf(NumToken(1f)),
                    Nodes(List(Leaf(PlusToken), Leaf(NumToken(1f)), Leaf(NumToken(2f)))),
                    Leaf(NumToken(3f)))),
             Nodes(List(Leaf(NumToken(4f))))))
    assert(
      parseTokensToNodes(tokenize("'(1 (+ 1 2) 3) (4)")) ===
        List(
          Nodes(
            List(Leaf(Quote),
                 Nodes(List(Leaf(NumToken(1f)),
                            Nodes(List(Leaf(PlusToken), Leaf(NumToken(1f)), Leaf(NumToken(2f)))),
                            Leaf(NumToken(3f)))))),
          Nodes(List(Leaf(NumToken(4f))))
        ))
    assert(
      parseTokensToNodes(tokenize("'a")) ===
        List(Nodes(List(Leaf(Quote), Leaf(VarToken("a"))))))
    assert(
      parseTokensToNodes(tokenize("(define (len lst) (if (null? x) 0 (+ 1 (len (cdr lst)))))")) ===
        List(Nodes(List(
          Leaf(Define),
          Nodes(List(Leaf(VarToken("len")), Leaf(VarToken("lst")))),
          Nodes(List(
            Leaf(If),
            Nodes(List(Leaf(VarToken("null?")), Leaf(VarToken("x")))),
            Leaf(NumToken(0f)),
            Nodes(List(Leaf(PlusToken),
                       Leaf(NumToken(1f)),
                       Nodes(List(Leaf(VarToken("len")),
                                  Nodes(List(Leaf(VarToken("cdr")), Leaf(VarToken("lst"))))))))
          ))
        ))))
    assert(
      parseTokensToNodes(tokenize("(if #t 1 2)")) === List(
        Nodes(List(Leaf(If), Leaf(TrueToken), Leaf(NumToken(1f)), Leaf(NumToken(2f))))))
  }
  test("parser.parseIfExp") {
    assert(
      parseExpList(parseTokensToNodes(tokenize("(if #t #t #f)"))) === List(
        IfExp(Bool(true), Bool(true), Some(Bool(false))))
    )
    assert(
      parseExpList(parseTokensToNodes(tokenize("(if #t #t #f) (len lst)"))) === List(
        IfExp(Bool(true), Bool(true), Some(Bool(false))),
        ProcedureCall(Symbol("len"), List(Symbol("lst"))))
    )
    assert(
      parseExpList(parseTokensToNodes(tokenize("(if (= 1 2) (+ 1 2) (- 1 2))"))) === List(
        IfExp(ProcedureCall(Op(Equal), List(Num(1f), Num(2f))),
              ProcedureCall(Op(Plus), List(Num(1f), Num(2f))),
              Some(ProcedureCall(Op(Minus), List(Num(1f), Num(2f)))))))
    assert(
      parseExpList(parseTokensToNodes(tokenize("(if (= 1 (+ 1 1)) #t #f)"))) === List(
        IfExp(ProcedureCall(Op(Equal),
                            List(Num(1f), ProcedureCall(Op(Plus), List(Num(1f), Num(1f))))),
              Bool(true),
              Some(Bool(false)))))
  }
  test("parser.parseLambdaExp") {
    assert(
      parseProgram(parseTokensToNodes(tokenize("(lambda (a b c) d)"))) === Program(
        List(
          LambdaExp(List(Symbol("a"), Symbol("b"), Symbol("c")), Program(List(Symbol("d"))))
        )))

    assert(
      parseProgram(parseTokensToNodes(tokenize("(lambda (x) (+ 1 2) 3"))) === Program(
        List(
          LambdaExp(List(Symbol("x")),
                    Program(List(ProcedureCall(Op(Plus), List(Num(1f), Num(2f))), Num(3f))))
        )))

    assert(
      parseProgram(parseTokensToNodes(tokenize("(lambda (x) (if (= x 1) #t #f))"))) === Program(
        List(
          LambdaExp(List(Symbol("x")),
                    Program(List(IfExp(ProcedureCall(Op(Equal), List(Symbol("x"), Num(1f))),
                                       Bool(true),
                                       Some(Bool(false)))))))))

    assert(parseProgram(
      parseTokensToNodes(tokenize("(lambda (x) (define y 1) (if (= x 1) (+ x y) y))"))) === Program(
      List(LambdaExp(
        List(Symbol("x")),
        Program(List(
          DefineStatement(Symbol("y"), Program(List(Num(1f)))),
          IfExp(ProcedureCall(Op(Equal), List(Symbol("x"), Num(1f))),
                ProcedureCall(Op(Plus), List(Symbol("x"), Symbol("y"))),
                Some(Symbol("y")))
        ))
      ))))
  }
  test("define") {
    assert(
      parseForm(parseTokensToNodes(tokenize("(define x (/ 2.1 5.22))")).head) ===
        DefineStatement(Symbol("x"),
                        Program(List(ProcedureCall(Op(Slash), List(Num(2.1f), Num(5.22f))))))
    )
    assert(
      parseForm(parseTokensToNodes(tokenize("(define x '(2.1 5.22))")).head) ===
        DefineStatement(Symbol("x"), Program(List(QuoteExp(DataList(List(Num(2.1f), Num(5.22f)))))))
    )
    assert(
      parseForm(parseTokensToNodes(
        tokenize("(define (len lst) (if (null? lst) 0 (+ 1 (len (cdr lst)))))")).head) ===
        DefineStatement(
          Symbol("len"),
          Program(
            List(LambdaExp(
              List(Symbol("lst")),
              Program(
                List(IfExp(
                  ProcedureCall(Symbol("null?"), List(Symbol("lst"))),
                  Num(0f),
                  Some(ProcedureCall(
                    Op(Plus),
                    List(Num(1f),
                         ProcedureCall(Symbol("len"),
                                       List(ProcedureCall(Symbol("cdr"), List(Symbol("lst"))))))))
                ))
              )
            )))
        )
    )
    assert(
      parseForm(
        parseTokensToNodes(tokenize(
          "(define (len lst) (define x 100) (if (null? lst) 0 (+ 1 (len (cdr lst)))))")).head) ===
        DefineStatement(
          Symbol("len"),
          Program(
            List(LambdaExp(
              List(Symbol("lst")),
              Program(List(
                DefineStatement(Symbol("x"), Program(List(Num(100f)))),
                IfExp(
                  ProcedureCall(Symbol("null?"), List(Symbol("lst"))),
                  Num(0f),
                  Some(ProcedureCall(
                    Op(Plus),
                    List(Num(1f),
                         ProcedureCall(Symbol("len"),
                                       List(ProcedureCall(Symbol("cdr"), List(Symbol("lst"))))))))
                )
              ))
            )))
        )
    )
  }
  test("let") {
    assert(
      parseBindings(parseTokensToNodes(tokenize("((x (+ 1 2)) (y (cdr a)))")).head)
        === List((Symbol("x"), ProcedureCall(Op(Plus), List(Num(1f), Num(2f)))),
                 (Symbol("y"), ProcedureCall(Symbol("cdr"), List(Symbol("a"))))))

    assert(
      parseBindings(parseTokensToNodes(tokenize("((x '(1 2)) (y (cdr a)))")).head)
        === List((Symbol("x"), QuoteExp(DataList(List(Num(1f), Num(2f))))),
                 (Symbol("y"), ProcedureCall(Symbol("cdr"), List(Symbol("a"))))))

    assert(
      parseExp(parseTokensToNodes(tokenize(
        "(let ((x (+ 1 2)) (y (cdr a))) (define (len lst) (if (null? lst) 0 (+ 1 (len (cdr lst)))))#t)")).head)
        === LetExp(
          List((Symbol("x"), ProcedureCall(Op(Plus), List(Num(1f), Num(2f)))),
               (Symbol("y"), ProcedureCall(Symbol("cdr"), List(Symbol("a"))))),
          Program(List(
            DefineStatement(
              Symbol("len"),
              Program(
                List(LambdaExp(
                  List(Symbol("lst")),
                  Program(List(
                    IfExp(
                      ProcedureCall(Symbol("null?"), List(Symbol("lst"))),
                      Num(0f),
                      Some(ProcedureCall(
                        Op(Plus),
                        List(Num(1f),
                             ProcedureCall(Symbol("len"),
                                           List(ProcedureCall(Symbol("cdr"),
                                                              List(Symbol("lst"))))))))
                    )
                  ))
                ))
              )
            ),
            Bool(true)
          ))
        ))
  }

  test("if") {
    assert(
      parseExp(parseTokensToNodes(tokenize("(if (= a 1) '(1) '(2))")).head)
        === IfExp(ProcedureCall(Op(Equal), List(Symbol("a"), Num(1f))),
                  QuoteExp(DataList(List(Num(1f)))),
                  Some(QuoteExp(DataList(List(Num(2f)))))))
  }

  test("cond") {
    assert(
      parseExp(parseTokensToNodes(tokenize("(cond ((= a 1) #t) (else #f))")).head)
        === CondExp(List((ProcedureCall(Op(Equal), List(Symbol("a"), Num(1f))), List(Bool(true)))),
                    List(Bool(false))))

    assert(
      parseExp(parseTokensToNodes(tokenize("(cond ((= a 'x) '(1 2)) (else '(1 2 3)))")).head)
        === CondExp(
          List((ProcedureCall(Op(Equal), List(Symbol("a"), QuoteExp(Symbol("x")))),
                List(QuoteExp(DataList(List(Num(1f), Num(2f))))))),
          List(QuoteExp(DataList(List(Num(1f), Num(2f), Num(3f)))))
        ))

    assert(
      parseExp(parseTokensToNodes(tokenize("(cond ((= a 1) #t) ((= a 2) #f))")).head)
        === CondExp(
          List((ProcedureCall(Op(Equal), List(Symbol("a"), Num(1f))), List(Bool(true))),
               (ProcedureCall(Op(Equal), List(Symbol("a"), Num(2f))), List(Bool(false)))),
          List()
        )
    )

    assert(
      parseExp(parseTokensToNodes(tokenize("(cond ((= a 1) (+ 1 2)) ((= a 2) (+ 1 2 )))")).head)
        === CondExp(
          List(
            (ProcedureCall(Op(Equal), List(Symbol("a"), Num(1f))),
             List(ProcedureCall(Op(Plus), List(Num(1f), Num(2f))))),
            (ProcedureCall(Op(Equal), List(Symbol("a"), Num(2f))),
             List(ProcedureCall(Op(Plus), List(Num(1f), Num(2f)))))
          ),
          List()
        )
    )
  }

  test("quote") {
    assert(
      parseExpList(parseTokensToNodes(tokenize("'a")))
        === List(QuoteExp(Symbol("a"))))

    assert(
      parseExpList(parseTokensToNodes(tokenize("'(a 1 2)")))
        === List(QuoteExp(DataList(List(Symbol("a"), Num(1f), Num(2f))))))

    assert(
      parseExpList(parseTokensToNodes(tokenize("'(a (1 2))")))
        === List(QuoteExp(DataList(List(Symbol("a"), DataList(List(Num(1f), Num(2f))))))))

    assert(
      parseExpList(parseTokensToNodes(tokenize("'(a \"(1 2)\")")))
        === List(QuoteExp(DataList(List(Symbol("a"), Str("(1 2)"))))))

    assert(
      parseExpList(parseTokensToNodes(tokenize("(quote a)")))
        === List(QuoteExp(Symbol("a"))))

    assert(
      parseExpList(parseTokensToNodes(tokenize("(quote (a 1 2))")))
        === List(QuoteExp(DataList(List(Symbol("a"), Num(1f), Num(2f))))))

    assert(
      parseExpList(parseTokensToNodes(tokenize("(quote (a (1 2)))")))
        === List(QuoteExp(DataList(List(Symbol("a"), DataList(List(Num(1f), Num(2f))))))))

    assert(
      parseExpList(parseTokensToNodes(tokenize("(quote (a \"(1 2)\"))")))
        === List(QuoteExp(DataList(List(Symbol("a"), Str("(1 2)"))))))

    assert(
      parseExpList(parseTokensToNodes(tokenize("(quote (+ 1 2))")))
        === List(QuoteExp(DataList(List(Op(Plus), Num(1f), Num(2f))))))
  }

  test("set!") {
    assert(
      parseExp(parseTokensToNodes(tokenize("(set! x #t)")).head)
        === SetExp(Symbol("x"), Bool(true)))
    assert(
      parseExp(parseTokensToNodes(tokenize("(set! x '(1 2 3))")).head)
        === SetExp(Symbol("x"), QuoteExp(DataList(List(Num(1f), Num(2f), Num(3f))))))
  }

  test("begin") {
    assert(
      parseExp(parseTokensToNodes(tokenize("(begin x #t)")).head)
        === BeginExp(List(Symbol("x"), Bool(true))))
  }

  test("and") {
    assert(
      parseExp(parseTokensToNodes(tokenize("(and x #t)")).head)
        === AndExp(List(Symbol("x"), Bool(true))))
  }

  test("or") {
    assert(
      parseExp(parseTokensToNodes(tokenize("(or x #t)")).head)
        === OrExp(List(Symbol("x"), Bool(true))))
  }
}
