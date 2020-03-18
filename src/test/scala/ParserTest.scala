import org.scalatest.FunSuite
import _root_.tokenize.token.Tokens._
import _root_.tokenize.Tokenizer.tokenize
import parser.ast.ast._
import parser.parser.parseTokensToNodes
import parser.parser.parseExpList
import parser.parser.parseForm
import parser.parser.parseProgram
import parser.parser.parseBindings
import parser.parser.parseLetExp

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
                List(Leaf(VarToken("d")), Leaf(VarToken("e")), Nodes(List(Leaf(VarToken("f")))))))))
    )
    assert(
      parseTokensToNodes(tokenize("(1 (+ 1 2) 3) (4)")) ===
        List(Nodes(
               List(Leaf(NumToken(1f)),
                    Nodes(List(Leaf(PlusToken), Leaf(NumToken(1f)), Leaf(NumToken(2f)))),
                    Leaf(NumToken(3f)))),
             Nodes(List(Leaf(NumToken(4f))))))
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
        IfExp(True, True, Some(False)))
    )
    assert(
      parseExpList(parseTokensToNodes(tokenize("(if #t #t #f) (len lst)"))) === List(
        IfExp(True, True, Some(False)),
        ProcedureCall(Var("len"), List(Var("lst"))))
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
              True,
              Some(False))))
  }
  test("parser.parseLambdaExp") {
    assert(
      parseProgram(parseTokensToNodes(tokenize("(lambda (a b c) d)"))) === Program(
        List(
          LambdaExp(List(Var("a"), Var("b"), Var("c")), Program(List(Var("d"))))
        )))

    assert(
      parseProgram(parseTokensToNodes(tokenize("(lambda (x) (+ 1 2) 3"))) === Program(
        List(
          LambdaExp(List(Var("x")),
                    Program(List(ProcedureCall(Op(Plus), List(Num(1f), Num(2f))), Num(3f))))
        )))

    assert(
      parseProgram(parseTokensToNodes(tokenize("(lambda (x) (if (= x 1) #t #f))"))) === Program(
        List(
          LambdaExp(
            List(Var("x")),
            Program(
              List(IfExp(ProcedureCall(Op(Equal), List(Var("x"), Num(1f))), True, Some(False))))))))

    assert(parseProgram(
      parseTokensToNodes(tokenize("(lambda (x) (define y 1) (if (= x 1) (+ x y) y))"))) === Program(
      List(LambdaExp(
        List(Var("x")),
        Program(List(
          DefineStatement(Var("y"), Program(List(Num(1f)))),
          IfExp(ProcedureCall(Op(Equal), List(Var("x"), Num(1f))),
                ProcedureCall(Op(Plus), List(Var("x"), Var("y"))),
                Some(Var("y")))
        ))
      ))))
  }
  test("define") {
    assert(
      parseForm(parseTokensToNodes(tokenize("(define x (/ 2.1 5.22))")).head) ===
        DefineStatement(Var("x"),
                        Program(List(ProcedureCall(Op(Slash), List(Num(2.1f), Num(5.22f))))))
    )
    assert(
      parseForm(parseTokensToNodes(
        tokenize("(define (len lst) (if (null? lst) 0 (+ 1 (len (cdr lst)))))")).head) ===
        DefineStatement(
          Var("len"),
          Program(
            List(LambdaExp(
              List(Var("lst")),
              Program(
                List(IfExp(
                  ProcedureCall(Var("null?"), List(Var("lst"))),
                  Num(0f),
                  Some(ProcedureCall(
                    Op(Plus),
                    List(Num(1f),
                         ProcedureCall(Var("len"),
                                       List(ProcedureCall(Var("cdr"), List(Var("lst"))))))))
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
          Var("len"),
          Program(
            List(LambdaExp(
              List(Var("lst")),
              Program(List(
                DefineStatement(Var("x"), Program(List(Num(100f)))),
                IfExp(
                  ProcedureCall(Var("null?"), List(Var("lst"))),
                  Num(0f),
                  Some(ProcedureCall(
                    Op(Plus),
                    List(Num(1f),
                         ProcedureCall(Var("len"),
                                       List(ProcedureCall(Var("cdr"), List(Var("lst"))))))))
                )
              ))
            )))
        )
    )
  }
  test("let") {
    assert(
      parseBindings(parseTokensToNodes(tokenize("((x (+ 1 2)) (y (cdr a)))")))
        === List((Var("x"), ProcedureCall(Op(Plus), List(Num(1f), Num(2f)))),
                 (Var("y"), ProcedureCall(Var("cdr"), List(Var("a"))))))
    assert(
      parseExpList(parseTokensToNodes(tokenize(
        "(let ((x (+ 1 2)) (y (cdr a))) (define (len lst) (if (null? lst) 0 (+ 1 (len (cdr lst)))))#t)")))
        === List(LetExp(
          List((Var("x"), ProcedureCall(Op(Plus), List(Num(1f), Num(2f)))),
               (Var("y"), ProcedureCall(Var("cdr"), List(Var("a"))))),
          Program(List(
            DefineStatement(
              Var("len"),
              Program(
                List(LambdaExp(
                  List(Var("lst")),
                  Program(List(
                    IfExp(
                      ProcedureCall(Var("null?"), List(Var("lst"))),
                      Num(0f),
                      Some(ProcedureCall(
                        Op(Plus),
                        List(Num(1f),
                             ProcedureCall(Var("len"),
                                           List(ProcedureCall(Var("cdr"), List(Var("lst"))))))))
                    )
                  ))
                ))
              )
            ),
            True
          ))
        )))
  }
}
