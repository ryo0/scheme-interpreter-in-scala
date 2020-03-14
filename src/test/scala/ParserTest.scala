import org.scalatest.FunSuite
import _root_.tokenize.token.Tokens._
import _root_.tokenize.Tokenizer.tokenize
import parser.ast.ast._
import parser.parser.parseTokensToNodes
import parser.parser.parseExp
import parser.parser.parseNodesToExps

class ParserTest extends FunSuite {
  test("parser.parseNodes") {
    assert(
      parseTokensToNodes(List(LParen, NumToken(1), RParen)) === List(
        Node(List(Leaf(NumToken(1f))))))
    assert(
      parseTokensToNodes(tokenize("(a b c (d e (f)))")) ===
        List(
          Node(
            List(
              Leaf(VarToken("a")),
              Leaf(VarToken("b")),
              Leaf(VarToken("c")),
              Node(
                List(Leaf(VarToken("d")), Leaf(VarToken("e")), Node(List(Leaf(VarToken("f")))))))))
    )
    assert(
      parseTokensToNodes(tokenize("(1 (+ 1 2) 3) (4)")) ===
        List(Node(
               List(Leaf(NumToken(1f)),
                    Node(List(Leaf(Plus), Leaf(NumToken(1f)), Leaf(NumToken(2f)))),
                    Leaf(NumToken(3f)))),
             Node(List(Leaf(NumToken(4f))))))
    assert(
      parseTokensToNodes(tokenize("(define (len lst) (if (null? x) 0 (+ 1 (len (cdr lst)))))")) ===
        List(Node(List(
          Leaf(Define),
          Node(List(Leaf(VarToken("len")), Leaf(VarToken("lst")))),
          Node(List(
            Leaf(If),
            Node(List(Leaf(VarToken("null?")), Leaf(VarToken("x")))),
            Leaf(NumToken(0f)),
            Node(List(Leaf(Plus),
                      Leaf(NumToken(1f)),
                      Node(List(Leaf(VarToken("len")),
                                Node(List(Leaf(VarToken("cdr")), Leaf(VarToken("lst"))))))))
          ))
        ))))
    assert(
      parseTokensToNodes(tokenize("(if #t 1 2)")) === List(
        Node(List(Leaf(If), Leaf(TrueToken), Leaf(NumToken(1f)), Leaf(NumToken(2f))))))
  }
  test("parser.parseExp") {
    assert(
      parseNodesToExps(parseTokensToNodes(tokenize("(if #t #t #f)"))) === List(
        IfExp(True, True, False))
    )
//    assert(
//      parseNodesToExps(parseTokensToNodes(tokenize("(if (= 1 2) (+ 1 2) (- 1 2))"))) === IfExp(
//        True,
//        True,
//        False)
//    )
  }
}