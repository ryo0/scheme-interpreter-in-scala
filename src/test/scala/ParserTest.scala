import org.scalatest.FunSuite
import _root_.tokenize.token.Tokens._
import _root_.tokenize.Tokenizer.tokenize
import parser.ast.ast._
import parser.parser.parseNodes

class ParserTest extends FunSuite {
  test("parser.parseNodes") {
    assert(parseNodes(List(LParen, Num(1), RParen)) === List(Node(List(Leaf(Num(1f))))))
    assert(
      parseNodes(tokenize("(a b c (d e (f)))")) ===
        List(
          Node(
            List(Leaf(Var("a")),
                 Leaf(Var("b")),
                 Leaf(Var("c")),
                 Node(List(Leaf(Var("d")), Leaf(Var("e")), Node(List(Leaf(Var("f")))))))))
    )
    assert(
      parseNodes(tokenize("(1 (+ 1 2) 3) (4)")) ===
        List(Node(
               List(Leaf(Num(1f)),
                    Node(List(Leaf(Plus), Leaf(Num(1f)), Leaf(Num(2f)))),
                    Leaf(Num(3f)))),
             Node(List(Leaf(Num(4f))))))
    assert(
      parseNodes(tokenize("(define (len lst) (if (null? x) 0 (+ 1 (len (cdr lst)))))")) ===
        List(Node(List(
          Leaf(Define),
          Node(List(Leaf(Var("len")), Leaf(Var("lst")))),
          Node(List(
            Leaf(If),
            Node(List(Leaf(Var("null?")), Leaf(Var("x")))),
            Leaf(Num(0f)),
            Node(List(Leaf(Plus),
                      Leaf(Num(1f)),
                      Node(List(Leaf(Var("len")), Node(List(Leaf(Var("cdr")), Leaf(Var("lst"))))))))
          ))
        ))))
  }
}
