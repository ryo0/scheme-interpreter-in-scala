import org.scalatest.FunSuite
import _root_.tokenize.token.Tokens._
import _root_.tokenize.Tokenizer.tokenize
import parser.ast.ast._
import parser.parser.parseNodeSub

class ParserTest extends FunSuite {
  test("Tokenizer.removeComments") {
    assert(
      parseNodeSub(List(LParen, Num(1), RParen), List()) === (List(), List(
        Node(List(Leaf(Num(1f)))))))
    assert(
      parseNodeSub(tokenize("(a b c (d e (f)))"), List()) ===
        (List(), List(
          Node(
            List(Leaf(Var("a")),
                 Leaf(Var("b")),
                 Leaf(Var("c")),
                 Node(List(Leaf(Var("d")), Leaf(Var("e")), Node(List(Leaf(Var("f")))))))))))
    assert(
      parseNodeSub(tokenize("(1 (+ 1 2) 3) (4)"), List()) ===
        (List(), List(Node(
                        List(Leaf(Num(1f)),
                             Node(List(Leaf(Plus), Leaf(Num(1f)), Leaf(Num(2f)))),
                             Leaf(Num(3f)))),
                      Node(List(Leaf(Num(4f)))))))

  }
}
