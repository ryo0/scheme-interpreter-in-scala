import org.scalatest.FunSuite
import tokenize.Tokenizer.tokenizeLetter
import tokenize.Tokenizer.tokenizeDigit
import tokenize.Tokenizer.tokenizeFloat
import tokenize.Tokenizer.tokenizeString
import tokenize.Tokenizer.tokenize
import _root_.tokenize.token.Tokens._

class TokenizerTest extends FunSuite {
  test("Tokenizer.tokenizeLetter") {
    assert(tokenizeLetter("aA1+") === (Var("aA1"), 3))
    assert(tokenizeLetter("a1") === (Var("a1"), 2))
    assert(tokenizeLetter("A)") === (Var("A"), 1))
    assert(tokenizeLetter("z") === (Var("z"), 1))
    assert(tokenizeLetter("null?") === (Var("null?"), 5))
    assert(tokenizeLetter("yes!") === (Var("yes!"), 4))
    assert(tokenizeLetter("len-iter") === (Var("len-iter"), 8))
  }

  test("Tokenizer.tokenizeLetter 予約語") {
    assert(tokenizeLetter("true") === (True, 4))
    assert(tokenizeLetter("false") === (False, 5))
    assert(tokenizeLetter("define") === (Define, 6))
    assert(tokenizeLetter("set!") === (Set, 4))
    assert(tokenizeLetter("lambda") === (Lambda, 6))
    assert(tokenizeLetter("cond") === (Cond, 4))
    assert(tokenizeLetter("if") === (If, 2))
    assert(tokenizeLetter("else") === (Else, 4))
    assert(tokenizeLetter("let") === (Let, 3))
    assert(tokenizeLetter("begin") === (Begin, 5))
    assert(tokenizeLetter("and") === (And, 3))
    assert(tokenizeLetter("or") === (Or, 2))
  }

  test("Tokenizer.tokenizeDigit") {
    assert(tokenizeDigit("1") === ("1", 1))
    assert(tokenizeDigit("123") === ("123", 3))
    assert(tokenizeDigit("2+") === ("2", 1))
    assert(tokenizeDigit("3)") === ("3", 1))
    assert(tokenizeDigit("4a)") === ("4", 1))
  }

  test("Tokenizer.tokenizeFloat") {
    assert(tokenizeFloat("1") === (Num(1f), 1))
    assert(tokenizeFloat("123") === (Num(123f), 3))
    assert(tokenizeFloat("2+") === (Num(2f), 1))
    assert(tokenizeFloat("3)") === (Num(3f), 1))
    assert(tokenizeFloat("4a)") === (Num(4f), 1))
    assert(tokenizeFloat("4.1a)") === (Num(4.1f), 3))
    assert(tokenizeFloat("1.0") === (Num(1.0f), 3))
    assert(tokenizeFloat("12.3") === (Num(12.3f), 4))
    assert(tokenizeFloat("2.1+") === (Num(2.1f), 3))
    assert(tokenizeFloat("3.5)") === (Num(3.5f), 3))
  }

  test("Tokenizer.tokenizeString") {
    assert(tokenizeString("\"aaa\")") === (Str("aaa"), 5))
    assert(tokenizeString("\"a(abc)\"e") === (Str("a(abc)"), 8))
    assert(tokenizeString("\"123\")") === (Str("123"), 5))
    assert(tokenizeString("\"(+ 1 2)\")") === (Str("(+ 1 2)"), 9))
  }

  test("Tokenizer.tokenize") {
    assert(
      tokenize("1+2-3*4/5") === List(Num(1f),
                                     Plus,
                                     Num(2f),
                                     Minus,
                                     Num(3f),
                                     Asterisk,
                                     Num(4f),
                                     Slash,
                                     Num(5f)))
    assert(tokenize("=") === List(Equal))
    assert(tokenize("'") === List(Quote))
    assert(tokenize("(> a 1)") === List(LParen, GreaterThan, Var("a"), Num(1f), RParen))
    assert(tokenize("(< a1 b2)") === List(LParen, LessThan, Var("a1"), Var("b2"), RParen))
    assert(tokenize("(= \"abc\" d)") === List(LParen, Equal, Str("abc"), Var("d"), RParen))
  }

  test("Tokenizer.tokenize2") {
    assert(
      tokenize("""
        (define (len lst)
          (if (null? lst)
          0
          (len (cdr lst))
        )
      """) === List(
        LParen,
        Define,
        LParen,
        Var("len"),
        Var("lst"),
        RParen,
        LParen,
        If,
        LParen,
        Var("null?"),
        Var("lst"),
        RParen,
        Num(0f),
        LParen,
        Var("len"),
        LParen,
        Var("cdr"),
        Var("lst"),
        RParen,
        RParen,
        RParen
      ))
  }
}
