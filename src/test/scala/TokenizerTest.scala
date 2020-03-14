import org.scalatest.FunSuite
import tokenize.Tokenizer.tokenizeLetter
import tokenize.Tokenizer.tokenizeDigit
import tokenize.Tokenizer.tokenizeFloat
import tokenize.Tokenizer.tokenizeString
import tokenize.Tokenizer.removeComments
import tokenize.Tokenizer.tokenize
import _root_.tokenize.token.Tokens._

class TokenizerTest extends FunSuite {
  test("Tokenizer.removeComments") {
    assert(removeComments("(define x 1)") === "(define x 1)")
    assert(removeComments("(define (len x) aaa ;bbb\n)") === "(define (len x) aaa \n)")
    assert(removeComments("(define (len x) ;aaa bbb\n)") === "(define (len x) \n)")
    assert(removeComments(";;aiueo\n;;README\n") === "\n\n")
    assert(
      removeComments(";;aiueo\na;;README\n(define (len x) aaa ;bbb\n)") === "\n\n(define (len x) aaa \n)")
  }

  test("Tokenizer.tokenizeLetter") {
    assert(tokenizeLetter("aA1+") === (VarToken("aA1"), 3))
    assert(tokenizeLetter("a1") === (VarToken("a1"), 2))
    assert(tokenizeLetter("A)") === (VarToken("A"), 1))
    assert(tokenizeLetter("z") === (VarToken("z"), 1))
    assert(tokenizeLetter("null?") === (VarToken("null?"), 5))
    assert(tokenizeLetter("yes!") === (VarToken("yes!"), 4))
    assert(tokenizeLetter("len-iter") === (VarToken("len-iter"), 8))
  }

  test("Tokenizer.tokenizeLetter 予約語") {
    assert(tokenizeLetter("true") === (TrueToken, 4))
    assert(tokenizeLetter("false") === (FalseToken, 5))
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
    assert(tokenizeFloat("1") === (NumToken(1f), 1))
    assert(tokenizeFloat("123") === (NumToken(123f), 3))
    assert(tokenizeFloat("2+") === (NumToken(2f), 1))
    assert(tokenizeFloat("3)") === (NumToken(3f), 1))
    assert(tokenizeFloat("4a)") === (NumToken(4f), 1))
    assert(tokenizeFloat("4.1a)") === (NumToken(4.1f), 3))
    assert(tokenizeFloat("1.0") === (NumToken(1.0f), 3))
    assert(tokenizeFloat("12.3") === (NumToken(12.3f), 4))
    assert(tokenizeFloat("2.1+") === (NumToken(2.1f), 3))
    assert(tokenizeFloat("3.5)") === (NumToken(3.5f), 3))
  }

  test("Tokenizer.tokenizeString") {
    assert(tokenizeString("\"aaa\")") === (StrToken("aaa"), 5))
    assert(tokenizeString("\"a(abc)\"e") === (StrToken("a(abc)"), 8))
    assert(tokenizeString("\"123\")") === (StrToken("123"), 5))
    assert(tokenizeString("\"(+ 1 2)\")") === (StrToken("(+ 1 2)"), 9))
  }

  test("Tokenizer.tokenize") {
    assert(
      tokenize("1+2-3*4/5") === List(NumToken(1f),
                                     Plus,
                                     NumToken(2f),
                                     Minus,
                                     NumToken(3f),
                                     Asterisk,
                                     NumToken(4f),
                                     Slash,
                                     NumToken(5f)))
    assert(tokenize("=") === List(Equal))
    assert(tokenize("'") === List(Quote))
    assert(tokenize("(> a 1)") === List(LParen, GreaterThan, VarToken("a"), NumToken(1f), RParen))
    assert(tokenize("(< a1 b2)") === List(LParen, LessThan, VarToken("a1"), VarToken("b2"), RParen))
    assert(
      tokenize("(= \"abc\" d)") === List(LParen, Equal, StrToken("abc"), VarToken("d"), RParen))
    assert(tokenize("(eq? #t #f)") === List(LParen, VarToken("eq?"), TrueToken, FalseToken, RParen))
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
        VarToken("len"),
        VarToken("lst"),
        RParen,
        LParen,
        If,
        LParen,
        VarToken("null?"),
        VarToken("lst"),
        RParen,
        NumToken(0f),
        LParen,
        VarToken("len"),
        LParen,
        VarToken("cdr"),
        VarToken("lst"),
        RParen,
        RParen,
        RParen
      ))
  }
}
