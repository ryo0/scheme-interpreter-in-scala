import tokenize.Tokenizer.tokenize

object Main extends App {
  println(tokenize(str = "11+22"))
  println(tokenize(str = "(ab1 + 22.012 + a * 'ccd - ab2 / 3)"))
}
