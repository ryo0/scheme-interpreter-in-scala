package tokenize
import token.Tokens._


object tokenizer {
  private val symbolMap = Map(
    '+'  -> Plus,
    '-'  -> Minus,
    '*'  -> Asterisk,
    '/' -> Slash,
    '\'' -> Quote,
    '(' -> LParen,
    ')' -> RParen
  )
  def tokenize(str: String): List[Token]= {
      var result: List[Token] = List()
      var i = 0
      while(i < str.length) {
       str(i) match {
          case '+' | '-' | '*' | '/' | '\'' | '(' | ')'  =>
            val token = symbolMap.get(str(i))
            token.foreach(token => result :+= token)
            i += 1
          case ' ' =>
            i += 1
          case _ =>
            if (str(i).isDigit) {
              val (num, j) = tokenizeDigit(str.slice(i, str.length))
              i += j
              result :+= num
            } else if(str(i).isLetter) {
              i += 1
            }
        }
      }
      result
    }

  def tokenizeDigit(str: String) : (Num, Int) = {
    var i = 0
    var result = ""
    while(i < str.length) {
      val c = str(i)
      if (c.isDigit) {
        result += c
        i += 1
      } else {
        return (Num(result.toFloat), i)
      }
    }
    (Num(result.toFloat), i)
  }

}