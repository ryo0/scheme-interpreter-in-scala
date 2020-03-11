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
              val (num, j) = tokenizeFloat(str.slice(i, str.length))
              i += j
              result :+= num
            } else if(str(i).isLetter) {
              val (num, j) = tokenizeLetter(str.slice(i, str.length))
              i += j
              result :+= num
            }
        }
      }
      result
    }

  def tokenizeLetter(str: String) : (Str, Int) = {
    var i = 1
    var result = str(0).toString
    while (i < str.length) {
      val c = str(i)
      if (c.isLetter || c.isDigit) {
        result += c
        i += 1
      } else {
        return (Str(result), i)
      }
    }
    (Str(result), i)
  }

  def tokenizeFloat(str: String) : (Num, Int) = {
    val (num1, i) = tokenizeDigit(str)
    if (i < str.length && str(i) == '.') {
      val (num2, j) = tokenizeDigit(str.slice(i+1, str.length))
      return (Num((num1 + "."  + num2).toFloat), i+1+j)
    }
    (Num(num1.toFloat), i)
  }

  def tokenizeDigit(str: String) : (String, Int) = {
    var i = 0
    var result = ""
    while(i < str.length) {
      val c = str(i)
      if (c.isDigit) {
        result += c
        i += 1
      } else {
        return (result, i)
      }
    }
    (result, i)
  }

}
