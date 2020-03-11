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
          case ' ' =>

          case _ =>
            result :+= Str(str(i).toString)
        }
        i += 1
      }
      result
    }

}
