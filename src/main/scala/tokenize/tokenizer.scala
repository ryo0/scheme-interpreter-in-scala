package tokenize
import token.Tokens._

object tokenizer {
    def tokenize(str: String): List[Token]= {
      var result: List[Token] = List()
      var i = 0
      while(i < str.length) {
       str(i) match {
          case '('  =>
            result :+= LParen
          case ')' =>
            result :+= RParen
          case _ =>
            result :+= Str(str(i).toString)
        }
        i += 1
      }
      result
    }

}
