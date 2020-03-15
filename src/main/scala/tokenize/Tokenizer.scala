package tokenize
import token.Tokens._

object Tokenizer {
  private val symbolMap = Map(
    '+' -> PlusToken,
    '-' -> MinusToken,
    '*' -> AsteriskToken,
    '/' -> SlashToken,
    ''' -> QuoteToken,
    '(' -> LParen,
    ')' -> RParen,
    '=' -> EqualToken,
    '>' -> GreaterThanToken,
    '<' -> LessThanToken,
  )
  private val identifierMap = Map(
    "true"   -> TrueToken,
    "false"  -> FalseToken,
    "define" -> Define,
    "set!"   -> Set,
    "lambda" -> Lambda,
    "cond"   -> Cond,
    "if"     -> If,
    "else"   -> Else,
    "let"    -> Let,
    "begin"  -> Begin,
    "and"    -> AndToken,
    "or"     -> OrToken,
  )
  def tokenize(str: String): List[Token] = {
    var result: List[Token] = List()
    var i                   = 0
    while (i < str.length) {
      str(i) match {
        case '+' | '-' | '*' | '/' | ''' | '(' | ')' | '=' | '>' | '<' =>
          val token = symbolMap.get(str(i))
          token.foreach(token => result :+= token)
          i += 1
        case ' ' | '\n' =>
          i += 1
        case '\"' =>
          val (resultStr, j) = tokenizeString(str.slice(i, str.length))
          i += j
          result :+= resultStr
        case '#' =>
          if (i + 1 < str.length) {
            if (str(i + 1) == 't') {
              result :+= TrueToken
            } else if (str(i + 1) == 'f') {
              result :+= FalseToken
            } else {
              throw new Exception("#の後に来るのはtかfだけ" + str(i + 1))
            }
          } else {
            throw new Exception("#がプログラムの末尾にある")
          }
          i += 2
        case _ =>
          if (str(i).isDigit) {
            val (number, j) = tokenizeFloat(str.slice(i, str.length))
            i += j
            result :+= number
          } else if (str(i).isLetter) {
            val (letter, j) = tokenizeLetter(str.slice(i, str.length))
            i += j
            result :+= letter
          } else {
            throw new Exception("tokenize例外:" + str(i))
          }
      }
    }
    result
  }

  def removeComments(str: String): String = {
    var i         = 0
    var inComment = false
    var result    = ""
    while (i < str.length) {
      val c = str(i)
      if (c == ';' && !inComment) {
        inComment = true
      } else if (c == '\n' && inComment) {
        inComment = false
      }
      if (!inComment) {
        result += c
      }
      i += 1
    }
    result
  }

  def tokenizeLetter(str: String): (Token, Int) = {
    var i      = 1
    var result = str(0).toString
    while (i < str.length) {
      val c = str(i)
      if (c.isLetter || c.isDigit || c == '!' || c == '?' || c == '-') {
        result += c
        i += 1
      } else {
        return (returnVarOrIdentifier(result), i)
      }
    }
    (returnVarOrIdentifier(result), i)
  }

  def returnVarOrIdentifier(str: String): Token = {
    if (identifierMap.contains(str)) {
      identifierMap(str)
    } else {
      VarToken(str)
    }
  }

  def tokenizeFloat(str: String): (NumToken, Int) = {
    val (num1, i) = tokenizeDigit(str)
    if (i < str.length && str(i) == '.') {
      val (num2, j) = tokenizeDigit(str.slice(i + 1, str.length))
      return (NumToken((num1 + "." + num2).toFloat), i + 1 + j)
    }
    (NumToken(num1.toFloat), i)
  }

  def tokenizeDigit(str: String): (String, Int) = {
    var i      = 0
    var result = ""
    while (i < str.length) {
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

  def tokenizeString(str: String): (StrToken, Int) = {
    var i      = 1
    var result = ""
    while (i < str.length) {
      val c = str(i)
      if (c == '\"') {
        return (StrToken(result), i + 1)
      }
      result += c
      i += 1
    }
    throw new Exception("Stringのダブルクォートが閉じていない")
  }
}
