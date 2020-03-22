package tokenize.token

object Tokens {
  sealed class Token

  case class NumToken(value: Float) extends Token

  case class VarToken(name: String) extends Token

  case class StrToken(name: String) extends Token

  object TrueToken extends Token

  object FalseToken extends Token

  object LParen extends Token

  object RParen extends Token

  object PlusToken extends Token

  object MinusToken extends Token

  object AsteriskToken extends Token

  object SlashToken extends Token

  object EqualToken extends Token

  object Quote extends Token

  object LessThanToken extends Token

  object GreaterThanToken extends Token

  object Define extends Token

  object Lambda extends Token

  object Set extends Token

  object Cond extends Token

  object If extends Token

  object Else extends Token

  object Let extends Token

  object Begin extends Token

  object AndToken extends Token

  object OrToken extends Token

}
