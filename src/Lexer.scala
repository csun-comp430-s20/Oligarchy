sealed trait Token

case class VarToken(name: String) extends Token
case class BooleanToken(name: Boolean) extends Token
case object CaretToken extends Token
case object EqualsToken extends Token
case object LeftParenToken extends Token
case object ClassToken extends Token

class Lexer {

}
