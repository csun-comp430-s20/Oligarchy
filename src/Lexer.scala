sealed trait Token

case class StrToken(value: String) extends Token
case object MultiplicationToken extends Token
case object GreaterThanToken extends Token
case object RightCurlyToken extends Token
case object RightParenToken extends Token
case object PrintParenToken extends Token
case class ClassNameToken(name: String) extends Token
case object PlusToken extends Token
case object GreaterThanToken extends Token
case object RightCurlyToken extends Token
case object RightParenToken extends Token
case object PrintToken extends Token
case class VarToken(name: String) extends Token
case class BooleanToken(name: Boolean) extends Token
case object CaretToken extends Token
case object EqualsToken extends Token
case object LeftParenToken extends Token
case object ClassToken extends Token

class Lexer {

}
