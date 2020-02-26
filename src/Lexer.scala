sealed trait Token

case class StrToken(value: String) extends Token
case object MultiplicationToken extends Token
case object GreaterThanToken extends Token
case object RightCurlyToken extends Token
case object RightParenToken extends Token
case object PrintParenToken extends Token

class Lexer {

}
