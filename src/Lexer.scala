sealed trait Token
case class ClassNameToken(name: String) extends Token
case object PlusToken extends Token
case object GreaterThanToken extends Token
case object RightCurlyToken extends Token
case object RightParenToken extends Token
case object PrintToken extends Token
class Lexer {

}
