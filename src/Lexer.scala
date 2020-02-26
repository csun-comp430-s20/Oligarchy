sealed trait Token

case class ClassNameToken(name: String) extends Token
case class IntegerToken(value: Int) extends Token
case class StrToken(value: String) extends Token
case class VarToken(name: String) extends Token
case class BooleanToken(name: Boolean) extends Token

case object ClassToken extends Token
case object DivisionToken extends Token
case object OrToken extends Token
case object SemicolonToken extends Token
case object IfToken extends Token
case object ElseToken extends Token
case object PeriodToken extends Token
case object GreaterThanToken extends Token
case object LessThanToken extends Token
case object RightCurlyToken extends Token
case object LeftCurlyToken extends Token
case object RightParenToken extends Token
case object LeftParenToken extends Token
case object PlusToken extends Token
case object MultiplicationToken extends Token
case object CaretToken extends Token
case object EqualsToken extends Token
case object FuncToken extends Token
case object SubtractToken extends Token
case object ForToken extends Token
case object ConstructorToken extends Token
case object BreakToken extends Token
case object ReturnToken extends Token 
case object PrintToken extends Token 
case object AndToken extends Token 

class Lexer {
  
}

