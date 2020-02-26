sealed trait Token

case class IntegerToken(value: Int) extends Token
case object DivisionToken extends Token
case object OrToken extends Token
case object SemicolonToken extends Token
case object ElseToken extends Token
case object PeriodToken extends Token

class Lexer {
  //this is stest to see if push is sending to daniel branch
}