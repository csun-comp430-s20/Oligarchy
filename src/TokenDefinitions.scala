package src
sealed trait Token

case class ClassNameToken(value: String) extends Token // better if just use var for now ed
case class IntegerToken(value: Int) extends Token // detect if it is just a number dan
case class StrToken(value: String) extends Token // Detect if between to quotes imon
case class VarToken(name: String) extends Token // steph
case class BooleanToken(name: Boolean) extends Token // can treat as reserved word  but you pass in the value steph

case object IntTypeToken extends Token
case object StringTypeToken extends Token
case object BooleanTypeToken extends Token

case object VoidToken extends Token
case object ClassToken extends Token // reserved word class steph
case object HOFCToken extends Token
case object DivisionToken extends Token // single /  dan
case object OrToken extends Token // single |  dan
case object SemicolonToken extends Token // single ; dan
case object IfToken extends Token // reserved word if  imon  // already done given to us
case object ElseToken extends Token // reserved word else dan // already done given to us
case object PeriodToken extends Token // single  .  dan
case object CommaToken extends Token // single  .  dan
case object GreaterThanToken extends Token // single >  ed
case object LessThanToken extends Token // single < jiamin
case object RightCurlyToken extends Token // single  } ed
case object LeftCurlyToken extends Token // single  { jiamin
case object RightParenToken extends Token // single )  ed   // already done given to us
case object LeftParenToken extends Token // single (  steph  //already done given to us
case object PlusToken extends Token // single + ed
case object MultiplicationToken extends Token // single * imon
case object CaretToken extends Token // single ^ steph
case object EqualsToken extends Token // single  = steph
case object FuncToken extends Token // reserved word func  jiamin
case object SubtractToken extends Token // single  - jiamin
case object ForToken extends Token // reserved word for jiamin
case object ConstructorToken extends Token // reserved word   constructor jiamin
case object BreakToken extends Token // reserved word  break  imon
case object ReturnToken extends Token // reserved word  return imon
case object PrintToken extends Token // reserved word   print ed
case object AndToken extends Token // single & imon
case object ExtendsToken extends Token //reserved word
case object NewToken extends Token //reserved word
