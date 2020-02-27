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


case class LexerException(msg: String) extends Exception(msg)

object Lexer {
  def apply(input: String): Lexer = {
    new Lexer(input.toCharArray.toList)
  }
}

class Lexer(private var input: List[Char]) {
  private def tryTokenizeVariableOrReservedWord(): Option[Token] = {
    @scala.annotation.tailrec
    def readLetters(accum: String): String = {
      input match {
        case head :: tail if Character.isLetterOrDigit(head) => {
          input = tail
          readLetters(accum + head)
        }
        case _ => accum
      }
    }

    input match {
      case head :: tail if Character.isLetter(head) => {
        input = tail
        readLetters("" + head) match {
          case "if" => Some(IfToken)
          case "else" => Some(ElseToken)
          case other => Some(VarToken(other))
        }
      }
      case _ => None
    }
  } // tryTokenizeVariableOrReservedWord

  private def tryTokenizeInteger(): Option[IntegerToken] = {
    @scala.annotation.tailrec
    def readDigits(accum: String): Option[IntegerToken] = {
      input match {
        case head :: tail if Character.isDigit(head) => {
          input = tail
          readDigits(accum + head)
        }
        case _ => {
          if (accum.length > 0) {
            Some(IntegerToken(accum.toInt))
          } else {
            None
          }
        }
      }
    }

    input match {
      case '-' :: tail => readDigits("-")
      case _ => readDigits("")
    }
  } // tryTokenizeInteger

  @scala.annotation.tailrec
  private def skipWhitespace() {
    input match {
      case head :: tail if Character.isWhitespace(head) => {
        input = tail
        skipWhitespace()
      }
      case _ => ()
    }
  } // skipWhitespace

  // assumes it's not starting on whitespace
  private def tokenizeOne(): Token = {
    tryTokenizeVariableOrReservedWord().getOrElse {
      tryTokenizeInteger().getOrElse {
        input match {
          case '(' :: tail => {
            input = tail
            LeftParenToken
          }
          case ')' :: tail => {
            input = tail
            RightParenToken
          }
          case _ :: _ => {
            throw LexerException("Have input, but it's not valid")
          }
          case Nil => {
            throw LexerException("Have no more input")
          }
        }
      }
    }
  } // tokenizeOne

  def tokenize(): Seq[Token] = {
    @scala.annotation.tailrec
    def withAccum(accum: List[Token]): Seq[Token] = {
      input match {
        case _ :: _ => {
          skipWhitespace()
          input match {
            case _ :: _ => withAccum(tokenizeOne() :: accum)
            case Nil => accum.reverse.toSeq
          }
        }
        case Nil => accum.reverse.toSeq
      }
    }

    withAccum(List())
  } // tokenize

} // Lexer //
