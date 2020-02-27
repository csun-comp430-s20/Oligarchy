sealed trait Token

case class ClassNameToken(name: String) extends Token // better if just use var for now ed
case class IntegerToken(value: Int) extends Token // detect if it is just a number dan
case class StrToken(value: String) extends Token // Detect if between to quotes imon
case class VarToken(name: String) extends Token // steph
case class BooleanToken(name: Boolean) extends Token // can treat as reserved word  but you pass in the value steph

case object ClassToken extends Token // reserved word class steph
case object DivisionToken extends Token // single /  dan
case object OrToken extends Token // single |  dan
case object SemicolonToken extends Token // single ; dan
case object IfToken extends Token // reserved word if  imon  // already done given to us
case object ElseToken extends Token // reserved word else dan // already done given to us
case object PeriodToken extends Token // single  .  dan
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
          //ed
          case "print" => Some(PrintToken)
            //jiamin
          case /*@todo*/ => Some(/*@todo*/)
          case /*@todo*/ => Some(/*@todo*/)
          case /*@todo*/ => Some(/*@todo*/)
          //imon
          case /*@todo*/ => Some(/*@todo*/)
          case /*@todo*/ => Some(/*@todo*/)
          // steph
          case /*@todo*/ => Some(/*@todo*/)
          case /*@todo*/ => Some(/*@todo*/)
          case /*@todo*/ => Some(/*@todo*/)
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
          // ed
          case ">" :: tail => {
            input = tail
            GreaterThanToken
          }
          case "}" :: tail => {
            input = tail
            RightCurlyToken
          }
          case ")" :: tail => {
            input = tail
            RightParenToken
          }
          case "+" :: tail => {
            input = tail
            PlusToken
          }
          // imon
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
          }
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
          }
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
          }
          // dan
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
          }
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
          }
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
          }
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
          }
          // jiamin
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
          }
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
          }
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
          }
          // steph
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
          }
          case /*@todo*/ :: tail => {
            input = tail
            /*@todo*/
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
