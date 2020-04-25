
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
          //daniel
          case "else" => Some(ElseToken)
          //ed
          case "print" => Some(PrintToken)
          //jiamin
          case "func" => Some(FuncToken)
          case "for" => Some(ForToken)
          case "constructor" => Some(ConstructorToken)
          //imon
          case "return" => Some(ReturnToken)
          case "break" => Some(BreakToken)
          // steph
          case "Class" => Some(ClassToken)
          case "true" => Some(BooleanToken(true))
          case "false" => Some(BooleanToken(false))
          case "extends" => Some(ExtendsToken)
          case "void" => Some(VoidToken)
          case "str" => Some(StringTypeToken)
          case "int" => Some(IntTypeToken)
          case "bool" => Some(BooleanTypeToken)
          case "new" => Some(NewToken)
          case "hofc" => Some(HOFCToken)
          case other => Some(VarToken(other))
        }
      }
      case _ => None
    }
  } // tryTokenizeVariableOrReservedWord

  private def tryTokenizeInteger(): Option[IntegerToken] = {
    var test: List[Char] = input;
    @scala.annotation.tailrec
    def readDigits(accum: String): Option[IntegerToken] = {
      input match {
        case head :: tail if Character.isDigit(head) => {
          input = tail
          readDigits(accum + head)
        }
        case _ => {

          if (accum.length > 0 ) {
            Some(IntegerToken(accum.toInt))
          } else {
            input = test
            None
          }
        }
      }
    }

    input match {
      case head::tail if Character.isDigit(head)=>
        input = tail
        readDigits(head.toString)
      case _  => None
    }

  } // tryTokenizeInteger

  private def tryTokenizeString(): Option[StrToken] = {
    @scala.annotation.tailrec
    var temp: List[Char] = input;
    def readChars(accum: String): Option[StrToken] = {
      input match {
        case '\"' :: tail  => {
          input = tail
          Some(StrToken(accum))
        }
        case _ :: tail if tail == Nil => {
          throw LexerException("Input ran out before seeing another \"")
        }
        case head :: tail  =>
          input = tail
          readChars(accum + head)
      }
    }

    input match {
      case '\"' :: tail =>
        input = tail
        readChars("")
      case _ => None
    }


  } // tryTokenizeString

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
        tryTokenizeString().getOrElse {
          input match {
            case '(' :: tail => {
              input = tail
              LeftParenToken
            }
            // ed
            case '>' :: tail => {
              input = tail
              GreaterThanToken
            }
            case '}' :: tail => {
              input = tail
              RightCurlyToken
            }
            case ')' :: tail => {
              input = tail
              RightParenToken
            }
            case '+' :: tail => {
              input = tail
              PlusToken
            }
            // imon
            case '&' :: tail => {
              input = tail
              AndToken
            }
            case ',' :: tail => {
              input = tail
              CommaToken
            }
            case '*' :: tail => {
              input = tail
              MultiplicationToken
            }
            // dan
            case ';' :: tail => {
              input = tail
              SemicolonToken
            }
            case '|' :: tail => {
              input = tail
              OrToken
            }
            case '/' :: tail => {
              input = tail
              DivisionToken
            }
            case '.' :: tail => {
              input = tail
              PeriodToken
            }
            // jiamin

            case '-' :: tail => {
              input = tail
              SubtractToken
            }
            case '<' :: tail => {
              input = tail
              LessThanToken
            }
            case '{' :: tail => {
              input = tail
              LeftCurlyToken
            }
            // steph
            case '(' :: tail => {
              input = tail
              LeftParenToken
            }
            case '=' :: tail => {
              input = tail
              EqualsToken
            }
            case '^' :: tail => {
              input = tail
              CaretToken
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
    }
  } // tokenizeOne

  def tokenize(): List[Token] = {
    @scala.annotation.tailrec
    def withAccum(accum: List[Token]): List[Token] = {
      input match {
        case _ :: _ => {
          skipWhitespace()
          input match {
            case _ :: _ => withAccum(tokenizeOne() :: accum)
            case Nil => accum.reverse.toList
          }
        }
        case Nil => accum.reverse.toList
      }
    }

    withAccum(List())
  } // tokenize

} // Lexer //
