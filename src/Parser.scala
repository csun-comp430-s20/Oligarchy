import java.text.ParseException

sealed trait MathOp
case object PlusMathOp extends MathOp
case object MinusMathOp extends MathOp
case object MultiplicationMathOp extends MathOp
case object DivisionMathOp extends MathOp
case object CaretMathOp extends MathOp
case object LessThanMathOp extends MathOp
case object GreatThanMathOp extends MathOp
case object LessThanEqualsMathOp extends MathOp
case object GreaterThanEqualsMathOp extends MathOp

sealed trait Logic
case object AndLogic extends Logic
case object OrLogic extends Logic
case object EqualsLogic extends Logic

sealed trait Types
case object IntTypes extends Types
case object BoolTypes extends Types
case object StrTypes extends Types

sealed trait VarDec
case class Declaration(types: Var) extends VarDec

sealed trait Exp
case class IntegerExp(value: Int) extends Exp
case class LogicExp(e1: Exp, l1: Logic, e2: Exp) extends Exp
case class MathExp(e1: Exp, m1: MathOp, e2: Exp) extends Exp
case class PrintExp(e1: Exp) extends Exp
case class MethodExp(e1: Exp, methodName: Variable, e2: Exp) extends Exp
case class NewClassExp(className: Variable, e1: Exp*) extends Exp
case class CastExp(t1: Types, e2: Exp) extends Exp
case class HighOrderExp(t1: Types, v1: Variable, e2: Exp) extends Exp
case class CallHighOrderExp(e1: Exp, e2: Exp) extends Exp

sealed trait Variable
case class Var(name: String) extends Variable


sealed trait Stmt
case class ExpStmt(e1: Exp) extends Stmt
case class AssignmentStmt(vd1: VarDec, exp: Exp) extends Stmt
case class ForStmt(v1: VarDec, e1: Exp, inc: Stmt, forBody: Stmt) extends Stmt
case object BreakStmt extends Stmt
case class BlockStmt(s1: Stmt*) extends Stmt
case class ConditionalStmt(e1: Exp, condition: Stmt, ifBody: Stmt) extends Stmt
case class ReturnStmt(e1: Exp) extends Stmt
case object VoidStmt extends Stmt

sealed trait Method
case class DefMethod(types: Types, methodName: Variable, stmt: Stmt, parameters: VarDec*) extends Method

sealed trait Instance
case class DecInstance(v1: VarDec) extends Instance

sealed trait ClassBody
case class InstanceClassBody(in1: DecInstance*) extends ClassBody
case class MethodClassBody(md1: DefMethod*) extends ClassBody
case class DeclarationClassBody(vd1: VarDec*) extends ClassBody

sealed trait Class
case class DefClass(v1: Variable, st1: Stmt, cb1: ClassBody*) extends Class
case class DefExtClass(classname: Variable, extendedClass: Variable, st1: Stmt, cb1: ClassBody*) extends Class

sealed trait Program
case class Prgm(e1: Exp, c1: DefClass*) extends Program


case class ParserException(msg: String) extends Exception(msg)

object Parser {
  def apply(input: List[Token]): Parser = {
    new Parser(input)
  }
}

class Parser(private var input: List[Token]) {


  private def parseStmt(tokens: List[Token]): (Stmt, List[Token]) = {
    tokens match { //for (vardec; exp; stmt) stmt
      case ForToken :: LeftParenToken :: tail => {
        val (vardec: VarDec, restTokens: List[Token]) = parseVardec(tail)
        restTokens match {
          case SemicolonToken :: restTokens2 => {
            var (exp: Exp, restTokens3: List[Token]) = parseExp(restTokens2)
            restTokens3 match {
              case SemicolonToken :: restTokens4 => {
                var (stmt1: Stmt, restTokens5: List[Token]) = parseStmt(restTokens4)
                restTokens5 match {
                  case LeftParenToken :: restTokens6 => {
                    var (stmt2: Stmt, finalTokens: List[Token]) = parseStmt(restTokens6)
                    (ForStmt(vardec, exp, stmt1, stmt2), finalTokens)
                  }
                  case _ => throw ParserException("missing LeftParenToken in ForStatement")
                }
              }
              case _ => throw ParserException("missing semicolon after exp in ForStatement")
            }
          }
          case _ => throw ParserException("missing semicolon after vardec in ForStatement")
        }
      }
      case BreakToken :: SemicolonToken :: tail => {
        (BreakStmt, tail)
      }
      case IfToken :: LeftParenToken :: tail => {
        var (exp: Exp, restTokens:List[Token]) = parseExp(tail)
        restTokens match {
          case RightParenToken :: restTokens2 => {
            var (stmt1: Stmt, restTokens3: List[Token]) = parseStmt(restTokens2)
            restTokens3 match {
              case ElseToken :: restTokens4 => {
                var (stmt2: Stmt, finalTokens: List[Token]) = parseStmt(restTokens4)
                (ConditionalStmt(exp, stmt1, stmt2), finalTokens)
              }
              case _ => throw ParserException("Missing ElseToken")
            }
          }
          case _ => throw ParserException("missing RightParen Token after Exp in If Statment")
        }
      }
      case ReturnToken :: tail => {
        tail match {
          case SemicolonToken :: restTokens => {
            (VoidStmt, restTokens)
          }
          case _ => {
            try {
              val (exp: Exp, restTokens: List[Token]) = parseExp(tail)
              restTokens match {
                case SemicolonToken :: finalTokens => {
                  (ReturnStmt(exp), finalTokens)
                }
                case _ => throw ParserException("No SemicolonToken after expression in return statement")
              }
            }
            catch{
              case _ => throw ParserException("Invalid return statement")
            }
          }
        }
      }
      case LeftCurlyToken :: tail =>{
        var(stmts: List[Stmt], restTokens: List[Token]) = parseRepeat(tail, parseStmt)
        (BlockStmt(stmts), restTokens)
      }
      case _ => {
        try {
          val (exp: Exp, finalTokens: List[Token]) = parseExp(tail)
          finalTokens match {
            case SemicolonToken :: finalTokens => {
              (ExpStmt(exp), finalTokens)
            }
            case _ => throw ParserException("no SemicolonToken after expression in Exp Statement")
          }
        }
        catch {
          try {
            val (vardec: VarDec, restTokens: List[Token]) = parseVardec(tail)
            restTokens match{
              case EqualsToken :: restTokens2 =>{
                val(exp: Exp, restTokens3:List[Token]) = parseExp(restTokens2)
                restTokens3 match{
                  case SemicolonToken :: finalTokens => {
                    (AssignmentStmt(vardec, exp), finalTokens)
                  }
                  case _ => throw ParserException("No SemicolonToken after expression in AssignmentStatement")
                }
              }
              case _ => throw ParserException("No EqualsToken in Assignment Statement")
            }
          }
          catch{
            case _ => throw ParserException("Not Statement")
          }
        }
      }
    }
  }

  def parseRepeat[A] (tokens: List[Token], parseOne: List[Token] => (A, List[Token])): (List[A], List[Token]) ={
    try{
      val (a, restTokens) = parseOne(tokens)
      val(restAs, finalRestTokens) = parseRepeat(restTokens, parseOne)
      (a:: restAs, finalRestTokens)
    }
    catch{
      case _: ParserException => (List(), tokens)
    }
  }

}

