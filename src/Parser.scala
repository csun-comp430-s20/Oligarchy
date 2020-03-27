import java.sql.Statement

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
case class Declaration(types: Var)extends VarDec

sealed trait Exp
case class IntegerExp(value:Int) extends Exp
case class LogicExp(e1:Exp , l1: Logic, e2: Exp) extends Exp
case class MathExp(e1:Exp , m1: MathOp, e2: Exp) extends Exp
case class PrintExp(e1:Exp) extends Exp
case class MethodExp(e1:Exp , methodName: Variable, e2: Exp) extends Exp
case class NewClassExp(className: Variable, e1:Exp* ) extends Exp
case class CastExp(t1: Types , e2: Exp) extends Exp
case class HighOrderExp(t1: Types , v1: Variable, e2: Exp) extends Exp
case class CallHighOrderExp(e1:Exp , e2: Exp) extends Exp

sealed trait Variable
case class Var(name:String) extends Variable


sealed trait Stmt
case class ExpStmt(e1: Exp) extends Stmt
case class AssignmentStmt(v1: Variable, exp: Exp) extends Stmt
case class AssignmentStmtVardec(vd1: VarDec, exp: Exp) extends Stmt
case class ForStmt(v1: VarDec, e1: Exp, inc: Stmt, forBody: Stmt) extends Stmt
case object BreakStmt extends Stmt
case class BlockStmt(s1: Stmt) extends Stmt
case class ConditionalStmt(e1: Exp, condition: Stmt, ifBody: Stmt) extends Stmt
case class ReturnStmt(e1: Exp) extends Stmt
case object VoidStmt extends Stmt

sealed trait Method
case class DefMethod(types:Types, methodName: Variable,  stmt: Stmt, parameters: VarDec*) extends Method

sealed trait Instance
case class DecInstance(v1: VarDec) extends Instance

sealed trait ClassBody
case class InstanceClassBody(in1: DecInstance*) extends ClassBody
case class MethodClassBody(md1: DefMethod*) extends ClassBody
case class DeclarationClassBody(vd1: VarDec*) extends ClassBody

sealed trait Class
// Modified from: (v1: Variable, st1: Stmt, cb1: ClassBody*)  //daniel
case class DefClass(v1: String, st1: Stmt, ins: List[InstanceClassBody], dec: List[DeclarationClassBody], met: List[MethodClassBody]) extends Class
// Modified from: (classname: Variable, extendedClass: Variable, st1: Stmt, cb1: ClassBody*)  //daniel
case class DefExtClass(classname: String, extendedClass: String, st1: Stmt, ins: List[InstanceClassBody], dec: List[DeclarationClassBody], met: List[MethodClassBody]) extends Class

sealed trait Program
// Modified from: (e1: Exp, c1: DefClass*)  //daniel
case class Prgm(e1: Exp, c1: Class*) extends Program



case class ParserException(msg: String) extends Exception(msg)

object Parser {
  def apply(input: Seq[Token]): Parser = {
    new Parser(input)
  }
}

class Parser(private var input: Seq[Token]) {

  //Daniel
  private def parseProgram(tokens: List[Token]): (Prgm, List[Token]) = {
    tokens match {
      case DefClassToken :: tail =>{
        var(classes: List[Class], restTokens: List[Token]) = parseRepeat(tail, parseClass)
        restTokens match {
          case ExpToken :: restTokens2 => {
            val(exp: Exp, restTokens3: List[Token]) = parseExp(restTokens2)
            (Prgm(exp, classes), restTokens3)
          }
          case _ => throw ParserException("Missing Expression Token after Classes")
        }
      }
      case _ => throw ParserException("Missing atleast one Class Definition")
    }
  }  //ParseProgram

  //Daniel
  private def parseClass(tokens: List[Token]): (Class, List[Token]) = {
    tokens match {
      case ClassToken :: VarToken(classname: String) :: ExtendsToken :: VarToken(extendclassname: String) :: LeftCurlyToken :: tail => {
        var(instances: List[InstanceClassBody], restTokens: List[Token]) = parseRepeat(tail, parseInstanceClassBody)
        restTokens match {
          case ConstructorToken :: LeftParenToken :: tail => {
            var(declarations: List[DeclarationClassBody], restTokens2: List[Token]) = parseRepeat(tail, parseDeclarationClassBody)
            restTokens2 match {
              case RightParenToken :: tail => {
                val (stmt, restTokens3) = parseStmt(tail)
                var(methods: List[MethodClassBody], restTokens4: List[Token]) = parseRepeat(tail, parseMethodClassBody)
                (DefExtClass(classname, extendclassname, stmt, instances, declarations, methods), restTokens4)
              }
              case _ => throw parserException("Not a DefExtClass")
            }
          }
          case _ => throw parserException("Expected Constructor and LeftParen")
        }
      }  //Extended Class
      case ClassToken :: VarToken(classname: String) :: LeftCurlyToken :: tail => {
        var(instances: List[InstanceClassBody], restTokens: List[Token]) = parseRepeat(tail, parseInstanceClassBody)
        restTokens match {
          case ConstructorToken :: LeftParenToken :: tail => {
            var(declarations: List[DeclarationClassBody], restTokens2: List[Token]) = parseRepeat(tail, parseDeclarationClassBody)
            restTokens2 match {
              case RightParenToken :: tail => {
                val (stmt, restTokens3) = parseStmt(tail)
                var(methods: List[MethodClassBody], restTokens4: List[Token]) = parseRepeat(tail, parseMethodClassBody)
                (DefClass(classname, stmt, instances, declarations, methods), restTokens4)
              }
              case _ => throw parserException("Not a DefClass")
            }
          }
          case _ => throw parserException("Expected Constructor and LeftParen")
        }
      }  //Basic Class
      case _ => throw PraserException("Not a proper Class Definition")
    }
  }  //Parse Classes


  def parseRepeat[A] (tokens: List[Token], parseOne: List[Token] => (A, List[Token])): (List[A], List[Token]) = {
    try {
      val (a, restTokens) = parseOne(tokens)
      val (restAs, finalRestTokens) = parseRepeat(restTokens, parseOne)
      (a :: restAs, finalRestTokens)
    }
    catch {
      case _: ParserException => (List(), tokens)
    }
  }  //parseRepeat

}


