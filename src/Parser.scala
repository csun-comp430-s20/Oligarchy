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
case class DefClass(v1: Variable, st1: Stmt, cb1: ClassBody*) extends Class
case class DefExtClass(classname: Variable, extendedClass: Variable, st1: Stmt, cb1: ClassBody*) extends Class

sealed trait Program
case class Prgm(e1: Exp, c1: DefClass*) extends Program




case class ParserException(msg: String) extends Exception(msg)

object Parser {
  def apply(input: Seq[Token]): Parser = {
    new Parser(input)
  }
}

class Parser(private var input: Seq[Token]) {
  private def ParseIntegerExpression(position: Int): (IntegerExp, Int)={
    input(position) match{
      case head: IntegerToken =>{
        (IntegerExp(head.value), position + 1)
      }
      case _ =>{
        throw new ParserException("not an integer")
      }
    }
  }





  // work that needs to be done
  // adding terminals to expression
  // check if something is a certain token
  // parse program
   /*
    parse program is going to take a seq of tokens and check if they are
    a series of class definitions followed by an experession  and return a program class
    otherwise throw a parse exception

    */

  //
//  private def parseProgram():Prgm ={
//    val (classDef:DefClass, position: Int) = parseClassDef(0)
//    val (expression:Exp, nextPos: Int )= parseExpression(position)
//    Prgm(expression,classDef)
//  }
//
//  private def parseExpression(position: Int):(Exp,Int)={
//
//  }
//  private def parseClassDef(position: Int):( DefClass, Int)={
//
//  }
  // parse classdef
  /*
    checks for class token
   */
  // parse instancedec
  // parse methoddef
  // parse stmt
  // parse vardec
  // parse exp
  // parse mathExp
    // precedence
  //

}

