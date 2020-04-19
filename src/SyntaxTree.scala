package src

sealed trait Types
case object IntTypes extends Types
case object BoolTypes extends Types
case object StrTypes extends Types
case object VoidTypes extends Types
case class ClassTypes(className: String) extends Types
case class MethodTypes(paramTypes: List[Types] , returnTypes: Types) extends Types


sealed trait VarDec
case class VarDeclaration(t1: Types, v1: String)extends VarDec


sealed trait Exp
case class IntegerExp(value:Int) extends Exp
case class StringExp(value:String) extends Exp
case class BooleanExp(value: Boolean) extends Exp
case class VariableExp(value: String) extends Exp
case class PrintExp(e1:Exp) extends Exp
case class MethodExp(e1:Exp , methodName: String, e2: List[Exp]) extends Exp
case class NewClassExp(className: String, e1:List[Exp] ) extends Exp
case class CastExp(t1: Types , e2: Exp) extends Exp
case class GroupedExp(e: Exp) extends Exp
case class HighOrderExp(t1: Types , v1: String, e2: Exp) extends Exp
case class CallHighOrderExp(e1:Exp , e2: Exp) extends Exp
case class LTEExp(exp: Exp, value: Exp) extends Exp
case class LTExp(exp: Exp, value: Exp) extends Exp
case class GTEExp(exp: Exp, value: Exp) extends Exp
case class GTExp(exp: Exp, value: Exp) extends Exp
case class AndExp(exp: Exp, value: Exp) extends Exp
case class OrExp(exp: Exp, value: Exp) extends Exp
case class PlusExp(exp: Exp, value: Exp) extends Exp
case class SubtractExp(exp: Exp, value: Exp) extends Exp
case class MultiplyExp(exp: Exp, value: Exp) extends Exp
case class DivideExp(exp: Exp, value: Exp) extends Exp
case class PowerExp(exp: Exp, value: Exp) extends Exp
case class EqualsExp(exp: Exp, value: Exp) extends Exp

sealed trait Stmt
case class ExpStmt(e1: Exp) extends Stmt
case class AssignmentStmt(vd1: VarDec, exp: Exp) extends Stmt
case class ForStmt(assign: Stmt, e1: Exp, inc: Stmt, forBody: Stmt) extends Stmt
case object BreakStmt extends Stmt
case class BlockStmt(s1: List[Stmt]) extends Stmt
case class ConditionalStmt(e1: Exp, condition: Stmt, ifBody: Stmt) extends Stmt
case class ReturnStmt(e1: Exp) extends Stmt
case object VoidStmt extends Stmt
case class VarStmt(name:String, e1:Exp) extends Stmt

sealed trait Method
case class DefMethod(types:Types, methodName: String,  stmt: Stmt, parameters: List[VarDec]) extends Method


sealed trait Instance
case class DecInstance(v1: VarDec) extends Instance

sealed trait Class
// Modified from: (v1: Variable, st1: Stmt, cb1: ClassBody*)  //daniel
case class DefClass(v1: String, st1: Stmt, ins: List[Instance], dec: List[VarDec], met: List[Method]) extends Class
// Modified from: (classname: Variable, extendedClass: Variable, st1: Stmt, cb1: ClassBody*)  //daniel
case class DefExtClass(classname: String, extendedClass: String, st1: Stmt, ins: List[Instance], dec: List[VarDec], met: List[Method]) extends Class

sealed trait Program
// Modified from: (e1: Exp, c1: DefClass*)  //daniel
case class Prgm(e1: Exp, c1: List[Class]) extends Program