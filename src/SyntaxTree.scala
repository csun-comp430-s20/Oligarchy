package src

sealed trait Types
case object IntTypes extends Types
case object BoolTypes extends Types
case object StrTypes extends Types
case object VoidTypes extends Types
case class ClassTypes(className: String) extends Types
case class MethodTypes(paramTypes: List[Types] , returnTypes: Types) extends Types



case class VarDeclaration(types: Types, varName: String)


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
case class AssignmentStmt(vd1: VarDeclaration, exp: Exp) extends Stmt
case class ForStmt(assign: Stmt, e1: Exp, inc: Stmt, forBody: Stmt) extends Stmt
case object BreakStmt extends Stmt
case class BlockStmt(s1: List[Stmt]) extends Stmt
case class ConditionalStmt(e1: Exp, condition: Stmt, ifBody: Stmt) extends Stmt
case class ReturnStmt(e1: Exp) extends Stmt
case object VoidStmt extends Stmt
case class VarStmt(name:String, e1:Exp) extends Stmt


case class MethodDef(types:Types, methodName: String,  stmt: Stmt, parameters: List[VarDeclaration], returnExpression: Exp)

case class InstanceDec(v1: VarDeclaration)

sealed trait Class
case class DefClass(className: String,
                    statements: Stmt,
                    instance: List[InstanceDec],
                    parameters: List[VarDeclaration],
                    methods: List[MethodDef]) extends Class
case class DefExtClass(classname: String,
                       extendedClass: String,
                       statements: Stmt,
                       instance: List[InstanceDec],
                       parameters: List[VarDeclaration],
                       methods: List[MethodDef]) extends Class



case class Program(e1: Exp, c1: List[Class])