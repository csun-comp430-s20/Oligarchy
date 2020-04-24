package src

sealed trait Types
case object IntTypes extends Types
case object BoolTypes extends Types
case object StrTypes extends Types
case object VoidTypes extends Types // should be removed
case class ClassTypes(className: String) extends Types
case class MethodTypes(paramTypes: Seq[Types] , returnTypes: Types) extends Types



case class VarDeclaration(types: Types, varName: String)


sealed trait Exp
case class IntegerExp(value:Int) extends Exp
case class StringExp(value:String) extends Exp
case class BooleanExp(value: Boolean) extends Exp
case class VariableExp(value: String) extends Exp
case class PrintExp(e1:Exp) extends Exp
case class MethodExp(e1:Exp , methodName: String, e2: List[Exp]) extends Exp
//@todo lets covert it so that it is just a list of expressions IE parameters
//case class MethodExp(methodName: String, parameters: List[Exp]) extends Exp
case class NewClassExp(className: String, e1:List[Exp] ) extends Exp
//@todo changed t1 to newTypes
case class CastExp(newTypes: Types , e2: Exp) extends Exp
case class GroupedExp(e: Exp) extends Exp
//@todo changed to have parameters declared followed by the experession
case class HighOrderExp(params: Seq[VarDeclaration], exp: Exp) extends Exp
//@todo changed exp1  to function and exp2 to params and so that it takes multiple parameters
case class CallHighOrderExp(function: Exp, params: Seq[Exp]) extends Exp
case class LTEExp(leftExp: Exp, rightExp: Exp) extends Exp
case class LTExp(leftExp: Exp, rightExp: Exp) extends Exp
case class GTEExp(leftExp: Exp, rightExp: Exp) extends Exp
case class GTExp(leftExp: Exp, rightExp: Exp) extends Exp
case class AndExp(leftExp: Exp, rightExp: Exp) extends Exp
case class OrExp(leftExp: Exp, rightExp: Exp) extends Exp
case class PlusExp(leftExp: Exp, rightExp: Exp) extends Exp
case class SubtractExp(leftExp: Exp, rightExp: Exp) extends Exp
case class MultiplyExp(leftExp: Exp, rightExp: Exp) extends Exp
case class DivideExp(leftExp: Exp, rightExp: Exp) extends Exp
case class PowerExp(leftExp: Exp, rightExp: Exp) extends Exp
case class EqualsExp(leftExp: Exp, rightExp: Exp) extends Exp

sealed trait Stmt
case class ExpStmt(e1: Exp) extends Stmt
case class AssignmentStmt(varDec: VarDeclaration, exp: Exp) extends Stmt
case class ForStmt(assign: Stmt, e1: Exp, inc: Stmt, forBody: Stmt) extends Stmt
case object BreakStmt extends Stmt
case class BlockStmt(statements: List[Stmt]) extends Stmt
case class ConditionalStmt(condition: Exp, ifTrue: Stmt, ifFalse: Stmt) extends Stmt
case class ReturnStmt(returnExp: Exp) extends Stmt
case object VoidStmt extends Stmt
case class VarStmt(variableName:String, newValue:Exp) extends Stmt


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
                       instances: List[InstanceDec],
                       parameters: List[VarDeclaration],
                       methods: List[MethodDef]) extends Class



case class Program(entryPoint: Exp, classes: List[Class])