
sealed trait Types {
  def toDescriptorString: String
}

case object IntTypes extends Types() {
  def toDescriptorString: String = {
    "I"
  }
}

case object BoolTypes extends Types() {
  def toDescriptorString: String = {
    "Z"
  }
}

case object VoidTypes extends Types() {
  def toDescriptorString: String = {
    "V"
  }
}

case class ClassTypes(className: String) extends Types() {
  def toDescriptorString: String = {
    ("L" + className + ";")
  }
}

case class MethodTypes(paramTypes: List[Types], returnTypes: Types) extends Types() {
  def toDescriptorString: String = {
    val result = new StringBuffer
    result.append("(")
    for (param <- paramTypes) {
      result.append(param.toDescriptorString)
    }
    result.append(")")
    result.append(returnTypes.toDescriptorString)
    result.toString
  }
}

case class HighOrderFuncType(paramType: Types, returnTypes: Types) extends Types() {
  def toDescriptorString: String = "LFunction1<" + paramType.toDescriptorString + returnTypes.toDescriptorString + ">;"
}

case class VarDeclaration(types: Types, varName: String)() {

}


sealed trait Exp

case class IntegerExp(value: Int) extends Exp

case class StringExp(value: String) extends Exp

case class BooleanExp(value: Boolean) extends Exp

case class VariableExp(value: String) extends Exp

//case class MethodExp(e1:Exp , methodName: String, e2: List[Exp]) extends Exp
case class MethodExp(callVariable: Exp, className: String, methodName: String, params: List[Exp]) extends Exp

case class NewClassExp(className: String, params: List[Exp]) extends Exp

case class GroupedExp(e: Exp) extends Exp

//case class HighOrderExp(params: List[VarDeclaration], body: Exp) extends Exp
case class HighOrderExp(param: String, paramType: ClassTypes, returnType: Types, body: Exp) extends Exp

//case class CallHighOrderExp(function: Exp, params: List[Exp]) extends Exp
case class CallHighOrderExp(lambda: Exp, returnType: ClassTypes, param: Exp) extends Exp

sealed trait BOP extends Exp {
  val leftExp: Exp
  val rightExp: Exp
}

case class LTEExp(leftExp: Exp, rightExp: Exp) extends BOP

case class LTExp(leftExp: Exp, rightExp: Exp) extends BOP

case class GTEExp(leftExp: Exp, rightExp: Exp) extends BOP

case class GTExp(leftExp: Exp, rightExp: Exp) extends BOP

case class AndExp(leftExp: Exp, rightExp: Exp) extends BOP

case class OrExp(leftExp: Exp, rightExp: Exp) extends BOP

case class PlusExp(leftExp: Exp, rightExp: Exp) extends BOP

case class SubtractExp(leftExp: Exp, rightExp: Exp) extends BOP

case class MultiplyExp(leftExp: Exp, rightExp: Exp) extends BOP

case class DivideExp(leftExp: Exp, rightExp: Exp) extends BOP

case class PowerExp(leftExp: Exp, rightExp: Exp) extends BOP

case class EqualsExp(leftExp: Exp, rightExp: Exp) extends BOP

sealed trait Stmt

case class ExpStmt(e1: Exp) extends Stmt

case class AssignmentStmt(varDec: VarDeclaration, exp: Exp) extends Stmt

case class ForStmt(assign: Stmt, e1: Exp, inc: Stmt, forBody: Stmt) extends Stmt

case class BlockStmt(statements: List[Stmt]) extends Stmt

case class ConditionalStmt(condition: Exp, ifTrue: Stmt, ifFalse: Stmt) extends Stmt

case class ReturnStmt(returnExp: Exp) extends Stmt

case object VoidStmt extends Stmt

case class VarStmt(variableName: String, newValue: Exp) extends Stmt

case class PrintExp(e1: Exp) extends Stmt

case object MethodDef {
  def toDescriptorString(formalParams: List[VarDeclaration], returnType: Types): String = {
    val result = new StringBuffer
    result.append("(")
    for (param <- formalParams) {
      result.append(param.types.toDescriptorString)
    }
    result.append(")")
    result.append(returnType.toDescriptorString)
    result.toString
  } // toDescriptorString
}

case class MethodDef(types: Types, methodName: String, stmt: Stmt, parameters: List[VarDeclaration], returnExpression: Exp) {
  def toDescriptorString: String = {
    MethodDef.toDescriptorString(parameters, types)
  }

}

case class InstanceDec(v1: VarDeclaration)

sealed trait Class {
  val className: String
  val extendedClass: String
  val statements: Stmt
  val instances: List[InstanceDec]
  val parameters: List[VarDeclaration]
  val methods: List[MethodDef]
}

case class DefClass(className: String,
                    extendedClass: String = null,
                    statements: Stmt,
                    instances: List[InstanceDec],
                    parameters: List[VarDeclaration],
                    methods: List[MethodDef]) extends Class

case class DefExtClass(className: String,
                       extendedClass: String,
                       statements: Stmt,
                       instances: List[InstanceDec],
                       parameters: List[VarDeclaration],
                       methods: List[MethodDef]) extends Class


case class Program(entryPoint: Stmt, classes: List[Class])