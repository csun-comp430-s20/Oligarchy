import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Label

import org.objectweb.asm.Opcodes.IRETURN
import org.objectweb.asm.Opcodes.ICONST_M1
import org.objectweb.asm.Opcodes.ICONST_0
import org.objectweb.asm.Opcodes.ICONST_1
import org.objectweb.asm.Opcodes.ICONST_2
import org.objectweb.asm.Opcodes.ICONST_3
import org.objectweb.asm.Opcodes.ICONST_4
import org.objectweb.asm.Opcodes.ICONST_5
import org.objectweb.asm.Opcodes.INVOKEVIRTUAL
import org.objectweb.asm.Opcodes.GOTO
import org.objectweb.asm.Opcodes.IFEQ
import org.objectweb.asm.Opcodes.NEW
import org.objectweb.asm.Opcodes.DUP
import org.objectweb.asm.Opcodes.INVOKESPECIAL

class ExpressionStatementGenerator(Map[String,Class], lambdaMaker:LambdaMaker, variables:VariableTable, methodVisitor:MethodVisitor) {

  def printlnDescriptor(forType: Types): String = {
    val inner = ClassTypes((ClassGemerator.objectName)).className
    ("(" + inner + ")V")
  }

  def classDefFor(name: String): Unit = {
    val classDef = allClasses.get(name)
    classDef match {
      case null => throw CodeGeneratorException("No class named " + name)
      case _ => classDef
    }
  }

  def constructorDescriptorFor(name: String): String = {
    name match {
      case ClassGenerator.objectName => ("()V")
      case name.startsWith(LambdaMaker.LAMBDA_PREFIX) => lambdaMaker.constructorDescriptorFor(name)
      case _ => classDefFor(name).constructor.toDescriptorString()
    }
  }

  def methodDescriptorFor(className: String, methodName: String): String = {
    className match {
      case ClassGenerator.objectName => throw CodeGeneratorException("Nonexistant Method: " + methodName)
      case _ => {
        val classDef = classDefFor(className)
        val methodDef = classDef.methods
        methodDef.foreach {
          methodDef.methodName match {
            case methodName => (methodDef.methodName) //might need to change this to a toDescriptorString but I think this is what it wants
          }
        }
      }
    }
  }

  def fieldDescriptorFor(className: String, fieldName: String): String = {
    className match {
      case ClassGenerator.objectName => throw CodeGeneratorException("Nonexistant field " = fieldName.name)
      case className.startsWith(LambdaMaker.LAMBDA_PREFIX) => lambdaMaker.feildDesriptorFor(className, fieldName)
      case _ => {
        val classDef = classDefFor(className)
        val varDecs = classDef.parameters
        varDecs.foreach {
          varDecs.varName match {
            case fieldName => (varDecs.type.toDescriptorString())
          }
        }
        (fieldDescriptorFor(classDef.extendedClass, fieldName))
      }
    }
  }
  def loadVariable(variable:String):Unit ={
    variables.getEntryFor(variable).store(methodVisitor)
  }

  def storeVariable(variable:String): Unit={
    variable.getEntryFor(variable).store(methodVisitor)
  }

  def doReturn(returnType: Types):Unit ={
    returnType match{
      case IntTypes | BoolTypes => methodVisitor.visitInt(IRETURN)
        //@ToDo add refence type return
      case _ => throw new CodeGeneratorException("Unkonwn Type: " + returnType)
    }
  }

  def writeIntLiteral(value:Int): Unit ={
    value match{
      case -1 => methodVisitor.visitInsn(ICONST_M1)
      case 0 => methodVisitor.visitInsn(ICONST_0)
      case 1 => methodVisitor.visitInsn(ICONST_1)
      case 2 => methodVisitor.visitInsn(ICONST_2)
      case 3 => methodVisitor.visitInsn(ICONST_3)
      case 4 => methodVisitor.visitInsn(ICONST_4)
      case 5 => methodVisitor.visitInsn(ICONST_5)
      case _ => methodVisitor.visitLdcInsn(value)
    }
  }

  def writeArithmeticComparisonOp(exp: Exp): Unit={
    val conditionTrue = Label
    val afterCondition = Label
    exp match{
        //@ToDo add more cases for LTEEXP GTEEXP, GTEXP
      case LTExp => methodVisitor.visitJumpInsn(IF_ICMPLT, conditionTrue)
      case EqualsExp => methodVisitor.visitJumpInsn(IF_ICMPEQ, conditionTrue)
        //@ToDo assert(false) is it needed
      case _ => throw CodeGeneratorException("Unrecognized orperation:  " + exp)
    }
    writeIntLiteral(0)
    methodVisitor.visitJumpInsn(GOTO, afterCondition)
    methodVisitor.visitLable(conditionTrue)
    writeIntLiteral(1)
    methodVisitor.visitLable(afterCondition)
  }

  def writeOp(exp: Exp): Unit ={
    exp match{
      case PlusExp(leftExp, rightExp) => methodVisitor.visitInsn(IADD)
      case SubtractExp(leftExp, rightExp) => methodVisitor.visitInsn(ISUB)
      case DivideExp(leftExp, rightExp) => methodVisitor.visitInsn(IDIV)
      case MultiplyExp(leftExp, rightExp) => methodVistor.visitInsn(IMUL)
      case LTExp(leftExp, rightExp) | LTEExp(leftExp, rightExp) | GTExp(leftExp, rightExp) | GTEExp(leftExp, rightExp) | EqualsExp(leftExp, rightExp) => writeArithmeticComparisonOp(exp)
      case _ => CodeGeneratorException("unknown binary operator: " + exp)
    }
  }

  def writeMethodCall(methodExp: MethodExp): Unit={
      writeExpression(methodExp.e1)
      writeExpressions(methodExp.e2)
      methodVisitor.vistitMethodInsn(INVKOEVIRTUAL, className, methodExp.methodName, methodDescriptorFor(className, methodExp.methodName), false)
  }

  def writeNew(newExp:NewClassExp): Unit={
    methodVisitor.visitTypeInsn(NEW, newExp.className)
    methodVisitor.visitInsn(DUP)
    writeExpressions(newExp.e1)
    methodVisitor.visitMethodInsn(INVOKESPECIAL, newExp.className, "<init>", constructorDescriptorFor(newExp.className), false)
  }
  //@TODO dont know what this section is for we also dont have a lambdaExp
  //def writeGet( ) def lambdaEXP or lambdaCallExp

  def writeExpressions(exp: List[Exp]): Unit ={
    exp.foreach{
      exp => writeExpression(exp)
    }
  }

  def writeExpression(exp: Exp): Unit={
    exp match{
      case VariableExp(value) => loadVariable(exp.asInstanceOf[VariableExp].value)
      case IntegerExp(value) => writeIntLiteral(exp.asInstanceOf[IntegerExp].value)
      case BooleanExp(value) => if (exp.asInstanceOf[BooleanExp].value) writeIntLiteral( 1) else writeIntLiteral(0)
      case PlusExp(leftExp, rightExp)  => {
        val plusExp = exp.asInstanceOf[PlusExp]
        writeExpression(plusExp.leftExp)
        writeExpression(plusExp.rightExp)
        writeOp(plusExp)
      }
      case SubtractExp(leftExp, rightExp)  => {
        val subExp = exp.asInstanceOf[SubtractExp]
        writeExpression(subExp.leftExp)
        writeExpression(subExp.rightExp)
        writeOp(subExp)
      }
      case DivideExp(leftExp, rightExp)  => {
        val divExp = exp.asInstanceOf[DivideExp]
        writeExpression(divExp.leftExp)
        writeExpression(divExp.rightExp)
        writeOp(divExp)
      }
      case MultiplyExp(leftExp, rightExp)  => {
        val multExp = exp.asInstanceOf[MultiplyExp]
        writeExpression(multExp.leftExp)
        writeExpression(multExp.rightExp)
        writeOp(multExp)
      }
      case MethodExp(e1, methodName, e2) => writeMethodCall(exp.asInstanceOf[MethodExp])
      case NewClassExp(className, e1) => writeNew(exp.asInstanceOf[NewClassExp])
        //@ToDo add the lambda things
      case _ => throw CodeGeneratorException("Unrecognized expression: " + exp)
    }
  }

  def writePrint(variable: String): Unit={
    val entry = variables.getEntryFor(variable)
    val descriptor = printlnDescriptor(entry.types)
    methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", descriptor, false)
  }

  def writeIfStatement(ifStmt:ConditionalStmt): Unit ={
    val falseLabel:Label = Label
    val afterFalseLabel:Label =Label
    writeExpression(ifStmt.condition)
    methodVisitor.visitJumpInsn(IFEQ, falseLabel)
    writeStatements(ifStmt.ifTrue)
    methodVisitor.visitJumpInsn(GOTO, afterFalseLabel)
    methodVisitor.visitLabel(falseLabel)
    writeStatements(ifStmt.ifFalse)
    methodVisitor.visitLabel(afterFalseLabel)
  }

  def writeStatements(stmt:Stmt): Unit={
    stmt match {
      case ExpStmt(e1) =>
      case AssignmentStmt(varDec, exp) =>{
        val stmtEntry = stmt.asInstanceOf[AssignmentStmt]
        variables.addEntry(stmtEntry.varDec.varName, stmtEntry.varDec.types)
        writeExpression(stmtEntry.exp)
        storeVariable(stmtEntry.varDec.varName)
      }
      case ForStmt(assign, e1, inc, forBody) =>
      case BreakStmt =>
      case BlockStmt(statements) =>
      case ConditionalStmt(condition, ifTrue, ifFalse) =>
      case ReturnStmt(returnExp) =>
      case VoidStmt =>
      case VarStmt(variableName, newValue) =>{
        writeExpression(stmt.asInstanceOf[VarStmt].newValue)
        storeVariable(stmt.asInstanceOf[VarStmt].variableName)
      }
      case _=> throw CodeGeneratorException("Not a statement")
  }

}
