import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Label
import org.objectweb.asm.Opcodes._


case object ExpressionStatementGenerator{
  def printlnDescriptor(forType: Types): String = {
    val inner = ClassTypes((ClassGenerator.objectName)).className
    ("(" + inner + ")V")
  }
}

case class ExpressionStatementGenerator(allClasses: Map[String, Class], lambdaMaker: LambdaMaker, variables: VariableTable, methodVisitor: MethodVisitor) {

  def classDefFor(name: String): Class = {
    if (allClasses contains name) {
      allClasses(name)
    } else {
      throw new CodeGeneratorException("No Class named " + name)
    }
  }

  def methodDescriptorFor(className: String, methodName: String): String = {
    className match {
      case ClassGenerator.objectName => throw new CodeGeneratorException("Nonexistant Method: " + methodName)
      case _ => {
        val classDef: Class = classDefFor(className)
        val methodDef: List[MethodDef] = classDef.methods
        methodDef.foreach( method=> {
          if (method.methodName equals methodName) (method.toDescriptorString())
        })
        (methodDescriptorFor(classDef.extendedClass, methodName))
      }
    }
  }

  def fieldDescriptorFor(className: String, fieldName: String): String = {
    className match {
      case ClassGenerator.objectName => throw new CodeGeneratorException("Nonexistant field " + fieldName)
      case className => if(className.startsWith(LambdaMaker.LAMBDA_PREFIX)) lambdaMaker.fieldDescriptorFor(className, fieldName) else
        throw new CodeGeneratorException("doesnt start with lambda prefix")
      case _ => {
        val classDef: Class = classDefFor(className)
        val varDecs: List[InstanceDec] = classDef.instances
        varDecs.foreach(instanceDec =>{
          if (instanceDec.v1.varName equals fieldName) (instanceDec.v1.types.toDescriptorString())
        })
        (fieldDescriptorFor(classDef.extendedClass, fieldName))
      }
    }
  }

  def loadVariable(variable: String): Unit = {
    variables.getEntryFor(variable).store(methodVisitor)
  }

  def storeVariable(variable: String): Unit = {
    variables.getEntryFor(variable).store(methodVisitor)
  }

  def doReturn(returnType: Types): Unit = {
    returnType match {
      case IntTypes | BoolTypes => methodVisitor.visitInsn(IRETURN)
      case classTypes:ClassTypes => methodVisitor.visitInsn(ARETURN)
      case _ => throw new CodeGeneratorException ("Unkonwn Type: " + returnType)
    }
  }
  //@Todo writeGet not sure what get is in our language

  def writeLambdaExp(highOrderExp: HighOrderExp): Unit={
    writeExpression(lambdaMaker.translateLambda(highOrderExp, variables))
  }

  def writeLambdaCallExp(callhighOrderExp: CallHighOrderExp): Unit={
    writeExpression(callhighOrderExp.lambda)
    writeExpression(callhighOrderExp.param)
    methodVisitor.visitMethodInsn(INVOKEINTERFACE, LambdaMaker.EXTENDS_NAME, LambdaMaker.APPLY_NAME, LambdaDef.bridgeApplyDescriptorString(), true)
    methodVisitor.visitTypeInsn(CHECKCAST, callhighOrderExp.returnType.className) // we need to change how our higher order functions are called
  }

  def writeIntLiteral(value: Int): Unit = {
    value match {
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

  def writeArithmeticComparisonOp(exp: Exp): Unit = {
    val conditionTrue = new Label()
    val afterCondition = new Label()
    exp match {
      case LTEExp(leftExp, rightExp) => methodVisitor.visitJumpInsn(IF_ICMPLT, conditionTrue)
      case LTExp(leftExp, rightExp) => methodVisitor.visitJumpInsn(IF_ICMPLE, conditionTrue)
      case GTEExp(leftExp, rightExp) => methodVisitor.visitJumpInsn(IF_ICMPGE, conditionTrue)
      case GTExp(leftExp, rightExp) => methodVisitor.visitJumpInsn(IF_ICMPGT, conditionTrue)
      case EqualsExp(leftExp, rightExp) => methodVisitor.visitJumpInsn(IF_ACMPEQ, conditionTrue)
      case _ => throw new CodeGeneratorException ("Unrecognized operation: " + exp)
    }
    writeIntLiteral(0)
    methodVisitor.visitJumpInsn(GOTO, afterCondition)
    methodVisitor.visitLabel(conditionTrue)
    writeIntLiteral(1)
    methodVisitor.visitLabel(afterCondition)
  }

  def writeOp(exp: Exp): Unit = {
    exp match {
      case PlusExp(leftExp, rightExp) => methodVisitor.visitInsn(IADD)
      case SubtractExp(leftExp, rightExp) => methodVisitor.visitInsn(ISUB)
      case DivideExp(leftExp, rightExp) => methodVisitor.visitInsn(IDIV)
      case MultiplyExp(leftExp, rightExp) => methodVisitor.visitInsn(IMUL)
      case LTExp(_, _) | LTEExp(_, _) | GTExp(_, _) | GTEExp(_, _) | EqualsExp(_, _) => writeArithmeticComparisonOp(exp)
      case _ => new CodeGeneratorException("unknown binary operator: " + exp)
    }
  }

  def writeMethodCall(methodExp: MethodExp): Unit = {
    writeExpression(methodExp.callVariable)
    writeExpressions(methodExp.params)
    methodVisitor.visitMethodInsn(INVOKEVIRTUAL, methodExp.className, methodExp.methodName, methodDescriptorFor(methodExp.className, methodExp.methodName), false)
  }

  def writeNew(newExp: NewClassExp): Unit = {
    methodVisitor.visitTypeInsn(NEW, newExp.className)
    methodVisitor.visitInsn(DUP)
    writeExpressions(newExp.params)
    methodVisitor.visitMethodInsn(INVOKESPECIAL, newExp.className, "<init>", null, false)
  }

  def writeExpressions(exp: List[Exp]): Unit = {
    exp.foreach {
      exp => writeExpression(exp)
    }
  }

  def writeExpression(exp: Exp): Unit = {
    exp match {
      case VariableExp(value) => loadVariable(value)
      case IntegerExp(value) => writeIntLiteral(value)
      case BooleanExp(value) => writeIntLiteral(if (value) 1 else 0)
      case plusExp: PlusExp => {
        writeExpression(plusExp.leftExp)
        writeExpression(plusExp.rightExp)
        writeOp(plusExp)
      }
      case subExp: SubtractExp => {
        writeExpression(subExp.leftExp)
        writeExpression(subExp.rightExp)
        writeOp(subExp)
      }
      case divExp: DivideExp => {
        writeExpression(divExp.leftExp)
        writeExpression(divExp.rightExp)
        writeOp(divExp)
      }
      case multExp: MultiplyExp => {
        writeExpression(multExp.leftExp)
        writeExpression(multExp.rightExp)
        writeOp(multExp)
      }
      case methodExp: MethodExp => writeMethodCall(methodExp)
      case NewClassExp(className, e1) => writeNew(exp.asInstanceOf[NewClassExp])
      case newExp: NewClassExp => writeNew(newExp)
      case GroupedExp(e) => writeExpression(e)
      case highOrderExp:HighOrderExp => writeLambdaExp(highOrderExp)
      case callHighOrderExp: CallHighOrderExp => writeLambdaCallExp(callHighOrderExp)
      case _ => throw new CodeGeneratorException("Unrecognized expression: " + exp)
    }
  }

  def writePrint(variable: String): Unit = {
    val entry = variables.getEntryFor(variable)
    val descriptor = ExpressionStatementGenerator.printlnDescriptor(entry.types)
    methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", descriptor, false)
  }

  def writeIfStatement(ifStmt: ConditionalStmt): Unit = {
    val falseLabel: Label = new Label()
    val afterFalseLabel: Label = new Label()
    writeExpression(ifStmt.condition)
    methodVisitor.visitJumpInsn(IFEQ, falseLabel)
    writeStatements(ifStmt.ifTrue)
    methodVisitor.visitJumpInsn(GOTO, afterFalseLabel)
    methodVisitor.visitLabel(falseLabel)
    writeStatements(ifStmt.ifFalse)
    methodVisitor.visitLabel(afterFalseLabel)
  }

  def writeForStatement(forStmt: ForStmt): Unit ={
    val head = new Label()
    val afterFor = new Label()
    methodVisitor.visitLabel(head)
    writeStatements(forStmt.assign)
    writeExpression(forStmt.e1)
    methodVisitor.visitJumpInsn(IFEQ, afterFor)
    writeStatements(forStmt.forBody)
    writeStatements(forStmt.inc)
    methodVisitor.visitJumpInsn(GOTO, head)
    methodVisitor.visitLabel(afterFor)
  }
  def writeStatements(stmt: Stmt): Unit = {
    stmt match {
      case expStmt:ExpStmt => writeExpression(expStmt.e1)
      case AssignmentStmt(varDec, exp) => {
        val stmtEntry = stmt.asInstanceOf[AssignmentStmt]
        variables.addEntry(stmtEntry.varDec.varName, stmtEntry.varDec.types)
        writeExpression(stmtEntry.exp)
        storeVariable(stmtEntry.varDec.varName)
      }
      case forStmt:ForStmt => writeForStatement(forStmt)
//      case BreakStmt => ???
      case BlockStmt(statements) => for (statement: Stmt <- statements) {
        writeStatements(statement)
      }
      case stmt:ConditionalStmt => writeIfStatement(stmt)
      case returnStmt:ReturnStmt => writeExpression(returnStmt.returnExp)
      case VarStmt(variableName, newValue) => {
        writeExpression(stmt.asInstanceOf[VarStmt].newValue)
        storeVariable(stmt.asInstanceOf[VarStmt].variableName)
      }
      case _ => throw new CodeGeneratorException("Not a statement")
    }

  }
}