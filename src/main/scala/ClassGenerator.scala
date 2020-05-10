import java.io.File
import java.sql.Statement

import org.objectweb.asm.Label
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

class CodeGeneratorException(val message: String) extends Exception(message)

class ClassGenerator(program: Program) {
  val thisVariable = "this"
  val objectName = "java/lang/Object"
  var allClasses: Map[String, Class] = _
  //  lambdaMaker: LambdaMaker

  // should be similar to the constructor method in java here we will set up how we will generate code
  def apply(program: Program): ClassGenerator = {
    program.classes.foreach(myClass => {
      if (this.allClasses contains (myClass.className)) {
        throw new CodeGeneratorException("redefining a defined class " + myClass.className)
      }
      this.allClasses += (myClass.className -> myClass)
    })
    this
  } //apply

  def printlnDescriptorString(forType: Types): String = {
    var inner: String = ""
    forType match {
      case ClassTypes(_) => inner = ClassTypes(objectName).toDescriptorString
      case _ => inner = forType.toDescriptorString
    }
    return "(" + inner + ")V"
  } // printlnDescriptorString

  import java.io.IOException

  @throws[CodeGeneratorException]
  def writeClasses(): Unit = {
    for (classDef <- allClasses.values) {
      val genClass = SingleClassGenerator(classDef)
      genClass.writeClass
    }
  } // writeClasses

  @throws[CodeGeneratorException]
  private def classDefFor(name: String): Class = {
    if (allClasses contains name) {
      allClasses(name)
    } else {
      throw new CodeGeneratorException("No such class: " + name)
    }
  } // classDefFor

  @throws[CodeGeneratorException]
  private def methodDescriptorFor(className: String, methodName: String): String = {
    if (className.equals(objectName)) throw new CodeGeneratorException("Nonexistant method: " + methodName)
    else {
      val classDef = classDefFor(className)
      for (methodDef <- classDef.methods) {
        if (methodDef.methodName.equals(methodName)) return methodDef.toDescriptorString
      }
      methodDescriptorFor(classDef.extendedClass, methodName)
    }
  } // methodDescriptorFor

  // we wont be adding constructors
  //  @throws[CodeGeneratorException]
  //  private def constructorDescriptorFor(name: String):String = {
  //    if (name.equals(objectName)) "()V"
  //    else classDefFor(name).constructor.toDescriptorString(name)
  //  } // constructorDescriptorFor


  @throws[CodeGeneratorException]
  private def fieldDescriptorFor(className: String, fieldName: String): String = {
    if (className.equals(objectName)) throw new CodeGeneratorException("Nonexistant field: " + fieldName)
    else {
      val classDef = classDefFor(className)
      for (field <- classDef.instances) {
        if (field.v1.varName.equals(fieldName)) return field.v1.types.toDescriptorString
      }
      fieldDescriptorFor(classDef.extendedClass, fieldName)
    }
  } // fieldDescriptorFor


  case class SingleClassGenerator(classDef: Class) {

    private var forClass: Class = _
    private var thisType: ClassTypes = _
    private var classWriter: ClassWriter = _


    def apply(forClass: Class) {
      this.forClass = forClass
      this.thisType = ClassTypes(forClass.className)
      this.classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
      classWriter.visit(V1_7, // Java 1.7
        ACC_PUBLIC, // public
        forClass.className, // class name
        null, // signature (null means not generic)
        forClass.extendedClass, // superclass if null is passed it checks for it and corrects it
        new Array[String](0)) // interfaces (none)

      for (field <- forClass.instances) {
        classWriter.visitField(ACC_PUBLIC, field.v1.varName, field.v1.types.toDescriptorString, null, null).visitEnd()
      }
    } // SingleClassGenerator

    import java.io.BufferedOutputStream
    import java.io.FileOutputStream
    import java.io.IOException

    @throws[CodeGeneratorException]
    def writeClass(): Unit = {
      // we dont have constructors or main
      for (method <- forClass.methods) {
        val methodGen = SingleMethodGenerator(method)
        methodGen.writeMethod()
      }
      classWriter.visitEnd()
      val output = new BufferedOutputStream(new FileOutputStream(new File(forClass.className + ".class")))
      output.write(classWriter.toByteArray)
      output.close()
    } // writeClass

    case class SingleMethodGenerator(m: MethodDef) {

      private var method: MethodDef = _
      private var variables: VariableTable = _
      private var methodVisitor: MethodVisitor = _

      def apply(methodDef: MethodDef): SingleMethodGenerator = {
        this.method = methodDef
        val flags = ACC_PUBLIC
        variables = variables.withFormalParamsFrom(thisType, method)
        methodVisitor = classWriter.visitMethod(flags, method.methodName, method.toDescriptorString(), null, null)
        this
      }

      @throws[CodeGeneratorException]
      private def loadVariable(variable: String): Unit = {
        variables.getEntryFor(variable).load(methodVisitor)
      } // loadVariable

      @throws[CodeGeneratorException]
      private def storeVariable(variable: String): Unit = {
        variables.getEntryFor(variable).store(methodVisitor)
      } // storeVariable

      @throws[CodeGeneratorException]
      private def doReturn(returnType: Types): Unit = {
        returnType match {
          case IntTypes | BoolTypes => methodVisitor.visitInsn(IRETURN)
          case VoidTypes => methodVisitor.visitInsn(RETURN)
          case ClassTypes(className) | MethodTypes(paramTypes, returnTypes) => methodVisitor.visitInsn(ARETURN)
          case _ => throw new CodeGeneratorException("unknown type" + returnType)
        }
      } // doReturn

      private def writeIntLiteral(value: Int): Unit = {
        value match {
          case -1 =>
            methodVisitor.visitInsn(ICONST_M1)
          case 0 =>
            methodVisitor.visitInsn(ICONST_0)
          case 1 =>
            methodVisitor.visitInsn(ICONST_1)
          case 2 =>
            methodVisitor.visitInsn(ICONST_2)
          case 3 =>
            methodVisitor.visitInsn(ICONST_3)
          case 4 =>
            methodVisitor.visitInsn(ICONST_4)
          case 5 =>
            methodVisitor.visitInsn(ICONST_5)
          case _ =>
            methodVisitor.visitLdcInsn(Integer.valueOf(value))
        }
      } // writeIntLiteral

      @throws[CodeGeneratorException]
      private def writeArithmeticComparisonOp(bop: Exp): Unit = {
        val conditionTrue = new Label()
        val afterCondition = new Label()
        bop match {
          case LTEExp(leftExp, rightExp) => methodVisitor.visitJumpInsn(IF_ICMPLT, conditionTrue)
          case LTExp(leftExp, rightExp) => methodVisitor.visitJumpInsn(IF_ICMPLE, conditionTrue)
          case GTEExp(leftExp, rightExp) => methodVisitor.visitJumpInsn(IF_ICMPGE, conditionTrue)
          case GTExp(leftExp, rightExp) => methodVisitor.visitJumpInsn(IF_ICMPGT, conditionTrue)
          case EqualsExp(leftExp, rightExp) => methodVisitor.visitJumpInsn(IF_ACMPEQ, conditionTrue)
          case _ => throw new CodeGeneratorException("Unrecognized operation: " + bop)
        }
        writeIntLiteral(0)
        methodVisitor.visitJumpInsn(GOTO, afterCondition)
        methodVisitor.visitLabel(conditionTrue)
        writeIntLiteral(1)
        methodVisitor.visitLabel(afterCondition)
      } // writeArithmeticComparisonOp

      @throws[CodeGeneratorException]
      private def writeOp(bop: Exp): Unit = {
        bop match {
          case PlusExp(leftExp, rightExp) => methodVisitor.visitInsn(IADD)
          case SubtractExp(leftExp, rightExp) => methodVisitor.visitInsn(ISUB)
          case DivideExp(leftExp, rightExp) => methodVisitor.visitInsn(IDIV)
          case MultiplyExp(leftExp, rightExp) => methodVisitor.visitInsn(IMUL)
          case LTEExp(_, _) |
               LTExp(_, _) |
               GTEExp(_, _) |
               GTExp(_, _) |
               EqualsExp(_, _) => writeArithmeticComparisonOp(bop)
          case _ => throw new CodeGeneratorException("unknown bop" + bop)
          //          case PowerExp(leftExp, rightExp) => will get rid of
        }
      } // writeOp
      @throws[CodeGeneratorException]
      private def writeMethodCall(call: MethodExp): Unit = {
        writeExpression(call.callVariable)
        writeExpressions(call.params)
        methodVisitor.visitMethodInsn(INVOKEVIRTUAL, call.className, call.methodName, methodDescriptorFor(call.className, call.methodName), false)
      } // writeMethodCall

      @throws[CodeGeneratorException]
      private def writeNew(newExp: NewClassExp): Unit = {
        methodVisitor.visitTypeInsn(NEW, newExp.className)
        // calling the constructor will pop this off
        methodVisitor.visitInsn(DUP)
        writeExpressions(newExp.params)
        methodVisitor.visitMethodInsn(INVOKESPECIAL, newExp.className, "<init>", "()V", false)
      } // writeNew

      // we did not have a get
      //      @throws[CodeGeneratorException]
      //      private def writeGet(getExp: Nothing): Unit = {
      //        writeExpression(getExp.target)
      //        methodVisitor.visitFieldInsn(GETFIELD, getExp.name.name, getExp.field.name, fieldDescriptorFor(getExp.name, getExp.field))
      //      } // writeGet


      @throws[CodeGeneratorException]
      private def writeExpressions(exps: List[Exp]): Unit = {
        for (exp <- exps) {
          writeExpression(exp)
        }
      } // writeExpressions
      @throws[CodeGeneratorException]
      private def writeExpression(exp: Exp): Unit = {
        exp match {
          case IntegerExp(value) => writeIntLiteral(value)
          case BooleanExp(value) => writeIntLiteral(if (value) 1 else 0)
          case VariableExp(value) => loadVariable(value)
          case PrintExp(e1) =>
          case methodExp: MethodExp => writeMethodCall(methodExp)
          case newExp: NewClassExp => writeNew(newExp)
//          case CastExp(newTypes, e2) => ???
          case GroupedExp(e) => writeExpression(e)
          case HighOrderExp(params, body) => ???
          case CallHighOrderExp(function, params) => ???
          case bop: BOP => bopHandler(bop)

          case _ => throw new CodeGeneratorException("Unrecognized expression: " + exp)
        }
      } // writeExpression

      @throws[CodeGeneratorException]
      private def writePrint(variable: String): Unit = {
        val entry = variables.getEntryFor(variable)
        val descriptor = printlnDescriptorString(entry.types)
        methodVisitor.visitFieldInsn(GETSTATIC, "java/lang/System", "out", new ClassTypes("java/io/PrintStream").toDescriptorString)
        loadVariable(variable)
        methodVisitor.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", descriptor, false)
      } // writePrint

      @throws[CodeGeneratorException]
      private def writeIfStatement(conditionalStmt: ConditionalStmt): Unit = {
        val falseLabel = new Label
        val afterFalseLabel = new Label
        writeExpression(conditionalStmt.condition)
        methodVisitor.visitJumpInsn(IFEQ, falseLabel)
        writeStatement(conditionalStmt.ifTrue)
        methodVisitor.visitJumpInsn(GOTO, afterFalseLabel)
        methodVisitor.visitLabel(falseLabel)
        writeStatement(conditionalStmt.ifFalse)
        methodVisitor.visitLabel(afterFalseLabel)
      } // writeIfStatement


      @throws[CodeGeneratorException]
      private def writeStatements(stmts: List[Stmt]): Unit = {
        for (statement <- stmts) {
          writeStatement(statement)
        }
      } // writeStatements

      @throws[CodeGeneratorException]
      private def writeStatement(stmt: Stmt): Unit = {
        stmt match {
          case ExpStmt(e1) =>
          case AssignmentStmt(varDec, exp) => variables.addEntry(varDec.varName, varDec.types)
            writeExpression(exp)
            storeVariable(varDec.varName)
          case ForStmt(assign, e1, inc, forBody) => // writeWhileStatement(stmt.asInstanceOf[Nothing])
          //          case BreakStmt => lets get rid of these
          case BlockStmt(statements) => for (statement: Statement <- statements) {
            writeStatement(statement)
          }
          case stmt: ConditionalStmt => writeIfStatement(stmt)
          case VoidStmt =>
          //          case PrintStmt(variable) => writePrint(variable)
          case VarStmt(variableName, newValue) => writeExpression(newValue)
            storeVariable(variableName)
        }

      } // writeStatement

      def bopHandler(bop: BOP): Unit = {
        writeExpression(bop.leftExp)
        writeExpression(bop.rightExp)
        writeOp(bop)
      }

      @throws[CodeGeneratorException]
      def writeMethod(): Unit = {
        methodVisitor.visitCode
        writeStatement(method.stmt)
        writeExpression(method.returnExpression)
        doReturn(method.types)
        methodVisitor.visitMaxs(0, 0)
      } // writeMethod
    } // singleMethodGenerator
  } // singleClassGenerator

}