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
    var inner = _
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
  private def methodDescriptorFor(className:String, methodName: String): String = {
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
  }// fieldDescriptorFor


  case class SingleClassGenerator(classDef: Class) {

    private var forClass:DefExtClass =_
    private var referenceType:ClassTypes = _
    private var classWriter:ClassWriter = _

    private def getEntryFor(variable: String) = {

    } // getEntryFor

    private def addEntry(variable: String, types: Types) = if (variables contains variable) { // should be caught by typechecker
    } // addEntry

    private def writeIntLiteral(value: Int): Unit = {
    } // writeIntLiteral
    private def writeOp(binaryOp: Nothing): Unit = {
    } // writeOp
    private def writeExpression(exp: Nothing): Unit = {
    } // writeExpression
    private def writePrint(variable: String): Unit = {
    } //writePrint
    private def writeStatement(statement: Nothing): Unit = {
    } //writeStatement
    private def writeVarDeclaration(varDec: Nothing): Unit = {
    } //writeVarDeclaration
    private def writeInstance(instance: Nothing): Unit = {
    } //writeInstance
    private def writeMethod(method: Nothing): Unit = {
    } //writeMethod
    private def writeClass(myClass: Nothing): Unit = {
    } //writeClass
    private def writeProgram(program: Nothing): Unit = {
    } //writeProgram
  }

}