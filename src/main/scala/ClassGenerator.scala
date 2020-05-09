import java.sql.Statement

import org.objectweb.asm.Label
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import  org.objectweb.asm.Opcodes._
class CodeGeneratorException(val message: String) extends Exception(message)

class ClassGenerator(program: Program) {
  val thisVariable = "this"
  val objectName = "java/lang/Object"
  var allClasses: Map[String, Class] = _
  //  lambdaMaker: LambdaMaker

  // should be similar to the constructor method in java here we will set up how we will generate code
  def apply(program: Program): CodeGenerator = {
    program.classes.foreach(myClass=> {
      if (this.allClasses contains( myClass.className)){

      }
    })
  } //apply

  private def getEntryFor(variable: String): VariableEntry = {
  } // getEntryFor

  private def addEntry(variable: String, types: Types): VariableEntry = if (variables contains variable) { // should be caught by typechecker
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