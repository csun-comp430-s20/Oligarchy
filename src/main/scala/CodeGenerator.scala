import java.sql.Statement

import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.ClassWriter
case class CodeGeneratorException(val message: String) extends Exception(message)
class CodeGenerator(
                     val outputClassName: String,
                     val outputMethodName: String,
                     classWriter: ClassWriter,
                     variables: Map[String,VariableEntry],
                     nextIndex: Int,
                     methodVisitor: MethodVisitor
                   ) {


  // should be similar to the constructor method in java here we will set up how we will generate code
  def apply(outputClassName: String, outputMethodName: String): CodeGenerator = {
  }//apply

  private def getEntryFor(variable: String): VariableEntry = {

  } // getEntryFor

  private def addEntry(variable: String, types: Types) ={ // should be caught by typechecker
    if(variables contains variable){
      throw CodeGeneratorException("Variable has already been created")
    }
    else {
      (variables + (variable -> types))
    }
  }// addEntry

  private def writeIntLiteral(value: Int): Unit = {
  }// writeIntLiteral
  private def writeOp(binaryOp: Nothing): Unit ={
  }// writeOp
  private def writeExpression(exp: Exp): Unit={
  }// writeExpression
  private def writePrint(variable:String): Unit={
  }//writePrint
  private def writeStatement(stmt:Stmt):Unit={
    stmt match {
      case ExpStmt(e1) =>
      case AssignmentStmt(varDec, exp) =>{
        variables + (varDec.varName -> varDec.types)
        writeExpression(exp)
        getEntryFor(varDec.varName)
      }
      case ForStmt(assign, e1, inc, forBody) =>
      case BreakStmt =>
      case BlockStmt(statements) =>
      case ConditionalStmt(condition, ifTrue, ifFalse) =>
      case ReturnStmt(returnExp) =>
      case VoidStmt =>
      case VarStmt(variableName, newValue) =>{
        writeExpression(newValue)
        getEntryFor(variableName)
      }
      case _=> throw CodeGeneratorException("Not a statement")
    }
  }//writeStatement
  private def writeVarDeclaration(varDec:  Nothing):Unit={
  }//writeVarDeclaration
  private def writeInstance(instance:  Nothing):Unit={
  }//writeInstance
  private def writeMethod(method:  Nothing):Unit={
  }//writeMethod
  private def writeClass(myClass:  Nothing):Unit={
  }//writeClass
  private def writeProgram(program: Nothing):Unit={
  }//writeProgram
}