import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

case object VariableEntry {
  @throws[CodeGeneratorException]
  def loadInstructionForType(types: Types): Int = { // both are treated as integers at the bytecode level
    types match {
      case IntTypes | BoolTypes => ILOAD
      case ClassTypes(className) =>  ALOAD
      case MethodTypes(paramTypes, returnTypes) => ALOAD
      case _ => throw new CodeGeneratorException("Unknown load type: " + types)
    }
  } // loadInstructionForType


  @throws[CodeGeneratorException]
  def storeInstructionForType(types: Types) = {
    types match {
      case IntTypes | BoolTypes => ISTORE
      case ClassTypes(className) => ASTORE
      case MethodTypes(paramTypes, returnTypes) => ASTORE
      case _ => throw new CodeGeneratorException("Unknown storetype: " + types)
    }
  } // storeInstructionForType
}

class VariableEntry(val variable: String, val types: Types, val index: Int) {
  assert(index >= 0)

  def load(visitor: MethodVisitor): Unit = { // both are treated as integers at the bytecode level
    types match {
      case IntTypes | BoolTypes =>
        visitor.visitVarInsn(ILOAD, index)
    }
  }

  def store(visitor: MethodVisitor): Unit = {
    types match {
      case IntTypes | BoolTypes =>
        visitor.visitVarInsn(ISTORE, index)
    }
  }

  // VariableEntr
}