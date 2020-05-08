import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes.ILOAD
import org.objectweb.asm.Opcodes.ISTORE

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