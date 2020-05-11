import java.util.concurrent.Callable

import com.sun.jdi.ClassType
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes.ACC_BRIDGE
import org.objectweb.asm.Opcodes.ACC_PUBLIC
import org.objectweb.asm.Opcodes.ACC_SYNTHETIC
import org.objectweb.asm.Opcodes.ALOAD
import org.objectweb.asm.Opcodes.CHECKCAST
import org.objectweb.asm.Opcodes.INVOKEVIRTUAL
import org.objectweb.asm.Opcodes.ARETURN
import org.objectweb.asm.Opcodes.RETURN
import org.objectweb.asm.Opcodes.PUTFIELD


class LambdaDef(className: String, varDeclaration: List[VarDeclaration], param: String, paramType: Types, returnType: Types, body: Exp) {

  def toSignatureString(): Unit = {
    (VarDeclaration(ClassTypes(ClassGenerator.objectName)).toSignatureString() + LambdaType(paramType, returnType).toSignatureString())
  }

  def constructorDescriptorString(): Unit = {
    (varDeclaration.types.toDescriptorString())
  }

  def toSignatureString(fieldName: String): Unit = {
    varDeclaration.foreach {
      varDeclaration => {
        if (varDeclaration.varName == fieldName) {}
        (varDeclaration.types.toDescriptorString())
      }
    }
  }

  def bridgeApplyDescriptorString(): Unit = {
    val objectType = VarDeclaration(ClassTypes(ClassGereator.objectName))

  }

  def writeConstructor(classWriter: ClassWriter): Unit = {
    try {
      val methodVisitor = classWriter.visitMethod(ACC_PUBLIC, "<init>", varDeclaration.className, null, null)
      methodVisitor.visitCode()
      methodVisitor.visitVarInsn(ALOAD, 0)
      methodVisitor.visitMethodInsn(INVOKEVIRTUAL, ClassGenerator.objectName, "<init>", "()V", false)
      val variableIndex = 1
      varDeclaration.foreach {
        varDeclaration: VarDeclaration => {
          methodVisitor.visitVarInsn(ALOAD, 0)
          methodVisitor.visitVarInsn(VariableEntry.loadInstructionForType(varDeclaration.types.toString))
          , variableIndex += 1
          )
          methodVisitor.visitFieldInsn(PUTFIELD, className, varDeclaration.varName, varDeclaration.types.toString)
        }
      }
      methodVisitor.visitInsn(RETURN)
      methodVisitor.visitMaxs(0, 0)

    }
    catch {
      case _ => throw CodeGeneratorException("Failed to write constructor")
    }
  }

  def writeTypedApply(classWriter: ClassWriter, allClasses: Map[String, Class], lambdaMaker: LambdaMaker) = {
    try {
      val methodVisitor = classWriter.visitMethod(ACC_PUBLIC, LambdaMaker.APPLY_NAME.name, typedApplyDescriptorString())
      methodVisitor.visitCode()
      val gen = ExpressionStatementGenerator(allClasses, lambdaMaker, VariableTable.withFormalParams(ClassTypes(className), varDeclaration), methodVisitor)
      gen.writeExpression(body)
      gen.doReturn(returnType)
      methodVisitor.visitMaxs(0, 0)
    }
    catch {
      case _ => throw CodeGeneratorException("Failed to write typed apply")
    }
  }

  def writeBridgeApply(classWriter: ClassWriter, allClasses: Map[String, Class]) = {
    try {
      val methodVisitor = classWriter.visitMethod(ACC_PUBLIC | ACC_SYNTHETIC | ACC_BRIDGE, LambdaMaker.APPLY_NAME.name, bridgeApplyDescriptorString(), null, null)
      methodVisitor.visitCode()
      methodVisitor.visitVarInsn(ALOAD, 0)
      methodVisitor.visitVarInsn(ALOAD, 1)
      methodVisitor.visitTypeInsn(CHECKCAST, className)
      methodVisitor.visitMethodInsn(INVOKEVIRTUAL, className, LambdaMaker.APPLY_NAME.name, typedApplyDescriptorString(), false)
      methodVisitor.visitInsn(ARETURN)
      methodVisitor.visitMaxs(0, 0)
    }
    catch {
      case _ => throw CodeGeneratorException("Failed to write bridge apply")
    }
  }

}
