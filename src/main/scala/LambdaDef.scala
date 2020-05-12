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

case object LambdaDef {
  def bridgeApplyDescriptorString(): String = {
    var objectFormalParams: List[VarDeclaration] = List()
    val objectType = ClassTypes(ClassGenerator.objectName)
    objectFormalParams = objectFormalParams.:+(VarDeclaration(objectType, ""))
    MethodDef.toDescriptorString(objectFormalParams, objectType)
  }
}

case class LambdaDef(className: String, varDeclarations: List[VarDeclaration], param: String, paramType: Types, returnType: Types, body: Exp) {

  var formalParams: List[VarDeclaration] = List()

  def apply(className: String, varDeclarations: List[VarDeclaration], param: String, paramType: Types, returnType: Types, body: Exp): LambdaDef = new LambdaDef(className, varDeclarations, param, paramType, returnType, body){
    formalParams = formalParams :+ (VarDeclaration(paramType,param))
    this
  }
  def toSignatureString: String = {
    ClassTypes(className).toDescriptorString() + HighOrderFuncType(returnType, returnType).toDescriptorString()
  }

  def constructorDescriptorString(): String = {
    MethodDef.toDescriptorString(varDeclarations, VoidTypes)
  }

  @throws[CodeGeneratorException]
  def fieldDescriptorString(fieldName: String): String = {
    for (varDec <- varDeclarations) {
      if (varDec.varName.equals(fieldName)) return varDec.types.toDescriptorString
    }
    throw new CodeGeneratorException("Unknown field: " + fieldName)

  } // fieldDescriptorString

  def typedApplyDescriptorString: String = {
    MethodDef.toDescriptorString(formalParams, returnType)
  }// typedApplyDescriptorString

  def writeConstructor(classWriter: ClassWriter): Unit = {
    try {
      val methodVisitor = classWriter.visitMethod(ACC_PUBLIC, "<init>", constructorDescriptorString(), null, null)
      methodVisitor.visitCode()
      methodVisitor.visitVarInsn(ALOAD, 0)
      methodVisitor.visitMethodInsn(INVOKEVIRTUAL, ClassGenerator.objectName, "<init>", "()V", false)
      var variableIndex = 1
      varDeclarations.foreach {
        varDeclarations: VarDeclaration => {
          methodVisitor.visitVarInsn(ALOAD, 0)
          methodVisitor.visitVarInsn(VariableEntry.loadInstructionForType(varDeclarations.types), variableIndex = variableIndex + 1)
          methodVisitor.visitFieldInsn(PUTFIELD, className, varDeclarations.varName, varDeclarations.types.toString)
        }
      }
      methodVisitor.visitInsn(RETURN)
      methodVisitor.visitMaxs(0, 0)
    }
    catch {
      case _: Throwable => throw new CodeGeneratorException("Failed to write constructor")
    }
  }

  def writeTypedApply(classWriter: ClassWriter, allClasses: Map[String, Class], lambdaMaker: LambdaMaker): Unit = {
    try {
      val methodVisitor = classWriter.visitMethod(ACC_PUBLIC, LambdaMaker.APPLY_NAME, typedApplyDescriptorString, null, null)
      methodVisitor.visitCode()
      val gen = ExpressionStatementGenerator(allClasses, lambdaMaker, VariableTable.withFormalParams(ClassTypes(className), varDeclarations), methodVisitor)
      gen.writeExpression(body)
      gen.doReturn(returnType)
      methodVisitor.visitMaxs(0, 0)
    }
    catch {
      case _ => throw new CodeGeneratorException("Failed to write typed apply")
    }
  }

  def writeBridgeApply(classWriter: ClassWriter, allClasses: Map[String, Class]) = {
    try {
      val methodVisitor = classWriter.visitMethod(ACC_PUBLIC | ACC_SYNTHETIC | ACC_BRIDGE, LambdaMaker.APPLY_NAME, LambdaDef.bridgeApplyDescriptorString(), null, null)
      methodVisitor.visitCode()
      methodVisitor.visitVarInsn(ALOAD, 0)
      methodVisitor.visitVarInsn(ALOAD, 1)
      methodVisitor.visitTypeInsn(CHECKCAST, className)
      methodVisitor.visitMethodInsn(INVOKEVIRTUAL, className, LambdaMaker.APPLY_NAME, typedApplyDescriptorString, false)
      methodVisitor.visitInsn(ARETURN)
      methodVisitor.visitMaxs(0, 0)
    }
    catch {
      case _ => throw new CodeGeneratorException("Failed to write bridge apply")
    }
  }

}
