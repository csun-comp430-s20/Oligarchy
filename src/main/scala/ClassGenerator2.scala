import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.sql.Statement

import org.objectweb.asm.Label
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

class CodeGeneratorException(val message: String) extends Exception(message)

object ClassGenerator2 {
  val thisVariable = "this"
  val objectName = "java/lang/Object"
  var allClasses: Map[String, Class] = _
  var lambdaMaker: LambdaMaker

  // should be similar to the constructor method in java here we will set up how we will generate code
  def apply(program: Program){
    program.classes.foreach(myClass => {
      if (this.allClasses contains (myClass.className)) {
        throw new CodeGeneratorException("redefining a defined class " + myClass.className)
      }
      this.allClasses += (myClass.className -> myClass)
    })
    this.lambdaMaker = lambdaMaker(this.allClasses)
  } //apply

  def writeClass(classWriter: ClassWriter, destination: File): Unit = {
    val output = new BufferedOutputStream((new FileOutputStream(destination)))
    output.write(classWriter.toByteArray)
    output.close()
  } // writeClass

  def writeInstanceVariables(classWriter: ClassWriter, variables:List[VarDeclaration]): Unit={
    for (feild <- variables){
      classWriter.visitField(ACC_PUBLIC, feild.varName, feild.types.toDescriptorString(),null, null ).visitEnd()
    }
  }// instance Variables

  @throws[CodeGeneratorException]
  def writeClasses(toDirectory:String): Unit = {
    for (classDef <- allClasses.values) {
      val genClass = SingleClassGenerator(classDef)
      genClass.writeClass(toDirectory)
    }
    lambdaMaker.writeLambdas(toDirectory)
  } // writeClasses

  @throws[CodeGeneratorException]
  def deleteClasses(inDirectory:String): Unit={
    for (classDef <- allClasses.values) {
      new File(inDirectory, classDef.className + ".class").delete()
    }
  }//deleteClasses

  case class SingleClassGenerator(classDef: Class) {

    private var forClass: Class = _
    private var thisType: ClassTypes = _
    private var classWriter: ClassWriter = _

    @throws[CodeGeneratorException]
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
    } // SingleClassGenerator

    @throws[CodeGeneratorException]
    def writeClass(toDiectory: String): Unit = {
      // we dont have constructors or main
      for (method <- forClass.methods) {
        val methodGen = SingleMethodGenerator(method)
        methodGen.writeMethod()
      }
      classWriter.visitEnd()
      ClassGenerator2.writeClass(classWriter, new File(toDiectory, forClass.className + ".class"))
    } // writeClass

    case class SingleMethodGenerator(m: MethodDef) {

      private var method: MethodDef = _
      private var variables: VariableTable = _
      private var methodVisitor: MethodVisitor = _

      def apply(methodDef: MethodDef): SingleMethodGenerator = {
        this.method = methodDef
        val flags = ACC_PUBLIC
        variables = VariableTable.withFormalParamsFrom(thisType, method)
        methodVisitor = classWriter.visitMethod(flags, method.methodName, method.toDescriptorString(), null, null)
        this
      }


      @throws[CodeGeneratorException]
      def writeMethod(): Unit = {
        methodVisitor.visitCode()
        val gen = ExpressionStatementGenerator(allClasses, lambdaMaker, variables, methodVisitor)
        gen.writeStatements(method.stmt)
        if(method.isInstanceOf[MethodDef]){
          gen.writeExpression(method.returnExpression)
        }
        gen.doReturn(method.types)
        methodVisitor.visitMaxs(0,0)
      } // writeMethod

    } // singleMethodGenerator
  } // singleClassGenerator
}