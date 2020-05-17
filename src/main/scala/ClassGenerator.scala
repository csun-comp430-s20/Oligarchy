import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.sql.Statement


import org.objectweb.asm.Label
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.MethodVisitor
import org.objectweb.asm.Opcodes._

class CodeGeneratorException(val message: String) extends Exception(message)

case object ClassGenerator {
  val thisVariable = "this"
  val objectName = "java/lang/Object"

  def writeClass(classWriter: ClassWriter, destination: File): Unit = {
    val output = new BufferedOutputStream((new FileOutputStream(destination)))
    output.write(classWriter.toByteArray)
    output.close()
  } // writeClass

  def writeInstanceVariables(classWriter: ClassWriter, variables: List[VarDeclaration]): Unit = {
    for (field <- variables) {
      classWriter.visitField(ACC_PUBLIC, field.varName, field.types.toDescriptorString, null, null).visitEnd()
    }
  } // instance Variables
}
case class ClassGenerator(program: Program){
  private var allClasses:Map[String,Class] = Map()
  private var lambdaMaker:LambdaMaker = _
  program.classes.foreach(myClass => {
    if (this.allClasses contains (myClass.className)) {
      throw new CodeGeneratorException("redefining a defined class " + myClass.className)
    }
    else {
      this.allClasses += (myClass.className -> myClass)
    }
  })
  this.lambdaMaker = LambdaMaker(this.allClasses)
  this
//  @throws[CodeGeneratorException]
//  def apply(program: Program): ClassGenerator ={
//    program.classes.foreach(myClass => {
//      if (this.allClasses contains (myClass.className)) {
//        throw new CodeGeneratorException("redefining a defined class " + myClass.className)
//      }
//      else {
//        this.allClasses += (myClass.className -> myClass)
//      }
//    })
//    this.lambdaMaker = LambdaMaker(this.allClasses)
//    this
//  } //apply

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


  case class SingleClassGenerator(forClass: Class) {

    // ---BEGIN INSTANCE VARIABLES---
    private var thisType:ClassTypes = _
    private var classWriter:ClassWriter = _
    // ---END INSTANCE VARIABLES---

    this.thisType = ClassTypes(forClass.className)
    this.classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
    classWriter.visit(V1_7, // Java 1.7
      ACC_PUBLIC, // public
      forClass.className, // class name
      null, // signature (null means not generic)
      if(forClass.extendedClass == null){
        ClassGenerator.objectName
      }
      else{
        forClass.extendedClass
      },
    new Array[String](0)) // interfaces (none)
    for (field <- forClass.instances) {
      classWriter.visitField(ACC_PUBLIC, field.v1.varName, field.v1.types.toDescriptorString, null, null).visitEnd()
    }

//    def SingleClassGenerator(forClass: Class) {
//      this.thisType = ClassTypes(forClass.className)
//      this.classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
//      classWriter.visit(V1_7, // Java 1.7
//        ACC_PUBLIC, // public
//        forClass.className, // class name
//        null, // signature (null means not generic)
//        forClass.extendedClass, // superclass if null is passed it checks for it and corrects it
//        new Array[String](0)) // interfaces (none)
//      for (field <- forClass.instances) {
//        classWriter.visitField(ACC_PUBLIC, field.v1.varName, field.v1.types.toDescriptorString, null, null).visitEnd()
//      }
//    } // SingleClassGenerator


    @throws[CodeGeneratorException]
    def writeClass(toDiectory: String): Unit = {
      // we dont have constructors or main
      //      val methodGen: SingleMethodGenerator = new SingleMethodGenerator(MethodDef(VoidTypes,"<init>",forClass.statements,forClass.parameters,IntegerExp(1)))
      //      methodGen.writeMethod()
      var owner = ClassGenerator.objectName
      if (forClass.extendedClass != null){
         owner =  forClass.extendedClass
      }
      val constructor: MethodVisitor = classWriter.visitMethod(ACC_PUBLIC, // access modifier
        "<init>", // method name (constructor)
        "()V", // descriptor
        null, // signature (null means not generic)
        null) // exceptions
      constructor.visitCode
      constructor.visitVarInsn(ALOAD, 0) // load "this"

      constructor.visitMethodInsn(INVOKESPECIAL, owner, "<init>", "()V", false) // super()

      constructor.visitInsn(RETURN)
      constructor.visitMaxs(0, 0)
//      classWriter.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
      for (method <- forClass.methods) {
        val methodGen = SingleMethodGenerator(method)
        methodGen.writeMethod()
      }
      classWriter.visitEnd()
      ClassGenerator.writeClass(classWriter, new File(toDiectory, forClass.className + ".class"))
    } // writeClass

    case class SingleMethodGenerator(method: MethodDef) {
      private var variables: VariableTable = _
      private var methodVisitor: MethodVisitor = _
      val flags = ACC_PUBLIC
      variables = VariableTable.withFormalParamsFrom(thisType, method)
      methodVisitor = classWriter.visitMethod(flags, method.methodName, method.toDescriptorString, null, null)
      this
//      def apply(method:MethodDef): SingleMethodGenerator = {
//        val flags = ACC_PUBLIC
//        variables = VariableTable.withFormalParamsFrom(thisType, method)
//        methodVisitor = classWriter.visitMethod(flags, method.methodName, method.toDescriptorString, null, null)
//        this
//      }


      @throws[CodeGeneratorException]
      def writeMethod(): Unit = {
        methodVisitor.visitCode()
        val gen = ExpressionStatementGenerator(allClasses, lambdaMaker, variables, methodVisitor)
        gen.writeStatements(method.stmt)
        method match{
          case method:MethodDef => {
            gen.writeExpression(method.returnExpression)
          }
        }
        gen.doReturn(method.types)
        methodVisitor.visitMaxs(0,0)
      } // writeMethod
    } // singleMethodGenerator
  } // singleClassGenerator
}