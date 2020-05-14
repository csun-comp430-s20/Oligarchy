import java.io.File

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.ClassWriter

case object LambdaMaker {
  val LAMBDA_PREFIX = "Lambda"
  val EXTENDS_NAME = "Function1"
  val APPLY_NAME = "apply"
  val REWRITTEN_THIS: String = "~" + ClassGenerator.thisVariable

  def setUnion[A](first: Set[A], second: Set[A]): Set[A] = {
    first.++(second)
  }

  def addSet[A](set: Set[A], element: A): Set[A] = {
    set.+(element)
  }

  def freeVariables(params: Set[String], exps: List[Exp]): Set[String] = {
    exps.foldLeft(Set(): Set[String])((res, cur) => {
      res ++ freeVariables(params, cur)
    })
  }


  @throws[CodeGeneratorException]
  def freeVariables(lambdaExp: HighOrderExp): Set[String] = {
    freeVariables(addSet(Set(), lambdaExp.param), lambdaExp.body)
  }// freeVariables

  def freeVariables(params: Set[String], exp: Exp):Set[String] = exp match {
    case IntegerExp(_) | BooleanExp(_) => Set()
    case VariableExp(value) => if (!params.contains(value)) Set().+(value) else Set()
    //      case PrintExp(e1) =>
    case NewClassExp(className, e1) => freeVariables(params, e1)
    case MethodExp(e1, className, methodName, e2) => freeVariables(params, e1) ++ freeVariables(params, e2)
    case GroupedExp(e) => freeVariables(params, e)
    case HighOrderExp(param, paramType,returnType, body) => freeVariables(addSet(params, param), body)
    case CallHighOrderExp(lambda, returnType, param) =>  freeVariables(params, lambda)++ freeVariables(params, param)
    case bop: BOP => freeVariables(params, bop.leftExp) ++ freeVariables(params, bop.rightExp)
  }


}


case class LambdaMaker(var allClasses: Map[String, Class], var additionalClasses: List[LambdaDef] = List()) {

  private var curLambda = 0

  @throws[CodeGeneratorException]
  def classDefinitionFor(name: String): LambdaDef = {
    for (lambdaDef <- additionalClasses) {
      if (lambdaDef.className.equals(name)) return lambdaDef
    }
    throw new CodeGeneratorException("No such class: " + name)
  } // classDefinitionFor

  @throws[CodeGeneratorException]
  def constructorDescriptorFor(name: String): String = {
    classDefinitionFor(name).constructorDescriptorString()
  } // constructorDescriptorFor

  @throws[CodeGeneratorException]
  def fieldDescriptorFor(className: String, fieldName: String): String = {
    classDefinitionFor(className).fieldDescriptorString(fieldName)
  } // fieldDescriptorFor

  @throws[CodeGeneratorException]
  private def translateLambdaBodies(bodies: List[Exp], lambdaParam: String, lambdaParamType: ClassTypes, lambdaClass: String): List[Exp] = {
    bodies.foldLeft(List(): List[Exp])((res, cur) => {
      res :+ translateLambdaBody(cur, lambdaParam, lambdaParamType, lambdaClass)
    }
    )
  } // translateLambdaBodies

  @throws[CodeGeneratorException]
  private def translateLambdaBody(body: Exp, lambdaParam: String, lambdaParamType: ClassTypes, lambdaClass: String): Exp = {
    body match {
      case body: VariableExp =>body
//      case body: VariableExp =>if (body.value.equals(lambdaParam)) return body
        // we dont have getters/setters
//      else{
//        val capturedVar = if (body.value.equals(ClassGenerator.thisVariable))  LambdaMaker.REWRITTEN_THIS else body
//        return new GetExp(VariableExp(ClassGenerator.thisVariable), lambdaClass, capturedVar)
//      }
      case _: IntegerExp | _: BooleanExp => return body
      case asMethodCall: MethodExp =>MethodExp(translateLambdaBody(asMethodCall.callVariable, lambdaParam, lambdaParamType, lambdaClass),
        asMethodCall.className,
        asMethodCall.methodName,
        translateLambdaBodies(asMethodCall.params, lambdaParam, lambdaParamType, lambdaClass)
      )
      case asNew: NewClassExp => NewClassExp(asNew.className, translateLambdaBodies(asNew.params, lambdaParam, lambdaParamType, lambdaClass))
//      case body: CastExp =>
//      case body: GetExp => GetExp(translateLambdaBody(asGet.target, lambdaParam, lambdaParamType, lambdaClass), asGet.name, asGet.field)
      case body: GroupedExp =>GroupedExp(translateLambdaBody(body,lambdaParam,lambdaParamType,lambdaClass))
      case body: HighOrderExp => translateLambda(body,
        VariableTable.withFormalParam(ClassTypes(lambdaClass),
          lambdaParamType,
          lambdaParam));
      case asCall: CallHighOrderExp => CallHighOrderExp(translateLambdaBody(asCall.lambda, lambdaParam, lambdaParamType, lambdaClass), asCall.returnType, translateLambdaBody(asCall.param, lambdaParam, lambdaParamType, lambdaClass))
      case body: PlusExp =>   PlusExp(
        translateLambdaBody(body.leftExp, lambdaParam, lambdaParamType, lambdaClass),
        translateLambdaBody(body.rightExp, lambdaParam, lambdaParamType, lambdaClass)
      )case body: SubtractExp =>   SubtractExp(
        translateLambdaBody(body.leftExp, lambdaParam, lambdaParamType, lambdaClass),
        translateLambdaBody(body.rightExp, lambdaParam, lambdaParamType, lambdaClass)
      )case body: MultiplyExp =>   MultiplyExp(
        translateLambdaBody(body.leftExp, lambdaParam, lambdaParamType, lambdaClass),
        translateLambdaBody(body.rightExp, lambdaParam, lambdaParamType, lambdaClass)
      )case body: DivideExp =>   DivideExp(
        translateLambdaBody(body.leftExp, lambdaParam, lambdaParamType, lambdaClass),
        translateLambdaBody(body.rightExp, lambdaParam, lambdaParamType, lambdaClass)
      )case body: AndExp =>   AndExp(
        translateLambdaBody(body.leftExp, lambdaParam, lambdaParamType, lambdaClass),
        translateLambdaBody(body.rightExp, lambdaParam, lambdaParamType, lambdaClass)
      )case body: OrExp =>   OrExp(
        translateLambdaBody(body.leftExp, lambdaParam, lambdaParamType, lambdaClass),
        translateLambdaBody(body.rightExp, lambdaParam, lambdaParamType, lambdaClass)
      )case body: LTEExp =>   LTEExp(
        translateLambdaBody(body.leftExp, lambdaParam, lambdaParamType, lambdaClass),
        translateLambdaBody(body.rightExp, lambdaParam, lambdaParamType, lambdaClass)
      )case body: LTExp =>   LTExp(
        translateLambdaBody(body.leftExp, lambdaParam, lambdaParamType, lambdaClass),
        translateLambdaBody(body.rightExp, lambdaParam, lambdaParamType, lambdaClass)
      )case body: GTEExp =>  GTEExp(
        translateLambdaBody(body.leftExp, lambdaParam, lambdaParamType, lambdaClass),
        translateLambdaBody(body.rightExp, lambdaParam, lambdaParamType, lambdaClass)
      )case body: GTExp =>   GTExp(
        translateLambdaBody(body.leftExp, lambdaParam, lambdaParamType, lambdaClass),
        translateLambdaBody(body.rightExp, lambdaParam, lambdaParamType, lambdaClass)
      )
      case body: EqualsExp => EqualsExp(
        translateLambdaBody(body.leftExp, lambdaParam, lambdaParamType, lambdaClass),
        translateLambdaBody(body.rightExp, lambdaParam, lambdaParamType, lambdaClass)
      )
      case _ => throw new CodeGeneratorException("Unrecognized expression: " + body)
    }
  } // translateLambdaBody

  @throws[CodeGeneratorException]
  private def writeLambda(lambdaDef: LambdaDef, toDirectory: String): Unit = {
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
    classWriter.visit(V1_7, ACC_PUBLIC,
      lambdaDef.className, lambdaDef.toSignatureString,
      ClassGenerator.objectName,
      Array[String](LambdaMaker.EXTENDS_NAME)
    )

    ClassGenerator.writeInstanceVariables(classWriter, lambdaDef.varDeclarations)
    lambdaDef.writeConstructor(classWriter)
    lambdaDef.writeTypedApply(classWriter, allClasses, this)
    lambdaDef.writeBridgeApply(classWriter, allClasses)
    classWriter.visitEnd()
    ClassGenerator.writeClass(classWriter, new File(toDirectory, lambdaDef.className + ".class"))
  } // writeLambda


  @throws[CodeGeneratorException]
  def translateLambda(lambdaExp: HighOrderExp, table: VariableTable): NewClassExp = {
    val outputClassName = (LambdaMaker.LAMBDA_PREFIX + {curLambda += 1;})
    val needToCapture = LambdaMaker.freeVariables(lambdaExp)
    val instanceVariables = needToCapture.foldLeft(List())((res,cur)=>{
      val varType = table.getEntryFor(cur).types
      val varName = if (cur.equals(ClassGenerator.thisVariable)) LambdaMaker.REWRITTEN_THIS
      else cur
      res :+ VarDeclaration(varType, varName)
    })

    val lambdaDef = new LambdaDef(outputClassName,
      instanceVariables,
      lambdaExp.param,
      lambdaExp.paramType, lambdaExp.returnType,
      translateLambdaBody(lambdaExp.body, lambdaExp.param, lambdaExp.paramType, outputClassName))
    additionalClasses = additionalClasses.:+(lambdaDef)
    var newParams:List[Exp] = List()
    for (instanceVariable <- instanceVariables) {
      val toPass = if (instanceVariable.equals(LambdaMaker.REWRITTEN_THIS)) ClassGenerator.thisVariable
      else instanceVariable
      newParams = newParams :+ VariableExp(toPass)
    }
    NewClassExp(outputClassName, newParams)
  } // translateLambda

  @throws[CodeGeneratorException]
  def writeLambdas(toDirectory: String): Unit = {
    for (lambdaDef <- additionalClasses) {
      writeLambda(lambdaDef, toDirectory)
    }
  } // writeLambdas

  // intended for testing.  Deletes all created classes in the given directory
  def deleteClasses(inDirectory: String): Unit = {
    for (lambdaDef <- additionalClasses) {
      new File(inDirectory, lambdaDef.className + ".class").delete
    }
  } // deleteClasses


}
