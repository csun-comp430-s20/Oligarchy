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
  def freeVariables(highOrderExp: HighOrderExp): Set[String] = {
      freeVariables(addSet(Set(), highOrderExp), highOrderExp.body)
  } // freeVariables

  def freeVariables(params: Set[String], exp: Exp): Set[String] = {
    exp match {
      case IntegerExp(_) | BooleanExp(_) => Set()
      case VariableExp(value) => if (!params.contains(value)) {
        Set().+(value)
      } else {
        Set()
      }
      //      case PrintExp(e1) =>
      case NewClassExp(className, e1) => freeVariables(params, e1)
      case MethodExp(e1, className, methodName, e2) => freeVariables(params, e1) ++ freeVariables(params, e2)
      case CastExp(newTypes, e2) => freeVariables(params, e2)
      case GroupedExp(e) => freeVariables(params, e)
      case HighOrderExp(param, body) => ???
      case CallHighOrderExp(function, params) => ???
      case bop:BOP => freeVariables(params, bop.leftExp) ++ freeVariables(params, bop.rightExp)
    }
  }


}


case class LambdaMaker(allClasses: Map[String, Class] ,  additionalClasses: List[LambdaDef] = List()) {

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
  private def translateLambdaBodies(bodies: List[Exp], lambdaParam: String, lambdaParamType: ClassTypes, lambdaClass: String) :List[Exp]= {
    var poop: List[Exp] = List()
    for (exp <- bodies) {
      val test = translateLambdaBody(exp, lambdaParam, lambdaParamType, lambdaClass)
      poop.:+(test)
    }
    poop
  } // translateLambdaBodies

  @throws[CodeGeneratorException]
  private def translateLambdaBody(body: Exp, lambdaParam: String, lambdaParamType:ClassTypes, lambdaClass: String):Exp = {
    if (body.isInstanceOf[VariableExp]) {
      val theVar = body.asInstanceOf[VariableExp].variable
      if (theVar.equals(lambdaParam)) return body
      else { // captured in the lambda class
        val capturedVar = if (theVar.equals(ClassGenerator.thisVariable)) REWRITTEN_THIS
        else theVar
        return new Nothing(new VariableExp(ClassGenerator.thisVariable), lambdaClass, capturedVar)
      }
    }
    else if (body.isInstanceOf[Nothing] || body.isInstanceOf[Nothing]) return body
    else if (body.isInstanceOf[Nothing]) {
      val asBinop = body.asInstanceOf[Nothing]
      return new Nothing(translateLambdaBody(asBinop.left, lambdaParam, lambdaParamType, lambdaClass), asBinop.bop, translateLambdaBody(asBinop.right, lambdaParam, lambdaParamType, lambdaClass))
    }
    else if (body.isInstanceOf[Nothing]) {
      val asMethodCall = body.asInstanceOf[Nothing]
      return new Nothing(translateLambdaBody(asMethodCall.callOn, lambdaParam, lambdaParamType, lambdaClass), asMethodCall.callOnName, asMethodCall.name, translateLambdaBodies(asMethodCall.actualParams, lambdaParam, lambdaParamType, lambdaClass))
    }
    else if (body.isInstanceOf[Nothing]) {
      val asNew = body.asInstanceOf[Nothing]
      return new Nothing(asNew.name, translateLambdaBodies(asNew.actualParams, lambdaParam, lambdaParamType, lambdaClass))
    }
    else if (body.isInstanceOf[Nothing]) {
      val asGet = body.asInstanceOf[Nothing]
      return new Nothing(translateLambdaBody(asGet.target, lambdaParam, lambdaParamType, lambdaClass), asGet.name, asGet.field)
    }
    else if (body.isInstanceOf[Nothing]) { // TODO: this needs a table of variables in scope.
      // I *think* this should just be `this`, but that means that
      // we need to properly handle naming `this`
      return translateLambda(body.asInstanceOf[Nothing], VariableTable.withFormalParam(new Nothing(lambdaClass), lambdaParamType, lambdaParam))
    }
    else if (body.isInstanceOf[Nothing]) {
      val asCall = body.asInstanceOf[Nothing]
      return new Nothing(translateLambdaBody(asCall.lambda, lambdaParam, lambdaParamType, lambdaClass), asCall.returnType, translateLambdaBody(asCall.param, lambdaParam, lambdaParamType, lambdaClass))
    }
    else {
      assert(false)
      throw new CodeGeneratorException("Unrecognized expression: " + body)
    }
  } // translateLambdaBody

  @throws[CodeGeneratorException]
  @throws[IOException]
  private def writeLambda(lambdaDef: LambdaDef, toDirectory: String): Unit

  = {
    val classWriter = new Nothing(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
    classWriter.visit(V1_7, ACC_PUBLIC, lambdaDef.className.name, lambdaDef.toSignatureString(), ClassGenerator.objectName, Array[String](EXTENDS_NAME.name))
    ClassGenerator.writeInstanceVariables(classWriter, lambdaDef.instanceVariables)
    lambdaDef.writeConstructor(classWriter)
    lambdaDef.writeTypedApply(classWriter, allClasses, this)
    lambdaDef.writeBridgeApply(classWriter, allClasses)
    classWriter.visitEnd
    ClassGenerator.writeClass(classWriter, new Nothing(toDirectory, lambdaDef.className.name + ".class"))
  } // writeLambda


  @throws[CodeGeneratorException]
  def translateLambda(lambdaExp: Nothing, table: VariableTable): Nothing = {
    val outputClassName = new Nothing(LAMBDA_PREFIX + {
      curLambda += 1;
      curLambda - 1
    })
    val needToCapture = LambdaMaker.freeVariables(lambdaExp)
    val instanceVariables = new util.ArrayList[Nothing]
    import scala.collection.JavaConversions._
    for (x <- needToCapture) {
      val varType = table.getEntryFor(x).`type`
      val varName = if (x.equals(ClassGenerator.thisVariable)) REWRITTEN_THIS
      else x
      instanceVariables.add(new Nothing(varType, varName))
    }
    val lambdaDef = new LambdaDef(outputClassName, instanceVariables, lambdaExp.param, lambdaExp.paramType, lambdaExp.returnType, translateLambdaBody(lambdaExp.body, lambdaExp.param, lambdaExp.paramType, outputClassName))
    additionalClasses.add(lambdaDef)
    val newParams = new util.ArrayList[Exp]
    import scala.collection.JavaConversions._
    for (instanceVariable <- instanceVariables) {
      val toPass = if (instanceVariable.variable.equals(REWRITTEN_THIS)) ClassGenerator.thisVariable
      else instanceVariable.variable
      newParams.add(new VariableExp(toPass))
    }
    return new Nothing(outputClassName, newParams)
  } // translateLambda

  @throws[CodeGeneratorException]
  @throws[IOException]
  def writeLambdas(toDirectory: String): Unit = {
    import scala.collection.JavaConversions._
    for (lambdaDef <- additionalClasses) {
      writeLambda(lambdaDef, toDirectory)
    }
  } // writeLambdas

  // intended for testing.  Deletes all created classes in the given directory
  def deleteClasses(inDirectory: String): Unit = {
    import scala.collection.JavaConversions._
    for (lambdaDef <- additionalClasses) {
      new Nothing(inDirectory, lambdaDef.className.name + ".class").delete
    }
  } // deleteClasses


}
