import org.objectweb.asm.Opcodes._
import org.objectweb.asm.ClassWriter

class LambdaMaker {

  val LAMBDA_PREFIX = "Lambda"
  val EXTENDS_NAME = "Function1"
  val APPLY_NAME = "apply"
  //  val REWRITTEN_THIS = "~" + ClassGenerator.thisVariable.name

  private var allClasses: Map[String, Class] = _
  private var additionalClasses: List[LambdaDef] = _
  private var curLambda = 0

  def apply(allClasses: Map[String, Class]): LambdaMaker = new LambdaMaker() {
    this.allClasses = allClasses
    additionalClasses = List()
  }

  //  def setUnion[A](first: Set[A],second: Set[A]): Set[A] ={
  //    first.++(second)
  //  }
  //
  //  def addSet[A](set: Set[A], element:A ): Set[A] ={
  //    set.+(element)
  //  }

  def freeVariables(params: Set[String], exps: List[Exp]): Set[String] = {
    exps.foldLeft(Set(): Set[String])((res, cur) => {
      res ++ freeVariables(params, cur)
    })
  }

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
      case MethodExp(e1, methodName, e2) => freeVariables(params, e1) ++ freeVariables(params, e2)
      case CastExp(newTypes, e2) => freeVariables(params, e2)
      case GroupedExp(e) => freeVariables(params, e)
      //      case HighOrderExp(param, body) =>freeVariables(params)
      //      case CallHighOrderExp(function, params) =>
      case LTEExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
      case LTExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
      case GTEExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
      case GTExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
      case AndExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
      case OrExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
      case PlusExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
      case SubtractExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
      case MultiplyExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
      case DivideExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
      case PowerExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
      case EqualsExp(leftExp, rightExp) => freeVariables(params, leftExp) ++ freeVariables(params, rightExp)
    }
  }

  @throws[CodeGeneratorException]
  def freeVariables(highOrderExp: HighOrderExp): Set[String] = {
//    freeVariables(addSet(Set(), lambdaExp), lambdaExp.body)
  } // freeVariables

  @throws[CodeGeneratorException]
  def classDefinitionFor(name: String): LambdaDef = {
    for (lambdaDef <- additionalClasses) {
//      if (lambdaDef.className.equals(name)) return lambdaDef
    }
    throw new CodeGeneratorException("No such class: " + name)
  } // classDefinitionFor
}
