case class IllTypedException(msg: String) extends Exception(msg)

object Typechecker {
  type TypeEnv = Map[Variable, Type]

  def typeof(e: Exp, gamma: TypeEnv): Type = {
    e match {
      case VariableExp(x) if gamma.contains(x) => gamma(x)
      case IntegerExp(_) => IntTypes
      case BooleanExp(_) => BoolTypes
      case AndExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (BoolTypes, BoolTypes) => BoolTypes
          case _ => throw IllTypedException("and")
        }
      }
      case PlusExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => IntTypes
          case _ => throw IllTypedException("plus")
        }
      }
      case SubtractExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => IntTypes
          case _ => throw IllTypedException("subtract")
        }
      }
      case MultiplyExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => IntTypes
          case _ => throw IllTypedException("multiply")
        }
      }
      case DivideExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => IntTypes
          case _ => throw IllTypedException("divide")
        }
      }
      case PowerExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => IntTypes
          case _ => throw IllTypedException("power")
        }
      }
      case EqualsExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => BoolTypes
          case (StrTypes, StrTypes) => BoolTypes
          case _ => throw IllTypedException("equals")
        }
      }
    }
  } // typeof

  def typecheckStatement(s: Stmt, gamma: TypeEnv): TypeEnv = {

  } // typecheckStatement

  def typecheckClass(c: Class) {

  }

  def typecheckProgram(input: Prgm, gamma: TypeEnv) {
    input.c1.foreach(typecheckClass)
    typeof(input.e1, gamma)
  } // typecheckProgram
} // Typechecker