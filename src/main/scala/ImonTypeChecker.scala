
case class IllTypedException(msg: String) extends Exception(msg)

object Typechecker {
  type TypeEnv = Map[String, Type]

  def typeof(e: Exp, gamma: TypeEnv): Type = {
    e match{
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
          case _ => throw IllTypedException("add")
        }
      }
      case OrExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (BoolTypes, BoolTypes) => BoolTypes
          case _ => throw IllTypedException("or")
        }
      }
      case GTExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => BoolTypes
          case _ => throw IllTypedException("greater than")
        }
      }
      case GTEExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => BoolTypes
          case _ => throw IllTypedException("greater than equals")
        }
      }
      case LTExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => BoolTypes
          case _ => throw IllTypedException("less than")
        }
      }
      case LTEExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => BoolTypes
          case _ => throw IllTypedException("less than equals")
        }
      }
      case CallHighOrderExp(e1, e2) => {
        typeof(e1, gamma) match {
          case MethodTypes(tau1, tau2) if tau1.size ==e2.size => {
            if (e2.map(e => typeof(e, gamma)) == tau1) {
              tau2
            }
            else{
              throw IllTypedException("parameter type mismatch")
            }
          }
          case _ => throw IllTypedException("not a higher-order function")
        }
      }
      case _ => throw IllTypedException("other-exp")
    }

  } // typeof

  def typecheckStatement(s: Stmt, gamma: TypeEnv): TypeEnv = {

  } // typecheckStatement

  def typecheckProgram(input: Program, gamma: TypeEnv): TypeEnv = {

  } // typecheckProgram
} // Typechecker