package src
case class IllTypedException(msg: String) extends Exception(msg)

object Typechecker {
  type TypeEnv = Map[, Type]

  def typeof(e: Exp, gamma: TypeEnv): Type = {

  } // typeof

  def typecheckStatement(s: Stmt, gamma: TypeEnv): TypeEnv = {

  } // typecheckStatement

  def typecheckProgram(input: Program, gamma: TypeEnv): TypeEnv = {

  } // typecheckProgram
} // Typechecker