package src

import Typechecker.{typecheckStatement, typeof}

case class IllTypedException(msg: String) extends Exception(msg)

object Typechecker {
  type TypeEnv = Map[String, Types]

  def typeof(e: Exp, gamma: TypeEnv): Types = {
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
      case VarDeclaration(t1: Types) =>{
        (typeof(t1, gamma)) match{
          case (Types) => t1
          case _ => throw IllTypedException("Variable Declaration")
        }
      }
    } // match
  } // typeof

  def typecheckStatements(s: Stmt, gamma: TypeEnv): TypeEnv = {
    s match {
      case ExpStmt(e1: Exp) =>{
        if(typeof(e1, gamma) Types){
          gamma
        }
        else{
          throw IllTypedException("Expression")
        }
      }
      case BreakStmt => gamma
      //      case VoidStmt => VoidTypes
      case ReturnStmt(e1: Exp)=>{
        if(typeof(e1, gamma) == Types){
          gamma
        }
        else{
          throw IllTypedException("Return")
        }
      }
      case AssignmentStmt(vd1:VarDeclaration, e1:Exp) =>{
        val tau = vd1.types
        if(typeof(e1, gamma) == tau){
          gamma + (vd1.varName -> tau)
        }
        else{
          throw IllTypedException("Assignment")
        }
      }
      case VarStmt(name: String, e1: Exp) if gamma.contains(name) =>{
        val tau = gamma(name)
        if(typeof(e1, gamma) == tau){
          gamma
        }
        else{
          throw IllTypedException("Var Statement")
        }
      }
      case ForStmt(s1: Stmt, e1:Exp, s2: Stmt, forBody: Stmt)=>{
        val gamma2 = typecheckStatements(s1, gamma)
        if(typeof(e1,gamma2) == BoolTypes) {
          val gamma3 = typecheckStatements(s2, gamma2)
          typecheckStatements(forBody, gamma3)
          gamma
        }
        else{
          throw IllTypedException("For Statement")
        }
      }
      case ConditionalStmt(e1: Exp, stmtTrue: Stmt, stmtFalse: Stmt)=>{
        if(typeof(e1, gamma) == BoolTypes) {
          typecheckStatements(stmtTrue, gamma)
          typecheckStatements(stmtFalse, gamma)
          gamma
        }
      }
      case BlockStmt(st: List[Stmt])=>{
        st.foreach{
          currentStatement => {
            st.foldLeft(gamma)((currentGamma, currentStatement) => typecheckStatements(currentStatement, gamma))
          }
        }
        gamma
      }
    }
  } // typecheckStatement

  def typecheckProgram(input: Program, gamma: TypeEnv): TypeEnv = {

  } // typecheckProgram
} // Typechecker