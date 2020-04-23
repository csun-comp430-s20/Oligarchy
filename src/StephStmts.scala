case class IllTypedException(msg: String) extends Exception(msg)

object Typechecker {
  type TypeEnv = Map[Variable, Types]

  def typeof(e:Exp, gamma:TypeEnv): Types ={
    e match{
      case VarDec(t1: Types) =>{
        (typeof(t1, gamma)) match{
          case (Types) => t1
          case _ => throw IllTypedException("Types")
        }
      }
    }
  }
  def typecheckStatements(s: Stmt, gamma: TypeEnv): TypeEnv = {
    s match {
      case ExpStmt(e1: Exp) =>{
        if(typeof(e1, gamma)== Types){
          gamma
        }
        else{
          throw IllTypedException("Expression")
        }
      }
      case BreakStmt => gamma
      case VoidStmt => VoidTypes
      case ReturnStmt(e1: Exp)=>{
        if(typeof(e1, gamma) == Types){
          gamma
        }
        else{
          throw IllTypedException("Return")
        }
      }
      case AssignmentStmt(vd1:VarDec, tau, e1:Exp) =>{
        if(typeof(vd1, e1) == tau){
          gamma + (vd1->tau)
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
        var gamma2 = typecheckStatements(s1, gamma)
        if(typeof(e1,gamma2) == BoolTypes) {
          var gamma3 = typecheckStatements(s2, gamma2)
          var gamma4 = typecheckStatements(forBody, gamma3)
          gamma
         }
        else{
          throw IllTypedException("For Statement")
         }
      }
      case ConditionalStmt(e1: Exp, stmtTrue: Stmt, stmtFalse: Stmt)=>{
        if(typeof(e1, gamma) == BoolTypes) {
          var gamma2 = typecheckStatements(stmtTrue, gamma)
          var gamma3 = typecheckStatements(stmtFalse, gamma2)
          gamma
        }
      }
      case BlockStmt(st: List[Stmt])=>{
        st.foreach{
          typecheckStatements(_, gamma)
        }
        gamma
      }
    }
  }

}