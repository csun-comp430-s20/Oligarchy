//package src
//
//case class IllTypedException(msg: String) extends Exception(msg)
//
//object Typechecker {
//  type TypeEnv = Map[String, Types]
//
//  def typecheckStatements(s: Stmt, gamma: TypeEnv): TypeEnv = {
//    s match {
//      case ExpStmt(e1: Exp) =>{
//        if(typeof(e1, gamma) Types){
//          gamma
//        }
//        else{
//          throw IllTypedException("Expression")
//        }
//      }
//      case BreakStmt => gamma
////      case VoidStmt => VoidTypes
//      case ReturnStmt(e1: Exp)=>{
//        if(typeof(e1, gamma) == Types){
//          gamma
//        }
//        else{
//          throw IllTypedException("Return")
//        }
//      }
//      case AssignmentStmt(vd1:VarDeclaration, e1:Exp) =>{
//        val tau = vd1.types
//        if(typeof(e1, gamma) == tau){
//          gamma + (vd1.varName -> tau)
//        }
//        else{
//          throw IllTypedException("Assignment")
//        }
//      }
//      case VarStmt(name: String, e1: Exp) if gamma.contains(name) =>{
//        val tau = gamma(name)
//        if(typeof(e1, gamma) == tau){
//          gamma
//        }
//        else{
//         throw IllTypedException("Var Statement")
//        }
//      }
//      case ForStmt(s1: Stmt, e1:Exp, s2: Stmt, forBody: Stmt)=>{
//        val gamma2 = typecheckStatements(s1, gamma)
//        if(typeof(e1,gamma2) == BoolTypes) {
//          val gamma3 = typecheckStatements(s2, gamma2)
//          typecheckStatements(forBody, gamma3)
//          gamma
//         }
//        else{
//          throw IllTypedException("For Statement")
//         }
//      }
//      case ConditionalStmt(e1: Exp, stmtTrue: Stmt, stmtFalse: Stmt)=>{
//        if(typeof(e1, gamma) == BoolTypes) {
//          typecheckStatements(stmtTrue, gamma)
//          typecheckStatements(stmtFalse, gamma)
//          gamma
//        }
//      }
//      case BlockStmt(st: List[Stmt])=>{
//        st.foreach{
//          currentStatement => {
//            st.foldLeft(gamma)((currentGamma, currentStatement) => typecheckStatements(currentStatement, gamma))
//          }
//        }
//        gamma
//      }
//    }
//  }
//
//
//
//}