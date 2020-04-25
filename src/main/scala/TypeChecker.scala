case class IllTypedException(msg: String) extends Exception(msg)


object Typechecker {
  type SymbolTableClass = Map[String, DefExtClass]

  def makeSymbolTables(myClasses: List[Class], stClass: SymbolTableClass): SymbolTableClass= {
    myClasses.foldLeft(stClass) ((res, cur) =>{
      cur match{
        case head: DefClass =>
          res ++ makeSymbolTableDefClassHelper(head, stClass)
        case head: DefExtClass =>
          res ++  makeSymbolTableClassExtHelper(head, stClass)
      }
    })
  }

  def makeSymbolTableClassExtHelper(myClass: DefExtClass, symbolTableClass: SymbolTableClass): SymbolTableClass ={
    val DefExtClass(classname, extendedClass, statements, instances,parameters,methods) = myClass
    val methodNames = methods.map(_.methodName).toSet
    if (methodNames.size != methods.size) {
      throw _root_.IllTypedException("duplicate methods ")
    }
    val instanceParamNames = instances.map(_.v1.varName).toSet
    if (instanceParamNames.size != instances.size) {
      throw _root_.IllTypedException("duplicate instance variable")
    }
    val paramNames = parameters.map(_.varName).toSet
    if (paramNames.size != parameters.size) {
      throw _root_.IllTypedException("duplicate constructor parameters")
    }
    if (symbolTableClass.contains(classname)) {
      throw _root_.IllTypedException("duplicate Class name: " + classname)
    }
    symbolTableClass + (classname -> myClass)
  }

  def makeSymbolTableDefClassHelper(myClass: DefClass, symbolTableClass: SymbolTableClass): SymbolTableClass={
    val DefClass(classname, statements, instances, parameters, methods) = myClass
    val methodNames = methods.map(_.methodName).toSet
    if (methodNames.size != methods.size) {
      throw _root_.IllTypedException("duplicate methods ")
    }
    val instanceParamNames = instances.map(_.v1.varName).toSet
    if (instanceParamNames.size != instances.size) {
      throw _root_.IllTypedException("duplicate instance variable")
    }
    val paramNames = parameters.map(_.varName).toSet
    if (paramNames.size != parameters.size) {
      throw _root_.IllTypedException("duplicate constructor parameters")
    }
    if (symbolTableClass.contains(classname)) {
      throw _root_.IllTypedException("duplicate Class name: " + classname)
    }

    symbolTableClass + (classname -> DefExtClass(classname,"",statements,instances,parameters,methods))
  }

  def allDistinct[A](items: Seq[A]): Boolean = {
    items.toSet.size == items.size
  }

  // also typechecks the input program
  def apply(myProgram: Program): _root_.Typechecker = {
    val classSymbolTable = makeSymbolTables(myProgram.classes,Map())
    val retval = new _root_.Typechecker(classSymbolTable)
    retval.typecheckProgram(myProgram, Map())
    retval
  }
} // Typechecker
import Typechecker.SymbolTableClass


class Typechecker(val stc: SymbolTableClass){
  type TypeEnv = Map[String, Types]

  def typeof(e: Exp, gamma: TypeEnv): Types = {
    e match {
      case VariableExp(x) if gamma.contains(x) => gamma(x)
      case IntegerExp(_) => IntTypes
      case StringExp(_) => StrTypes
      case BooleanExp(true) | BooleanExp(false) => BoolTypes
      case AndExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (BoolTypes, BoolTypes) => BoolTypes
          case _ => throw _root_.IllTypedException("and")
        }
      }
      case PrintExp(e1) => {
        typeof(e1, gamma) match {
          case StrTypes => StrTypes
          case _ => throw _root_.IllTypedException("print")
        }
      }
      case PlusExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => IntTypes
          case _ => throw _root_.IllTypedException("add")
        }
      }
      case OrExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (BoolTypes, BoolTypes) => BoolTypes
          case _ => throw _root_.IllTypedException("or")
        }
      }
      case GTExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => BoolTypes
          case _ => throw _root_.IllTypedException("greater than")
        }
      }
      case GTEExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => BoolTypes
          case _ => throw _root_.IllTypedException("greater than equals")
        }
      }
      case LTExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => BoolTypes
          case _ => throw _root_.IllTypedException("less than")
        }
      }
      case LTEExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => BoolTypes
          case _ => throw _root_.IllTypedException("less than equals")
        }
      }
      case CastExp(t1, e1) => { // assumes the user knows how to cast exp ?
        t1 match{
          case x:Types => {
            x
          }
          case _ => throw _root_.IllTypedException("cast exp")
        }
      }
      case GroupedExp(e1) => { // assumes the user knows how to cast exp ?
        typeof(e1,gamma) match {
          case x:Types => {
            x
          }
          case _ => throw _root_.IllTypedException("GroupedExp")
        }
      }
      // method call will need to check
      case MethodExp(e1, methodName, params) => {
        if (typeof(e1,gamma)== StrTypes) {
          val className = e1.asInstanceOf[StringExp]
          stc(className.value) match{
            case myClass:DefExtClass =>{
              myClass.methods(methodName) match{
                case myMethod:MethodDef =>{
                  val (returnTypes, paramVardecs) = (myMethod.types, myMethod.parameters)
                  if (params.size != paramVardecs.size) {
                    throw _root_.IllTypedException("wrong number of params")
                  } else {
                    val expectedTypes = paramVardecs.foldLeft(List(): List[Types])((res,cur)=>{res :+ cur.types})
                    val actualTypes = params.foldLeft(List(): List[Types])((res,cur)=>{res :+ typeof(cur,gamma)})
                    if (expectedTypes != actualTypes) {
                      throw _root_.IllTypedException("parameter type mismatch")
                    } else {
                      returnTypes
                    }
                  }
                }
              }
            }
          }
        }
        else{
          throw _root_.IllTypedException("Class is not in string format")
        }
      }
      case NewClassExp(className: String , e1:List[Exp])=>{
        if(stc contains className) {
          val myClass = stc(className)
          if(e1.size != myClass.parameters.size){
            throw _root_.IllTypedException("Missing Parameters")
          }
          else{
            val actualTypes = e1.foldLeft(List(): List[Types])((res,cur)=>{res :+ typeof(cur,gamma)})
            val expectedTypes = myClass.parameters.foldLeft(List(): List[Types])((res,cur)=>{res :+ cur.types })
            if(expectedTypes != actualTypes) {
              throw _root_.IllTypedException("parameters for new class don't match")
            }
            else{
              ClassTypes(className)
            }
          }
        }else{
          throw _root_.IllTypedException("class not defined")
        }
      }
      case HighOrderExp(params, exp) if _root_.Typechecker.allDistinct(params.map(_.varName)) =>{
        val gamma2 = gamma ++ params.map(pair => (pair.varName -> pair.types))
        val tau2 = typeof(exp, gamma2)
        MethodTypes(params.map(_.types), tau2)
      }
      case CallHighOrderExp(func,params) =>{
        typeof(func, gamma) match {
          case MethodTypes(tau1, tau2) if tau1.size == params.size => {
            if (params.map(e => typeof(e, gamma)) == tau1) {
              tau2
            } else {
              throw _root_.IllTypedException("parameter type mismatch")
            }
          }
          case _ => throw _root_.IllTypedException("not a higher-order function")
        }
      }

      case _ => throw _root_.IllTypedException("other-exp")
    }
  } // typeof


  def typecheckMethodDef(className:String, methodDef: MethodDef): Unit ={
    checkForDuplicatesParameters(methodDef.parameters)
    val gamma1 = methodDef.parameters.map(pair => (pair.varName -> pair.types)).toMap
    val gamma2 = typecheckStatement(methodDef.stmt, gamma1,false)
    if (typeof(methodDef.returnExpression, gamma2) != methodDef.types) {
      throw _root_.IllTypedException("return type mismatch")
    }
  }

  def typecheckClasses(): Unit ={
    stc.foreach(myClass => {
      checkForCycles(myClass._1, List()) // pass an empty list because you havent seen anything
      typecheckClass(myClass._1)
    })
  }

  def checkForCycles(className: String, seen: List[String]): Unit ={
    if (className == "") {
      // if you see "" it is our make shift terminator ie a base class with no extends
    }else{
      if (seen contains className){
        throw  _root_.IllTypedException("Cyclic inheritance on " + className)
      }else{
        val newSeen: List[String] = seen.:+(className)
        val nextClassName = stc(className).extendedClass
        checkForCycles(nextClassName,newSeen)
      }
    }
  }
  def checkForDuplicatesInstanceVars(instanceVars: List[InstanceDec]): Unit ={
    val varNames = instanceVars.map(_.v1.varName).toSet
    if (varNames.size != instanceVars.size) {
      throw _root_.IllTypedException("duplicate instance variable")
    }
  }
  def checkForDuplicatesParameters(parameters: List[VarDeclaration]): Unit ={
    val varNames = parameters.map(_.varName).toSet
    if (varNames.size != parameters.size) {
      throw _root_.IllTypedException("duplicate instance variable")
    }
  }

  def typecheckClass(className: String): Unit ={
    val myClass = stc(className)
    def checkForDuplicatesMethods(methods: List[MethodDef]): Unit ={
      val methodNames = methods.map(_.methodName).toSet
      if (methodNames.size != methods.size) {
        throw _root_.IllTypedException("duplicate method Name")
      }
    }
    @scala.annotation.tailrec
    def instanceVarsDoNotOverride(className: String, seen: List[String]): Unit ={
      if (className != "") {
        val classDef = stc(className);
        classDef.instances.foldLeft(seen)((res,cur) =>{
          if(seen contains cur.v1.varName){
            throw _root_.IllTypedException("Instance variable has been overrided")
          }else{
            res :+ cur.v1.varName
          }
        })
        instanceVarsDoNotOverride(classDef.extendedClass,seen);
      }
    }


    if (myClass.extendedClass != "" || stc.contains( myClass.extendedClass )){
      checkForDuplicatesMethods(myClass.methods)
      checkForDuplicatesInstanceVars(myClass.instances)
      instanceVarsDoNotOverride(className,List())
      myClass.methods.foreach(myMethod => typecheckMethodDef(className,myMethod))
    }else {
      throw _root_.IllTypedException("Extended Class Does not exist")
    }
  }




  def typecheckStatement(s: Stmt, gamma: TypeEnv, forLoopBool: Boolean): TypeEnv = {
    s match {
      case ExpStmt(e1: Exp) =>{
        typeof(e1,gamma)
        gamma
      }
      case BreakStmt => {
        if(forLoopBool == true){
          gamma
        }
        else{
          throw _root_.IllTypedException("Break")
        }
      }
        // we got rid of this
//      case ReturnStmt(e1: Exp)=>{
//        if(typeof(e1, gamma) == Types){
//          gamma
//        }
//        else{
//          throw IllTypedException("Return")
//        }
//      }
      case AssignmentStmt(vd1:VarDeclaration, e1:Exp) =>{
        val tau = vd1.types
        if(typeof(e1, gamma) == tau){
          gamma + (vd1.varName -> tau)
        }
        else{
          throw _root_.IllTypedException("Assignment")
        }
      }
      case VarStmt(name: String, e1: Exp) if gamma.contains(name) =>{
        val tau = gamma(name)
        if(typeof(e1, gamma) == tau){
          gamma
        }
        else{
          throw _root_.IllTypedException("Var Statement")
        }
      }
      case ForStmt(s1: Stmt, e1:Exp, s2: Stmt, forBody: Stmt)=>{
        val breakBool = true
        val gamma2 = typecheckStatement(s1, gamma, false)
        if(typeof(e1,gamma2) == BoolTypes) {
          val gamma3 = typecheckStatement(s2, gamma2, false)
          typecheckStatement(forBody, gamma3, breakBool)
          gamma
        }
        else{
          throw _root_.IllTypedException("For Statement")
        }
      }
      case ConditionalStmt(e1: Exp, stmtTrue: Stmt, stmtFalse: Stmt)=>{
        if(typeof(e1, gamma) == BoolTypes) {
          typecheckStatement(stmtTrue, gamma, false)
          typecheckStatement(stmtFalse, gamma, false)
          gamma
        }else{
          throw _root_.IllTypedException("conditional statement")
        }
      }
      case BlockStmt(st: List[Stmt])=>{
        st.foreach{
          currentStatement => {
            st.foldLeft(gamma)((currentGamma, currentStatement) => typecheckStatement(currentStatement, gamma,false))
          }
        }
        gamma
      }
    }
  } // typecheckStatement

  def typecheckProgram(input: Program, gamma: TypeEnv) {
    typecheckClasses()
    typeof(input.entryPoint,gamma)
  } // typecheckProgram
} // Typechecker