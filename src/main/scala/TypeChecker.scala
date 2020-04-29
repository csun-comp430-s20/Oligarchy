
case class IllTypedException(msg: String) extends Exception(msg)


object Typechecker {
  type SymbolTableClass = Map[String, DefExtClass]

  def makeSymbolTables(myClasses: List[Class], stClass: SymbolTableClass): SymbolTableClass= {
    myClasses.foldLeft(stClass) ((res: SymbolTableClass, cur:Class) =>{
      cur match{
        case head: DefClass =>
          makeSymbolTableDefClassHelper(head,res)
        case head: DefExtClass =>
          makeSymbolTableClassExtHelper(head,res)
      }
    })
  }

  def makeSymbolTableClassExtHelper(myClass: DefExtClass, symbolTableClass: SymbolTableClass): SymbolTableClass ={
    val DefExtClass(classname, extendedClass, statements, instances,parameters,methods) = myClass
    val methodNames = methods.map(_.methodName).toSet
    if (methodNames.size != methods.size) {
      throw IllTypedException("duplicate methods ")
    }
    val instanceParamNames = instances.map(_.v1.varName).toSet
    if (instanceParamNames.size != instances.size) {
      throw IllTypedException("duplicate instance variable")
    }
    val paramNames = parameters.map(_.varName).toSet
    if (paramNames.size != parameters.size) {
      throw IllTypedException("duplicate constructor parameters")
    }
    if (symbolTableClass.contains(classname)) {
      throw IllTypedException("duplicate Class name: " + classname)
    }
    symbolTableClass + (classname -> myClass)
  }

  def makeSymbolTableDefClassHelper(myClass: DefClass, symbolTableClass: SymbolTableClass): SymbolTableClass={
    val DefClass(classname, statements, instances, parameters, methods) = myClass
    val methodNames = methods.map(_.methodName).toSet
    if (methodNames.size != methods.size) {
      throw IllTypedException("duplicate methods ")
    }
    val instanceParamNames = instances.map(_.v1.varName).toSet
    if (instanceParamNames.size != instances.size) {
      throw IllTypedException("duplicate instance variable")
    }
    val paramNames = parameters.map(_.varName).toSet
    if (paramNames.size != parameters.size) {
      throw IllTypedException("duplicate constructor parameters")
    }
    if (symbolTableClass.contains(classname)) {
      throw IllTypedException("duplicate Class name: " + classname)
    }

    symbolTableClass + (classname -> DefExtClass(classname,"",statements,instances,parameters,methods))
  }

  def allDistinct[A](items: Seq[A]): Boolean = {
    items.toSet.size == items.size
  }

  // also typechecks the input program
  def apply(myProgram: Program): Typechecker = {
    val classSymbolTable = makeSymbolTables(myProgram.classes,Map())
    val retval = new Typechecker(classSymbolTable)
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
          case _ => throw IllTypedException("and")
        }
      }
      case SubtractExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => IntTypes
          case _ => throw IllTypedException("and")
        }
      }
      case MultiplyExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => IntTypes
          case _ => throw IllTypedException("and")
        }
      }
      case DivideExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => IntTypes
          case _ => throw IllTypedException("and")
        }
      }
      case PowerExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => IntTypes
          case _ => throw IllTypedException("and")
        }
      }
      case EqualsExp(e1, e2) => {
        (typeof(e1, gamma), typeof(e2, gamma)) match {
          case (IntTypes, IntTypes) => BoolTypes
          case (StrTypes, StrTypes) => BoolTypes
          case (BoolTypes, BoolTypes) => BoolTypes
          case _ => throw IllTypedException("and")
        }
      }
      case PrintExp(e1) => {
        typeof(e1, gamma) match {
          case StrTypes => StrTypes
          case _ => throw IllTypedException("print")
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
      case CastExp(t1, e1) => { // assumes the user knows how to cast exp ?
        t1 match{
          case x:Types => {
            x
          }
          case _ => throw IllTypedException("cast exp")
        }
      }
      case GroupedExp(e1) => { // assumes the user knows how to cast exp ?
        typeof(e1,gamma) match {
          case x:Types => {
            x
          }
          case _ => throw IllTypedException("GroupedExp")
        }
      }
      // method call will need to check
      case MethodExp(e1, methodName, params) => {
        if (typeof(e1,gamma)== StrTypes) {
          val className = e1.asInstanceOf[StringExp]
          stc(className.value) match{
            case myClass:DefExtClass =>{
              myClass.methods.foreach {
                case myMethod: MethodDef  if myMethod.methodName == methodName => {
                  val (returnTypes, paramVardecs) = (myMethod.types, myMethod.parameters)
                  if (params.size != paramVardecs.size) {
                    throw IllTypedException("wrong number of params")
                  } else {
                    val expectedTypes = paramVardecs.foldLeft(List(): List[Types])((res, cur) => {
                      res :+ cur.types
                    })
                    val actualTypes = params.foldLeft(List(): List[Types])((res, cur) => {
                      res :+ typeof(cur, gamma)
                    })
                    if (expectedTypes != actualTypes) {
                      throw IllTypedException("parameter type mismatch")
                    } else {
                      returnTypes
                    }
                  }
                }case _ =>
                  throw IllTypedException("Method Name not found")
              }
              throw IllTypedException("no Methods were defined")
            }
            case _ =>
              throw IllTypedException("Class name not found")
          }
        }
        else{
          throw IllTypedException("Class is not in string format")
        }
      }
      case NewClassExp(className: String , e1:List[Exp])=>{
        if(stc contains className) {
          val myClass = stc(className)
          if(e1.size != myClass.parameters.size){
            throw IllTypedException("Missing Parameters")
          }
          else{
            val actualTypes = e1.foldLeft(List(): List[Types])((res,cur)=>{res :+ typeof(cur,gamma)})
            val expectedTypes = myClass.parameters.foldLeft(List(): List[Types])((res,cur)=>{res :+ cur.types })
            if(expectedTypes != actualTypes) {
              throw IllTypedException("parameters for new class don't match")
            }
            else{
              ClassTypes(className)
            }
          }
        }else{
          throw IllTypedException("class not defined")
        }
      }
      case HighOrderExp(params, exp) if Typechecker.allDistinct(params.map(_.varName)) =>{
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
              throw IllTypedException("parameter type mismatch")
            }
          }
          case _ => throw IllTypedException("not a higher-order function")
        }
      }

      case _ => throw IllTypedException("other-exp")
    }
  } // typeof


  def typecheckMethodDef(className:String, methodDef: MethodDef): Unit ={
    checkForDuplicatesParameters(methodDef.parameters)
    val gamma1 = methodDef.parameters.map(pair => (pair.varName -> pair.types)).toMap
    val gamma2 = typecheckStatement(methodDef.stmt, gamma1,false)
    if (typeof(methodDef.returnExpression, gamma2) != methodDef.types) {
      throw IllTypedException("return type mismatch")
    }
  }

  def typecheckClasses(myClasses: List[Class]) : Unit ={
    if( myClasses.nonEmpty){
      myClasses.foldLeft(List(): List[String])((res,cur)=>{
        cur match {
        case myClass: DefClass =>
          checkForCycles(myClass.className, res) // pass an empty list because you havent seen anything
          typecheckClass(myClass.className)
          res
        case myClass: DefExtClass =>
          checkForCycles(myClass.classname,res)
          typecheckClass(myClass.classname)
          res
        }}
      )
    }
  }

  def checkForCycles(className: String, seen: List[String]): Unit ={
    if (className == "") {
      // if you see "" it is our make shift terminator ie a base class with no extends
    }else{
      if (seen contains className){
        throw  IllTypedException("Cyclic inheritance on " + className)
      }else{
        val newSeen: List[String] = seen.:+(className)
        val nextClassName = stc(className).extendedClass
        if (nextClassName != ""){
          checkForCycles(nextClassName,newSeen)
        }// else no more cycles
      }
    }
  }
  def checkForDuplicatesInstanceVars(instanceVars: List[InstanceDec]): Unit ={
    val varNames = instanceVars.map(_.v1.varName).toSet
    if (varNames.size != instanceVars.size) {
      throw IllTypedException("duplicate instance variable")
    }
  }
  def checkForDuplicatesParameters(parameters: List[VarDeclaration]): Unit ={
    val varNames = parameters.map(_.varName).toSet
    if (varNames.size != parameters.size) {
      throw IllTypedException("duplicate instance variable")
    }
  }

  def typecheckClass(className: String): Unit ={
    val myClass = stc(className)
    def checkForDuplicatesMethods(methods: List[MethodDef]): Unit ={
      val methodNames = methods.map(_.methodName).toSet
      if (methodNames.size != methods.size) {
        throw IllTypedException("duplicate method Name")
      }
    }
    @scala.annotation.tailrec
    def instanceVarsDoNotOverride(className: String, seen: List[String]): Unit ={
      if (className != "") {
        val classDef = stc(className);
        classDef.instances.foldLeft(seen)((res,cur) =>{
          if(seen contains cur.v1.varName){
            throw IllTypedException("Instance variable has been overrided")
          }else{
            res :+ cur.v1.varName
          }
        })
        instanceVarsDoNotOverride(classDef.extendedClass,seen);
      }
    }
    if (myClass.extendedClass == "" || stc.contains( myClass.extendedClass )){
      checkForDuplicatesMethods(myClass.methods)
      checkForDuplicatesInstanceVars(myClass.instances)
      instanceVarsDoNotOverride(className,List())
      myClass.methods.foreach(myMethod => typecheckMethodDef(className,myMethod))
    }else {
      throw IllTypedException("Extended Class Does not exist")
    }
  }




  def typecheckStatement(s: Stmt, gamma: TypeEnv, forLoopBool: Boolean): TypeEnv = {
    s match {
      case ExpStmt(e1: Exp) =>{
        typeof(e1,gamma)
        gamma
      }
      case BreakStmt => {
        if(forLoopBool){
          gamma
        }
        else{
          throw IllTypedException("Break")
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
        val breakBool = true
        val gamma2 = typecheckStatement(s1, gamma, false)
        if(typeof(e1,gamma2) == BoolTypes) {
          val gamma3 = typecheckStatement(s2, gamma2, false)
          typecheckStatement(forBody, gamma3, breakBool)
          gamma
        }
        else{
          throw IllTypedException("For Statement")
        }
      }
      case ConditionalStmt(e1: Exp, stmtTrue: Stmt, stmtFalse: Stmt)=>{
        if(typeof(e1, gamma) == BoolTypes) {
          typecheckStatement(stmtTrue, gamma, false)
          typecheckStatement(stmtFalse, gamma, false)
          gamma
        }else{
          throw IllTypedException("conditional statement")
        }
      }
      case BlockStmt(st: List[Stmt])=>{
        st.foldLeft(gamma)((currentGamma, currentStatement) => typecheckStatement(currentStatement, currentGamma,false))
      }
    }
  } // typecheckStatement

  def typecheckProgram(input: Program, gamma: TypeEnv) {
    typecheckClasses(input.classes)
    typeof(input.entryPoint,gamma)
  } // typecheckProgram
} // Typechecker