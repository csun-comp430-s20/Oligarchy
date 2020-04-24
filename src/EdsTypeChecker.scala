package src
case class IllTypedException(msg: String) extends Exception(msg)


object Typechecker {
  type SymbolTable = Map[String, (Types, List[Types])]
  type SymbolTableClass = Map[String, DefExtClass]

  def makeSymbolTables(myClasses: List[Class], stClass: SymbolTableClass, stFunctions: SymbolTable): (SymbolTableClass, SymbolTable )= {
    myClasses.foldLeft(stClass) ((res, cur) =>{
      case head: DefClass =>
        val newClassSymbolTable = makeSymbolTableDefClassHelper(head, stClass)
      case head: DefExtClass =>
        val newClassSymbolTable = makeSymbolTableClassExtHelper(head, stClass)
      newClassSymbolTable
    })
  }

  def makeSymbolTableClassExtHelper(myClass: DefExtClass, symbolTableClass: SymbolTableClass): SymbolTableClass ={
    myClass.foldLeft(symbolTableClass)((res, cur) => {
      val DefExtClass(classname, extendedClass, statements, instances,parameters,methods) = cur
      val methodNames = methods.map(_.methodName).toSet
      if (methodNames.size != methods.size) {
        throw IllTypedException("duplicate methods ")
      }
      val paramNames = instances.map(_.v1.varName).toSet
      if (paramNames.size != parameters.size) {
        throw IllTypedException("duplicate instance variable")
      }
      if (res.contains(classname)) {
        throw IllTypedException("duplicate Class name: " + classname)
      }
      res + (classname -> cur)
    })

  }

  def makeSymbolTableDefClassHelper(myClass: DefClass, symbolTableClass: SymbolTableClass): SymbolTableClass={
    myClass.foldLeft(symbolTableClass)((res, cur) => {
      val DefClass(classname,
      statements,
      instances,
      parameters,
      methods) = cur
      if (res.contains(classname)) {
        throw IllTypedException("duplicate Class name: " + classname)
      }
      val paramNames = instances.map(_.v1.varName).toSet
      if (paramNames.size != instances.size) {
        throw IllTypedException("duplicate instance variable")
      }
      res + (classname -> DefExtClass(classname,"",statements,instances,parameters,methods))
    })
  }

  def makeSymbolTable(myClass: Class, symbolTable: SymbolTable): SymbolTable = {
    myClass match{
      case newClass: DefClass =>  {
        makeSymbolTableHelper(newClass.methods, symbolTable)
      }
      case newExtendClass: DefExtClass =>{
        makeSymbolTableHelper(newExtendClass.methods, symbolTable)
      }
    }
  }

  def makeSymbolTableHelper(methods: List[MethodDef], symbolTable: SymbolTable): SymbolTable ={
    methods.foldLeft(symbolTable)((res, cur) => {
      val MethodDef(returnType, methodName, _, parameters, _) = cur
      if (res.contains(methodName)) {
        throw IllTypedException("duplicate function name: " + methodName)
      }
      val paramNames = parameters.map(_.varName).toSet
      if (paramNames.size != parameters.size) {
        throw IllTypedException("duplicate parameter name")
      }
      res + (methodName -> (returnType -> parameters.map(_.types)))
    })
  }
  def allDistinct[A](items: Seq[A]): Boolean = {
    items.toSet.size == items.size
  }

  // also typechecks the input program
  def apply(myProgram: Program): Typechecker = {
    val (classSymbolTable,functionSymbolTable) = makeSymbolTables(myProgram.classes,Map(),Map())
    val retval = new Typechecker(classSymbolTable,functionSymbolTable)
    retval.typecheckProgram(myProgram, Map())
    retval
  }
} // Typechecker
import Typechecker.{SymbolTable, SymbolTableClass}


class Typechecker(val stc: SymbolTableClass, val stf:SymbolTable ){
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
      case PrintExp(e1) => {
        typeof(e1, gamma) match {
          case StrTypes => StrTypes
          case _ => throw IllTypedException("print")
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
      case MethodExp(e1, methodName, params) if stf contains methodName=> {
        val (tau1, tau2) = stf(methodName)
        if (typeof(e1,gamma)== StrTypes) {
          val className = e1.asInstanceOf[StringExp]
          if (stc contains className.value) {
            if (params.size != tau2.size) {
              throw IllTypedException("wrong number of params")
            } else {
              if (params.map(curE => typeof(curE, gamma)) != tau2) {
                throw IllTypedException("parameter type mismatch")
              } else {
                tau1
              }
            }
          }else {
            throw IllTypedException("class does not exist")
          }
        }
        else{
          throw IllTypedException("Class is not in string format")
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
    val gamma2 = typecheckStatement(methodDef.stmt, gamma1)
    if (typeof(methodDef.returnExpression, gamma2) != methodDef.types) {
      throw IllTypedException("return type mismatch")
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
        throw  IllTypedException("Cyclic inheritance on " + className)
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
    //    myClass match{
    // I was able to get rid of this case by making non extending classes extend ""
    //      case newClass: DefClass =>  {
    //        // if we get rid of the constructor method then we dont need to type check params or statement
    //        val gamma2 = newClass.instance.foldLeft(gamma)((res, cur) => typecheckInstance(cur, res))
    //        newClass.methods.foldLeft(gamma2)((res, cur) => typecheckMethodDef(cur, res))
    //      }
    //      case newExtendClass: DefExtClass =>{
    // make sure extended class is valid
    if (myClass.extendedClass != "" || stc.contains( myClass.extendedClass )){
      checkForDuplicatesMethods(myClass.methods)
      checkForDuplicatesInstanceVars(myClass.instances)
      instanceVarsDoNotOverride(className,List())
      myClass.methods.foreach(x => typecheckMethodDef(x,Map()))
    }else {
      throw IllTypedException("Extended Class Does not exist")
    }
  }




  def typecheckStatement(s: Stmt, gamma: TypeEnv): TypeEnv = {

  } // typecheckStatement

  def typecheckProgram(input: Program, gamma: TypeEnv) {
    typecheckClasses()
    typeof(input.entryPoint,gamma)
  } // typecheckProgram
} // Typechecker


/*
class Base {  int b;  void initializeBase(int x) { b = x; }}class Sub extends Base {  int s;  void initializeSub(int x, int y) {    initializeBase(x);    s = y;  }}
adding
 */



