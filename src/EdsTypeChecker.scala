package src
case class IllTypedException(msg: String) extends Exception(msg)


object Typechecker {
  type SymbolTable = Map[String, (Types, List[Types])]
  type SymbolTableClass = Map[String, List[Types]]

  def makeSymbolTables(myClass: List[Class], stClass: SymbolTableClass, stFunctions: SymbolTable): (SymbolTableClass, SymbolTable )= {
    myClass match {
      case (_:DefClass)::_ =>
        val newClassSymbolTable = makeSymbolTableDefClassHelper(myClass,stClass)
        val newMethodSymbolTable = myClass.foldLeft(stFunctions)((res,cur) => makeSymbolTable(cur,res))
        (newClassSymbolTable,newMethodSymbolTable)
      case (_:DefExtClass)::_ =>
        val newClassSymbolTable = makeSymbolTableClassExtHelper(myClass,stClass)
        val newMethodSymbolTable = myClass.foldLeft(stFunctions)((res,cur) => makeSymbolTable(cur,res))
        (newClassSymbolTable,newMethodSymbolTable)
    }

  }

  def makeSymbolTableClassExtHelper(myClass: List[Class], symbolTableClass: SymbolTableClass): SymbolTableClass ={
    myClass.foldLeft(symbolTableClass)((res, cur) => {
      val DefExtClass(classname, extendedClass, statements, instances,parameters,methods) = cur
      if (res.contains(classname)) {
        throw IllTypedException("duplicate Class name: " + classname)
      }
      val paramNames = instances.map(_.v1.varName).toSet
      if (paramNames.size != parameters.size) {
        throw IllTypedException("duplicate instance variable")
      }
      res + (classname -> instances.map(_.v1.types))
    })

  }

  def makeSymbolTableDefClassHelper(myClass: List[Class], symbolTableClass: SymbolTableClass): SymbolTableClass={
    myClass.foldLeft(symbolTableClass)((res, cur) => {
      val DefClass(classname,
      _,
      instances,
      _,
      _) = cur
      if (res.contains(classname)) {
        throw IllTypedException("duplicate Class name: " + classname)
      }
      val paramNames = instances.map(_.v1.varName).toSet
      if (paramNames.size != instances.size) {
        throw IllTypedException("duplicate instance variable")
      }
      res + (classname -> instances.map(_.v1.types))
    })
  }

  def makeSymbolTable(myClass: Class, symbolTable: SymbolTable): SymbolTable = {
    myClass match{
      case newClass: DefClass =>  {
        val newTable =  makeSymbolTableHelper(newClass.methods)
        newTable
      }
      case newExtendClass: DefExtClass =>{
        makeSymbolTableHelper(newExtendClass.methods)
      }
    }
  }

  def makeSymbolTableHelper(methods: List[MethodDef]): SymbolTable ={
    methods.foldLeft(Map(): SymbolTable)((res, cur) => {
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

  def typecheckInstance(dec: InstanceDec, env: TypeEnv): TypeEnv={
      // we dont need this since these are just variable declarations and not instantiations
  }

  def typecheckMethodDef(methodDef: MethodDef, env: TypeEnv): TypeEnv ={
    val gamma1 = env ++ methodDef.parameters.map(pair => (pair.varName -> pair.types)).toMap
    val gamma2 = typecheckStatement(methodDef.stmt, gamma1)
    if (typeof(methodDef.returnExpression, gamma2) != methodDef.types) {
      throw IllTypedException("return type mismatch")
    }else{
      env
    }
  }
  def typecheckClasses(myClass: Class , gamma: TypeEnv): TypeEnv ={
    myClass match{
      case newClass: DefClass =>  {
        // if we get rid of the constructor method then we dont need to type check params or statement
        val gamma2 = newClass.instance.foldLeft(gamma)((res, cur) => typecheckInstance(cur, res))
        newClass.methods.foldLeft(gamma2)((res, cur) => typecheckMethodDef(cur, res))
    }
      case newExtendClass: DefExtClass =>{
        if (stc contains newExtendClass.extendedClass ){
          val gamma2 = newExtendClass.instances.foldLeft(gamma)((res, cur) => typecheckInstance(cur, res))
          newExtendClass.methods.foldLeft(gamma2)((res, cur) => typecheckMethodDef(cur, res))
        }else {
          throw IllTypedException("Extended Class Does not exist")
        }
      }
    }
  }


  def typecheckStatement(s: Stmt, gamma: TypeEnv): TypeEnv = {

  } // typecheckStatement

  def typecheckProgram(input: Program, gamma: TypeEnv) {
    input.classes.foreach(myClass=> typecheckClasses(myClass,gamma))
    typeof(input.entryPoint,gamma)
  } // typecheckProgram
} // Typechecker