package src
case class IllTypedException(msg: String) extends Exception(msg)


object Typechecker {
  type SymbolTable = Map[String, (Types, List[Types])]

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
    var test: SymbolTable = Map()
    myProgram.myClasses.foreach(x => {test = x :: makeSymbolTable})
    myClass.foreach makeMethodSymbolTable(myClass)
    val retval = new Typechecker(makeSymbolTable(myClass))
    retval.typecheckClass(myClass)
    retval
  }
} // Typechecker
import Typechecker.SymbolTable


class Typechecker(val st: SymbolTable){
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
//      case MethodExp(e1, methodName, e2) => {
//        //        (typeof(e1, gamma), typeof(e2, gamma)) match {
//        //          case (IntTypes, IntTypes) => BoolTypes
//        //          case _ => throw IllTypedException("less than")
//        //        }
//      }
//
//      case HighOrderExp(t1, v1, e2) =>{
//        t1
//      }
//      case CallHighOrderExp(e1,e2) =>{
//
//      }

      case _ => throw IllTypedException("other-exp")
    }
  } // typeof

  def typecheckStatement(s: Stmt, gamma: TypeEnv): TypeEnv = {

  } // typecheckStatement

  def typecheckProgram(input: Program, gamma: TypeEnv): TypeEnv = {

  } // typecheckProgram
} // Typechecker