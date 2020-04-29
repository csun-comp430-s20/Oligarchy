import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

class TypeCheckerTest extends AnyFunSuite {

  val myMethod = MethodDef(BoolTypes, "myMethod", ExpStmt(IntegerExp(1)),
    List(VarDeclaration(StrTypes, "myString")), BooleanExp(false))
  val instanceVar = InstanceDec(VarDeclaration(IntTypes, "myInt"))
  val parameter = VarDeclaration(IntTypes, "myInt")
  val myExpression = IntegerExp(1)
  val singleClassList: List[Class] = List(DefClass("testing",
    BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
    List(InstanceDec(VarDeclaration(IntTypes,"myInt"))),
    List(VarDeclaration(BoolTypes,"myBool")),
    List(MethodDef(BoolTypes,"myMethod",ExpStmt(IntegerExp(1) ),List(VarDeclaration(StrTypes,"myString")), BooleanExp(false)))
  ))
  val emptyClassList: List[Class] = List()
  val myEmptyProgram = Program(myExpression, emptyClassList)
  //serves to typecheck Empty Program
  val mynonEmptyTypechecker = Typechecker.apply(myEmptyProgram)

  //serves to typecheck Program with a Single DefClass)
  val emptyInstanceDecList: List[InstanceDec] = List()
  val emptyVarDeclarationList: List[VarDeclaration] = List()
  val emptyMethodDef: List[MethodDef] = List()
  val mySingleDefClass = DefClass("DefClassName",
    ExpStmt(IntegerExp(1)),
    emptyInstanceDecList,
    emptyVarDeclarationList,
    emptyMethodDef)

  val singleDefClassList: List[Class] = List(mySingleDefClass)  //??
  val mySingleDefClassProgram = Program(myExpression, singleDefClassList)
  val mySingleDefClassTypechecker = Typechecker.apply(mySingleDefClassProgram)

  //serves to typecheck Program with a Single DefExtClass
  val mySingleDefExtClass = DefExtClass("DefClassName",
    "DefExtClassName",
    ExpStmt(IntegerExp(1)),
    emptyInstanceDecList,
    emptyVarDeclarationList,
    emptyMethodDef)
  val singleDefExtClassList: List[Class] = List(mySingleDefExtClass)
  val mySingleDefExtClassProgram = Program(myExpression, singleDefExtClassList)
//  val mySingleDefExtTypechecker = Typechecker.apply(mySingleDefExtClassProgram)


  test("Makes sure an integer expression returns an IntTypes") {
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(IntegerExp(1), Map())
    assert(expected == received)
  }
  test("Makes sure an string expression returns an IntTypes") {
    val expected = StrTypes
    val received = mynonEmptyTypechecker.typeof(StringExp("string"), Map())
    assert(expected == received)
  }
  test("Makes sure an Boolean expression returns an booleanTypes") {
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(BooleanExp(true), Map())
    assert(expected == received)
  }
  test("Makes sure an And expression returns an BoolTypes") {
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(AndExp(BooleanExp(true),BooleanExp(false)), Map())
    assert(expected == received)
  }
  test("Make sure a 'Greater Than' expression takes returns a BoolTypes"){
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(GTExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }
  test("Make sure a 'Greater Than or Equals' expression takes returns a BoolTypes"){
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(GTEExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }
  test("Make sure a 'Less Than' expression takes returns a BoolTypes"){
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(LTExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }
  test("Make sure a 'Less Than or Equals' expression takes returns a BoolTypes"){
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(LTEExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }
  test ("testTypeofVariableExp()"){
    //type TypeEnv = Map[String, Types]
    val gamma = Map("x" -> StrTypes)
    val expected = StrTypes
    val received = mynonEmptyTypechecker.typeof(VariableExp("x"), gamma)
    assert(expected == received)
  }

  test("TypeofSubtractExp()"){
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(SubtractExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  test("TypeofMultiplyExp()"){
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(MultiplyExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  test("TypeofDivideExp()"){
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(DivideExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  test("TypeofPowerExp()"){
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(PowerExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  test("TypeofStrEqualsExp()"){
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(EqualsExp(StringExp("x"), StringExp("x")), Map())
    assert(expected == received)
  }
  test("TypeofBoolEqualsExp()"){
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(EqualsExp(BooleanExp(true), BooleanExp(true)), Map())
    assert(expected == received)
  }

  test("TypeofIntEqualsExp()"){
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(EqualsExp(IntegerExp(2), IntegerExp(2)), Map())
    assert(expected == received)
  }
  test("testing assignment statement returns type"){
    val expected = Map("x" -> IntTypes)
    val received = mynonEmptyTypechecker.typecheckStatement(AssignmentStmt((VarDeclaration(IntTypes, "x")), IntegerExp(1)), Map("x" -> IntTypes), false)
    assert(expected == received)
  }
  test("testing assignment statement returns Ill Typed Exception") {
    assertThrows[IllTypedException] {
     val received =  mynonEmptyTypechecker.typecheckStatement(AssignmentStmt((VarDeclaration(IntTypes, "x")), BooleanExp(false)), Map(), false)
    }
  }
  test("testing var statement returns a type"){
    val expected = Map( "i"-> IntTypes)
    val recieved = mynonEmptyTypechecker.typecheckStatement(VarStmt("i" , IntegerExp(1)), Map( "i" -> IntTypes), false)
    assert(recieved == expected)
  }
  test("testing var statement returns Ill Typed Exception"){
    assertThrows[IllTypedException]{
      mynonEmptyTypechecker.typecheckStatement(VarStmt( "x", IntegerExp(1)), Map("x" -> StrTypes), false)
    }
  }
  test("testing break statement outside of for loop"){
    assertThrows[IllTypedException]{
      mynonEmptyTypechecker.typecheckStatement(BreakStmt, Map(),false)
    }
  }
  test("testing for loop statement with break statement"){
      val expected = Map()
      val recieved = mynonEmptyTypechecker.typecheckStatement(ForStmt(AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(1)), LTEExp(IntegerExp(10), IntegerExp(30)), VarStmt("i", IntegerExp(2)), BreakStmt), Map(), false)
      assert(recieved == expected)
  }
  test("testing ill typed for loop statement with break statement"){
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typecheckStatement(ForStmt(AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(1)), IntegerExp(0), VarStmt("i", IntegerExp(2)), BreakStmt), Map(), false)
    }
  }
  test("testing conditional statements"){
    val expected = Map()
    val recieved = mynonEmptyTypechecker.typecheckStatement(ConditionalStmt(GTEExp(IntegerExp(10), IntegerExp(0)), AssignmentStmt(VarDeclaration(BoolTypes,"bool"), BooleanExp(false)), AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(1))) , Map(), false)
    assert(recieved == expected)
  }
  test("testing ill typed conditional statements"){
    assertThrows[IllTypedException] {
     mynonEmptyTypechecker.typecheckStatement(ConditionalStmt(IntegerExp(10), VarStmt("bool", BooleanExp(false)), VarStmt("i", IntegerExp(0))), Map(), false)
    }
  }
  test("testing block statements"){
    val expected = Map("bool" -> BoolTypes, "bool2" -> BoolTypes)
    val list:List[Stmt] = List(AssignmentStmt(VarDeclaration(BoolTypes,"bool"), BooleanExp(false)),  AssignmentStmt(VarDeclaration(BoolTypes,"bool2"), BooleanExp(true)))
    val recieved = mynonEmptyTypechecker.typecheckStatement(BlockStmt(list), Map("bool" -> BoolTypes, "bool2" -> BoolTypes), false)
    assert(recieved == expected)
  }
  test("testing print expression"){
    val expected = StrTypes
     val recieved = mynonEmptyTypechecker.typeof(PrintExp(StringExp("printing")), Map())
    assert(recieved == expected)
  }
  test("testing print expression returns ill typed"){
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(PrintExp(IntegerExp(0)), Map())
    }
  }
  test("testing plus expression"){
    val expected = IntTypes
    val recieved = mynonEmptyTypechecker.typeof(PlusExp(IntegerExp(5), IntegerExp(10)), Map())
    assert(recieved == expected)
  }
  test("testing plus expression returns ill typed"){
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(PlusExp(BooleanExp(false), IntegerExp(10)), Map())
    }
  }
  test("testing or expression returns ill typed"){
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(OrExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing GT expression returns ill typed"){
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(GTExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing GTE expression returns ill typed"){
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(GTEExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing LT expression returns ill typed"){
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(LTExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing LTE expression returns ill typed"){
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(LTEExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing and expression returns ill typed"){
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(AndExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing variable expression"){
    val expected = StrTypes
    val recieved = mynonEmptyTypechecker.typeof(VariableExp("i"), Map("i"-> StrTypes))
    assert(recieved == expected)
  }
  test("make symbol tables def class ext"){
    val child = DefExtClass("child",
      "parent",
      BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
      List(InstanceDec(VarDeclaration(IntTypes,"myInt"))),
      List(VarDeclaration(BoolTypes,"myBool")),
      List(MethodDef(BoolTypes,"myMethod",ExpStmt(IntegerExp(1) ),List(VarDeclaration(StrTypes,"myString")), BooleanExp(false)))
    )
    val parent = DefClass("parent",
      BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
      List(InstanceDec(VarDeclaration(IntTypes,"myInt"))),
      List(VarDeclaration(BoolTypes,"myBool")),
      List(MethodDef(BoolTypes,"myMethod",ExpStmt(IntegerExp(1) ),List(VarDeclaration(StrTypes,"myString")), BooleanExp(false)))
    )
    val extendedClassList: List[Class] = List(child, parent)
    val programThatExtendsAClass = Program(IntegerExp(1),extendedClassList)
    Typechecker(programThatExtendsAClass)

  }
  test("extended Class throws duplicate methods"){
    assertThrows[IllTypedException] {
      val duplicateMethod = MethodDef(BoolTypes, "myMethod", ExpStmt(IntegerExp(1)),
        List(VarDeclaration(StrTypes, "myString")), BooleanExp(false))
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(duplicateMethod, duplicateMethod))
      val parent = DefClass("parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(MethodDef(BoolTypes, "myMethod", ExpStmt(IntegerExp(1)), List(VarDeclaration(StrTypes, "myString")), BooleanExp(false)))
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(IntegerExp(1), extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("extended Class throws duplicate instance variable"){
    assertThrows[IllTypedException] {
      val myMethod = MethodDef(BoolTypes, "myMethod", ExpStmt(IntegerExp(1)),
        List(VarDeclaration(StrTypes, "myString")), BooleanExp(false))
      val duplicateInstanceVariable = instanceVar
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(duplicateInstanceVariable,duplicateInstanceVariable),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(myMethod))
      val parent = DefClass("parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(MethodDef(BoolTypes, "myMethod2", ExpStmt(IntegerExp(1)), List(VarDeclaration(StrTypes, "myString")), BooleanExp(false)))
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(IntegerExp(1), extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("extended Class throws duplicate constructor parameter"){
    assertThrows[IllTypedException] {
      val duplicateParameter = parameter
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(duplicateParameter,duplicateParameter),
        List(myMethod))
      val parent = DefClass("parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(MethodDef(BoolTypes, "myMethod2", ExpStmt(IntegerExp(1)), List(VarDeclaration(StrTypes, "myString")), BooleanExp(false)))
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(IntegerExp(1), extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }

  test("extended Class throws duplicate class name"){
    assertThrows[IllTypedException] {
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod))
      val duplicateClass = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod))
      val parent = DefClass("parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(MethodDef(BoolTypes, "myMethod2", ExpStmt(IntegerExp(1)), List(VarDeclaration(StrTypes, "myString")), BooleanExp(false)))
      )
      val extendedClassList: List[Class] = List(child,duplicateClass, parent)
      val programThatExtendsAClass = Program(IntegerExp(1), extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("Class throws duplicate method "){
    assertThrows[IllTypedException] {
      val duplicateMethod = myMethod
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod))
      val parent = DefClass("parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(duplicateMethod,duplicateMethod)
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(IntegerExp(1), extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("Class throws duplicate parameter"){
    assertThrows[IllTypedException] {
      val duplicate = parameter
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod))
      val parent = DefClass("parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(duplicate , duplicate),
        List(myMethod)
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(IntegerExp(1), extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("Class throws duplicate instance var "){
    assertThrows[IllTypedException] {
      val duplicate = instanceVar
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(duplicate, duplicate),
        List(parameter),
        List(myMethod))
      val parent = DefClass("parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod)
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(IntegerExp(1), extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }

}
