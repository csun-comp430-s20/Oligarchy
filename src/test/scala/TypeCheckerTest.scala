import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

class TypeCheckerTest extends AnyFunSuite {

  val myMethod = MethodDef(BoolTypes, "myMethod", ExpStmt(IntegerExp(1)),
    List(VarDeclaration(IntTypes, "myString")), BooleanExp(false))
  val instanceVar = InstanceDec(VarDeclaration(IntTypes, "myInt"))
  val parameter = VarDeclaration(IntTypes, "myInt")
  val myStmt= AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(1))
  val singleClassList: List[Class] = List(DefClass("testing", null,
    BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
    List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
    List(VarDeclaration(BoolTypes, "myBool")),
    List(MethodDef(BoolTypes, "myMethod", ExpStmt(IntegerExp(1)), List(VarDeclaration(IntTypes, "myString")), BooleanExp(false)))
  ))
  val emptyClassList: List[Class] = List()
  val myEmptyProgram = Program(myStmt, emptyClassList)
  //serves to typecheck Empty Program
  val mynonEmptyTypechecker = Typechecker.apply(myEmptyProgram)

  //serves to typecheck Program with a Single DefClass)
  val emptyInstanceDecList: List[InstanceDec] = List()
  val emptyVarDeclarationList: List[VarDeclaration] = List()
  val emptyMethodDef: List[MethodDef] = List()
  val mySingleDefClass = DefClass("DefClassName", null,
    ExpStmt(IntegerExp(1)),
    emptyInstanceDecList,
    emptyVarDeclarationList,
    emptyMethodDef)

  val singleDefClassList: List[Class] = List(mySingleDefClass) //??
  val mySingleDefClassProgram = Program(myStmt, singleDefClassList)
  val mySingleDefClassTypechecker = Typechecker.apply(mySingleDefClassProgram)

  //serves to typecheck Program with a Single DefExtClass
  val mySingleDefExtClass = DefExtClass("DefClassName",
    "DefExtClassName",
    ExpStmt(IntegerExp(1)),
    emptyInstanceDecList,
    emptyVarDeclarationList,
    emptyMethodDef)
  val singleDefExtClassList: List[Class] = List(mySingleDefExtClass)
  val mySingleDefExtClassProgram = Program(myStmt, singleDefExtClassList)
  //  val mySingleDefExtTypechecker = Typechecker.apply(mySingleDefExtClassProgram)


  test("Makes sure an integer expression returns an IntTypes") {
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(IntegerExp(1), Map())
    assert(expected == received)
  }

  test("Makes sure an Boolean expression returns an booleanTypes") {
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(BooleanExp(true), Map())
    assert(expected == received)
  }
  test("Makes sure an And expression returns an BoolTypes") {
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(AndExp(BooleanExp(true), BooleanExp(false)), Map())
    assert(expected == received)
  }
  test("Make sure a 'Greater Than' expression takes returns a BoolTypes") {
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(GTExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }
  test("Make sure a 'Greater Than or Equals' expression takes returns a BoolTypes") {
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(GTEExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }
  test("Make sure a 'Less Than' expression takes returns a BoolTypes") {
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(LTExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }
  test("Make sure a 'Less Than or Equals' expression takes returns a BoolTypes") {
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(LTEExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }
  test("testing variable expression returns StrTypes") {
    //type TypeEnv = Map[String, Types]
    val gamma = Map("x" -> IntTypes)
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(VariableExp("x"), gamma)
    assert(expected == received)
  }

  test("testing subtraction expression returns IntTypes") {
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(SubtractExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }
  test("testing multiply expression returns IntTypes") {
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(MultiplyExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  test("testing division expression returns IntTypes") {
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(DivideExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  test("testing power expression returns IntTypes") {
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(PowerExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  test("testing equals expression returns BoolTypes for a boolean case") {
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(EqualsExp(BooleanExp(true), BooleanExp(true)), Map())
    assert(expected == received)
  }

  test("testing boolean expression returns BoolTypes for a integer case") {
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(EqualsExp(IntegerExp(2), IntegerExp(2)), Map())
    assert(expected == received)
  }
  test("testing assignment statement returns type") {
    val expected = Map("x" -> IntTypes)
    val received = mynonEmptyTypechecker.typecheckStatement(AssignmentStmt((VarDeclaration(IntTypes, "x")), IntegerExp(1)), Map("x" -> IntTypes))
    assert(expected == received)
  }
  test("testing assignment statement returns Ill Typed Exception") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typecheckStatement(AssignmentStmt((VarDeclaration(IntTypes, "x")), BooleanExp(false)), Map())
    }
  }
  test("testing var statement returns a type") {
    val expected = Map("i" -> IntTypes)
    val recieved = mynonEmptyTypechecker.typecheckStatement(VarStmt("i", IntegerExp(1)), Map("i" -> IntTypes))
    assert(recieved == expected)
  }
  test("testing var statement returns Ill Typed Exception") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typecheckStatement(VarStmt("x", IntegerExp(1)), Map("x" -> BoolTypes))
    }
  }
  test("testing conditional statements") {
    val expected = Map()
    val recieved = mynonEmptyTypechecker.typecheckStatement(ConditionalStmt(GTEExp(IntegerExp(10), IntegerExp(0)), AssignmentStmt(VarDeclaration(BoolTypes, "bool"), BooleanExp(false)), AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(1))), Map())
    assert(recieved == expected)
  }
  test("testing ill typed conditional statements") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typecheckStatement(ConditionalStmt(IntegerExp(10), VarStmt("bool", BooleanExp(false)), VarStmt("i", IntegerExp(0))), Map())
    }
  }
  test("testing block statements") {
    val expected = Map("bool" -> BoolTypes, "bool2" -> BoolTypes)
    val list: List[Stmt] = List(AssignmentStmt(VarDeclaration(BoolTypes, "bool"), BooleanExp(false)), AssignmentStmt(VarDeclaration(BoolTypes, "bool2"), BooleanExp(true)))
    val recieved = mynonEmptyTypechecker.typecheckStatement(BlockStmt(list), Map("bool" -> BoolTypes, "bool2" -> BoolTypes))
    assert(recieved == expected)
  }
  test("testing for loop statements") {
    val expected = Map("i"->IntTypes)
    val recieved = mynonEmptyTypechecker.typecheckStatement(ForStmt(AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(1)), GTEExp(IntegerExp(1), IntegerExp(10)), VarStmt("i", IntegerExp(1)), VarStmt("i", IntegerExp(1))), Map("i"->IntTypes))
    assert(recieved == expected)
  }

  test("testing for Statement returns ill typed") {
    assertThrows[IllTypedException] {
     mynonEmptyTypechecker.typecheckStatement(ForStmt(AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(1)), IntegerExp(0), VarStmt("i", IntegerExp(1)), VarStmt("i", IntegerExp(1))), Map("i"->IntTypes))
    }
  }
  test("testing Print statement returns type") {
    val expected = Map()
    val received = mynonEmptyTypechecker.typecheckStatement(PrintExp(IntegerExp(0)), Map())
    assert(expected == received)
  }

  test("testing plus expression") {
    val expected = IntTypes
    val recieved = mynonEmptyTypechecker.typeof(PlusExp(IntegerExp(5), IntegerExp(10)), Map())
    assert(recieved == expected)
  }
  test("testing plus expression returns ill typed") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(PlusExp(BooleanExp(false), IntegerExp(10)), Map())
    }
  }
  test("testing or expression returns ill typed") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(OrExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing GT expression returns ill typed") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(GTExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing GTE expression returns ill typed") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(GTEExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing LT expression returns ill typed") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(LTExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing LTE expression returns ill typed") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(LTEExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing and expression returns ill typed") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(AndExp(IntegerExp(0), BooleanExp(false)), Map())
    }
  }
  test("testing variable expression") {
    val expected = BoolTypes
    val recieved = mynonEmptyTypechecker.typeof(VariableExp("i"), Map("i" -> BoolTypes))
    assert(recieved == expected)
  }
  test("make symbol tables def class ext") {
    val child = DefExtClass("child",
      "parent",
      BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
      List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
      List(VarDeclaration(BoolTypes, "myBool")),
      List(MethodDef(BoolTypes, "myMethod", ExpStmt(IntegerExp(1)), List(VarDeclaration(IntTypes, "myInt")), BooleanExp(false)))
    )
    val parent = DefClass("parent", null,
      BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
      List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
      List(VarDeclaration(BoolTypes, "myBool")),
      List(MethodDef(BoolTypes, "myMethod", ExpStmt(IntegerExp(1)), List(VarDeclaration(IntTypes, "myInt")), BooleanExp(false)))
    )
    val extendedClassList: List[Class] = List(child, parent)
    val programThatExtendsAClass = Program(myStmt, extendedClassList)
    Typechecker(programThatExtendsAClass)

  }
  test("extended Class throws duplicate methods") {
    assertThrows[IllTypedException] {
      val duplicateMethod = MethodDef(BoolTypes, "myMethod", ExpStmt(IntegerExp(1)),
        List(VarDeclaration(IntTypes, "myInt")), BooleanExp(false))
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(duplicateMethod, duplicateMethod))
      val parent = DefClass("parent", null,
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(MethodDef(BoolTypes, "myMethod", ExpStmt(IntegerExp(1)), List(VarDeclaration(IntTypes, "myInt")), BooleanExp(false)))
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(myStmt, extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("extended Class throws duplicate instance variable") {
    assertThrows[IllTypedException] {
      val myMethod = MethodDef(BoolTypes, "myMethod", ExpStmt(IntegerExp(1)),
        List(VarDeclaration(IntTypes, "myInt")), BooleanExp(false))
      val duplicateInstanceVariable = instanceVar
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(duplicateInstanceVariable, duplicateInstanceVariable),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(myMethod))
      val parent = DefClass("parent", null,
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(MethodDef(BoolTypes, "myMethod2", ExpStmt(IntegerExp(1)), List(VarDeclaration(IntTypes, "myInt")), BooleanExp(false)))
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(myStmt, extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("extended Class throws duplicate constructor parameter") {
    assertThrows[IllTypedException] {
      val duplicateParameter = parameter
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(duplicateParameter, duplicateParameter),
        List(myMethod))
      val parent = DefClass("parent", null,
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(MethodDef(BoolTypes, "myMethod2", ExpStmt(IntegerExp(1)), List(VarDeclaration(IntTypes, "myInt")), BooleanExp(false)))
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(myStmt, extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }

  test("extended Class throws duplicate class name") {
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
      val parent = DefClass("parent", null,
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(MethodDef(BoolTypes, "myMethod2", ExpStmt(IntegerExp(1)), List(VarDeclaration(IntTypes, "myString")), BooleanExp(false)))
      )
      val extendedClassList: List[Class] = List(child, duplicateClass, parent)
      val programThatExtendsAClass = Program(myStmt, extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("Class throws duplicate class name") {
    assertThrows[IllTypedException] {
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod))
      val duplicateClass = DefClass("parent", null,
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod))
      val parent = DefClass("parent", null,
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(InstanceDec(VarDeclaration(IntTypes, "myInt"))),
        List(VarDeclaration(BoolTypes, "myBool")),
        List(MethodDef(BoolTypes, "myMethod2", ExpStmt(IntegerExp(1)), List(VarDeclaration(IntTypes, "myInt")), BooleanExp(false)))
      )
      val extendedClassList: List[Class] = List(child, duplicateClass, parent)
      val programThatExtendsAClass = Program(myStmt, extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("Class throws duplicate method ") {
    assertThrows[IllTypedException] {
      val duplicateMethod = myMethod
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod))
      val parent = DefClass("parent", null,
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(duplicateMethod, duplicateMethod)
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(myStmt, extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("Class throws duplicate parameter") {
    assertThrows[IllTypedException] {
      val duplicate = parameter
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod))
      val parent = DefClass("parent", null,
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(duplicate, duplicate),
        List(myMethod)
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(myStmt, extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("Class throws duplicate instance var ") {
    assertThrows[IllTypedException] {
      val duplicate = instanceVar
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod))
      val parent = DefClass("parent", null,
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(duplicate, duplicate),
        List(parameter),
        List(myMethod)
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(myStmt, extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
  }
  test("testing subtract expression returns Ill Typed Exception") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(SubtractExp(IntegerExp(1), BooleanExp(false)), Map())
    }
  }
  test("testing multiplication expression returns Ill Typed Exception") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(MultiplyExp(IntegerExp(1), BooleanExp(false)), Map())
    }
  }
  test("divide subtract expression returns Ill Typed Exception") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(DivideExp(IntegerExp(1), BooleanExp(false)), Map())
    }
  }
  test("testing power expression returns Ill Typed Exception") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(PowerExp(IntegerExp(1), BooleanExp(false)), Map())
    }
  }
  test("equals subtract expression returns Ill Typed Exception") {
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(EqualsExp(IntegerExp(1), BooleanExp(false)), Map())
    }
  }

  test("testing grouped expression returns IntTypes") {
    val expected = IntTypes
    val recieved = mynonEmptyTypechecker.typeof(GroupedExp(IntegerExp(1)), Map())
    assert(recieved == expected)
  }

  test("Check Type of new class Exp missing parameters") {
    val e = intercept[IllTypedException] {
      val entryPoint = AssignmentStmt(VarDeclaration(ClassTypes("parent"), "x"), NewClassExp("parent", List(
        IntegerExp(1),
        IntegerExp(2),
        // missing the string
      )))
      val myInt = VarDeclaration(IntTypes, "myInt")
      val mySecondInt = VarDeclaration(IntTypes, "mySecondInt")
      val myString = VarDeclaration(IntTypes, "myString")
      val testingClassParams = List(myInt, mySecondInt, myString)
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod))
      val parent = DefClass("parent", null,
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        testingClassParams,
        List(myMethod)
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(entryPoint, extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
    assert(e.msg == "Missing Parameters")
  }
//  test("Check Type of new class Exp parameters dont match") {
//    val e2 = intercept[IllTypedException] {
//      val entryPoint = AssignmentStmt(VarDeclaration(ClassTypes("parent"), "x"), NewClassExp("parent", List(
//        IntegerExp(1),
//        IntegerExp(2),
//        StringExp("hello") // 3 ints when want 2 ints and a string
//      )))
//      val myInt = VarDeclaration(IntTypes, "myInt")
//      val mySecondInt = VarDeclaration(IntTypes, "mySecondInt")
//      val myString = VarDeclaration(IntTypes, "myString")
//      val testingParameters = List(myInt, mySecondInt, myString)
//      val child = DefExtClass("child",
//        "parent",
//        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
//        List(instanceVar),
//        List(parameter),
//        List(myMethod))
//      val parent = DefClass("parent",null,
//        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
//        List(instanceVar),
//        testingParameters,
//        List(myMethod)
//      )
//      val extendedClassList: List[Class] = List(child, parent)
//      val programThatExtendsAClass = Program(entryPoint, extendedClassList)
//      Typechecker(programThatExtendsAClass)
//    }
//    assert("parameters for new class don't match" == e2.msg)
//  }
  test("Check Type of new class Exp class not defined") {
    val e3 = intercept[IllTypedException] {
      val entryPoint = AssignmentStmt(VarDeclaration(ClassTypes("parent"), "x"), NewClassExp("notDefined", List(
        IntegerExp(1),
        IntegerExp(2),
        IntegerExp(3) // 3 ints when want 2 ints and a string
      )))
      val myInt = VarDeclaration(IntTypes, "myInt")
      val mySecondInt = VarDeclaration(IntTypes, "mySecondInt")
      val myString = VarDeclaration(IntTypes, "myString")
      val testingClassParams = List(myInt, mySecondInt, myString)
      val child = DefExtClass("child",
        "parent",
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        List(parameter),
        List(myMethod))
      val parent = DefClass("parent", null,
        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
        List(instanceVar),
        testingClassParams,
        List(myMethod)
      )
      val extendedClassList: List[Class] = List(child, parent)
      val programThatExtendsAClass = Program(entryPoint, extendedClassList)
      Typechecker(programThatExtendsAClass)
    }
    assert(e3.msg == "class not defined")
  }
  //@ToDo add test for high order expression and call high order expression
//  test("testing higher order expression reeturns method types") {
//    val expected = MethodTypes(List( IntTypes), IntTypes)
//    val list: List[VarDeclaration] = List(VarDeclaration(BoolTypes, "bool"), VarDeclaration(IntTypes, "intType"))
//    val mystmt = BlockStmt(List(
//      AssignmentStmt(VarDeclaration(IntTypes,"x"),HighOrderExp( "test1", ClassTypes("test2"), IntTypes, IntegerExp(10))),
//      ExpStmt(CallHighOrderExp(VariableExp("x"), ClassTypes("test2"), IntegerExp(2))),
//    ))
//    mynonEmptyTypechecker.typecheckStatement(mystmt,Map())
//  }
//  test("testing call high order expression returns a type") {
//    val expected = IntTypes
//    val recieved = mynonEmptyTypechecker.typeof(CallHighOrderExp(VariableExp("foo"), ClassTypes("className"), VariableExp("i")), Map("foo" ->MethodTypes(List(ClassTypes("className")),ClassTypes("className"))))
//    assert(recieved == expected)
//  }
//  test("testing call high order expression returns an ill typed for other exp") {
//    assertThrows[IllTypedException] {
//      mynonEmptyTypechecker.typeof(CallHighOrderExp((HighOrderExp(List(VarDeclaration(IntTypes, "i")), VariableExp("x"))), List(IntegerExp(0))), Map())
//    }
//  }
//  test("testing call high order expression returns an ill typed for not a higher-order function") {
//    assertThrows[IllTypedException] {
//      mynonEmptyTypechecker.typeof(CallHighOrderExp(HighOrderExp(List(VarDeclaration(IntTypes, "i")), StringExp("i")), List(StringExp("i"))), Map())
//    }
//  }
//  test("testing call high order expression returns an ill typed for not a parameter type mismatch") {
//    assertThrows[IllTypedException] {
//      mynonEmptyTypechecker.typeof(CallHighOrderExp(IntegerExp(1), List(StringExp("i"))), Map())
//    }
//  }

  test("MethodExp throws Class is not in string format"){
    assertThrows[IllTypedException] {
      mynonEmptyTypechecker.typeof(MethodExp(IntegerExp(0), null, "foo", List(IntegerExp(1))), Map())
    }
  }


  test("MethodExp throws trying to call a method on a non variable") {
    val e= intercept[IllTypedException] {
      mynonEmptyTypechecker.typeof(MethodExp(IntegerExp(2),null, "irrelavent", List(StringExp("i"))), Map())
    }
    assert("trying to call a method on a non variable" == e.msg )
  }
//  test("MethodExp throws Class name not found") {
//    val e= intercept[IllTypedException] {
//      mynonEmptyTypechecker.typeof(MethodExp(VariableExp(null),"testing2", "irrelavent", List(StringExp("i"))), Map())
//    }
//    assert("Class name not found" == e.msg )
//  }
//  test("MethodExp throws no Methods were defined") {
//    val e= intercept[IllTypedException] {
//      mySingleDefClassTypechecker.typeof(MethodExp(VariableExp("DefClassName"),"className", "irrelavent", List(StringExp("i"))), Map())
//    }
//    assert("no Methods were defined" == e.msg )
//  }
//  test("MethodExp throws Method Name not found") {
//    val e3 = intercept[IllTypedException] {
//      val entryPoint = MethodExp(VariableExp("myClass"), null, "cant find", List(
//        IntegerExp(1),
//        IntegerExp(2),
//        IntegerExp(3) // 3 ints when want 2 ints and a string
//      ))
//      val myInt = VarDeclaration(IntTypes, "myInt")
//      val mySecondInt = VarDeclaration(IntTypes, "mySecondInt")
//      val myString = VarDeclaration(IntTypes, "myString")
//      val testingClassParams = List(myInt, mySecondInt, myString)
//      val parent = DefClass("myClass", null,
//        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
//        List(instanceVar),
//        testingClassParams,
//        List(myMethod)
//      )
//      val extendedClassList: List[Class] = List(parent)
//      val programThatExtendsAClass = Program(myStmt, extendedClassList)
//      Typechecker(programThatExtendsAClass)
//    }
//    assert(e3.msg == "Method Name not found")
//  }
//  test("MethodExp throws parameter type mismatch") {
//    val e3 = intercept[IllTypedException] {
//      val entryPoint = MethodExp(VariableExp("myClass"),null, "myMethod", List(
//        IntegerExp(1) // passing an int when want a string
//      ))
//      val myInt = VarDeclaration(IntTypes, "myInt")
//      val mySecondInt = VarDeclaration(IntTypes, "mySecondInt")
//      val myString = VarDeclaration(IntTypes, "myString")
//      val testingClassParams = List(myInt, mySecondInt, myString)
//      val parent = DefClass("myClass", null,
//        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
//        List(instanceVar),
//        testingClassParams,
//        List(myMethod)
//      )
//      val extendedClassList: List[Class] = List(parent)
//      val programThatExtendsAClass = Program(myStmt, extendedClassList)
//      Typechecker(programThatExtendsAClass)
//    }
//    assert(e3.msg == "parameter type mismatch")
//  }
//  test("MethodExp throws wrong number of params") {
//    val e3 = intercept[IllTypedException] {
//      val entryPoint = MethodExp(VariableExp("myClass"),null, "myMethod", List(
//        IntegerExp(1), // passing an int when want a string
//        IntegerExp(1) // passing an int when want a string
//      ))
//      val myInt = VarDeclaration(IntTypes, "myInt")
//      val mySecondInt = VarDeclaration(IntTypes, "mySecondInt")
//      val myString = VarDeclaration(IntTypes, "myString")
//      val testingClassParams = List(myInt, mySecondInt, myString)
//      val parent = DefClass("myClass", null,
//        BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
//        List(instanceVar),
//        testingClassParams,
//        List(myMethod)
//      )
//      val extendedClassList: List[Class] = List(parent)
//      val programThatExtendsAClass = Program(myStmt, extendedClassList)
//      Typechecker(programThatExtendsAClass)
//    }
//    assert(e3.msg == "wrong number of params")
//  }
//  test("MethodExp succeeds") {
//
//    val start = MethodExp(VariableExp("myClass"),"nameClass", "myMethod", List(
//      VariableExp("myClass2"), // passing an int when want a string
//      VariableExp("myClass3") // passing an int when want a string
//    ))
//    val myInt = VarDeclaration(IntTypes, "myInt")
//    val mySecondInt = VarDeclaration(IntTypes, "mySecondInt")
//    val myString = VarDeclaration(IntTypes, "myString")
//    val testingClassParams = List(myInt, mySecondInt, myString)
//    val parent = DefClass("myClass", "nameClass",
//      BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
//      List(instanceVar),
//      testingClassParams,
//      List(myMethod)
//    )
//    val extendedClassList: List[Class] = List(parent)
//    val programThatExtendsAClass = Program(myStmt, extendedClassList)
//    Typechecker(programThatExtendsAClass)
//  }

}


