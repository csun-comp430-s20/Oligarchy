import org.scalatest.funsuite.AnyFunSuite

class TypeCheckerTest extends AnyFunSuite {
  //val something = new Typechecker()
  //val myInstanceDec = ;
  //val myVarDeclaration = ;
  //val myMethodDef = MethodDef()
  //val myClass = DefClass("ClassName", ExpStmt(IntegerExp(2)), )

  val myExpression = IntegerExp(1)
  val emptyClassList: List[Class] = List(DefClass("testing",
    BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
    List(InstanceDec(VarDeclaration(IntTypes,"myInt"))),
    List(VarDeclaration(BoolTypes,"myBool")),
    List(MethodDef(BoolTypes,"myMethod",ExpStmt(IntegerExp(1) ),List(VarDeclaration(StrTypes,"myString")), BooleanExp(false)))
  ))
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
  def testVariableExp(): Unit ={
    //type TypeEnv = Map[String, Types]
    val gamma = Map("x" -> StrTypes)
    val expected = StrTypes
    val received = mynonEmptyTypechecker.typeof(VariableExp("x"), gamma)
    assert(expected == received)
  }

  def testSubtractExp(): Unit ={
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(SubtractExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testMultiplyExp(): Unit ={
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(MultiplyExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testDivideExp(): Unit ={
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(DivideExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testPowerExp(): Unit ={
    val expected = IntTypes
    val received = mynonEmptyTypechecker.typeof(PowerExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testStrEqualsExp(): Unit ={
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(EqualsExp(StringExp("x"), StringExp("x")), Map())
    assert(expected == received)
  }

  def testIntEqualsExp(): Unit ={
    val expected = BoolTypes
    val received = mynonEmptyTypechecker.typeof(EqualsExp(IntegerExp(2), IntegerExp(2)), Map())
    assert(expected == received)
  }
}
