package src
class TypeCheckerTest {

  //val something = new Typechecker()
  //val myInstanceDec = ;
  //val myVarDeclaration = ;
  //val myMethodDef = MethodDef()
  //val myClass = DefClass("ClassName", ExpStmt(IntegerExp(2)), )

  val myExpression = IntegerExp(1)
  val emptyClassList: List[Class] = List()
  val myEmptyProgram = Program(myExpression, emptyClassList)
  //serves to typecheck Empty Program
  val myEmptyTypechecker = Typechecker.apply(myEmptyProgram)

  //serves to typecheck Program with a Single DefClass
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
  val mySingleDefExtClass = DefExtClass("DefExtClassName",
                                        "DefClassName",
                                        ExpStmt(IntegerExp(1)),
                                        emptyInstanceDecList,
                                        emptyVarDeclarationList,
                                        emptyMethodDef)
  val singleDefExtClassList: List[Class] = List(mySingleDefExtClass)
  val mySingleDefExtClassProgram = Program(myExpression, singleDefExtClassList)
  val mySingleDefExtTypechecker = Typechecker.apply(mySingleDefExtClassProgram)


  def testIntegerExp(): Unit ={
    val expected = IntTypes
    val received = myEmptyTypechecker.typeof(IntegerExp(1), Map())
    assert(expected == received)
  }

  def testVariableExp(): Unit ={
    //type TypeEnv = Map[String, Types]
    val gamma = Map("x" -> StrTypes)
    val expected = StrTypes
    val received = myEmptyTypechecker.typeof(VariableExp("x"), gamma)
    assert(expected == received)
  }

  def testSubtractExp(): Unit ={
    val expected = IntTypes
    val received = myEmptyTypechecker.typeof(SubtractExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testMultiplyExp(): Unit ={
    val expected = IntTypes
    val received = myEmptyTypechecker.typeof(MultiplyExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testDivideExp(): Unit ={
    val expected = IntTypes
    val received = myEmptyTypechecker.typeof(DivideExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testPowerExp(): Unit ={
    val expected = IntTypes
    val received = myEmptyTypechecker.typeof(PowerExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testStrEqualsExp(): Unit ={
    val expected = BoolTypes
    val received = myEmptyTypechecker.typeof(EqualsExp(StringExp("x"), StringExp("x")), Map())
    assert(expected == received)
  }

  def testIntEqualsExp(): Unit ={
    val expected = BoolTypes
    val received = myEmptyTypechecker.typeof(EqualsExp(IntegerExp(2), IntegerExp(2)), Map())
    assert(expected == received)
  }


}
