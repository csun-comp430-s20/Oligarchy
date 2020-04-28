package src

object TypeCheckerTest {

//  def testAssigmentStatement(): Unit = {
////   `type TypeEnv  = Map(x -> BoolTypes)
////    type SymboleTableClass = Map[String, DefExtClass ]
//    val prgm = Program(BooleanExp(true), List())
//    val typechecker = Typechecker(prgm)
//    val expected = BoolTypes
//    val received = typechecker.typeof(BooleanExp(true), Map())
//    assert(expected == received)
//
//  }
  val myExpression = IntegerExp(1)
  val emptyClassList: List[Class] = List()
  val myProgram = Program(myExpression, emptyClassList)
  val myTypechecker = Typechecker.apply(myProgram)
  def testIntegerExp(): Unit ={
    val expected = IntTypes
    val received = myTypechecker.typeof(IntegerExp(1), Map())
    assert(expected == received)
  }

  def testAssingmentStatementTrue(): Unit ={
    val expected = Map("x" -> IntTypes)
    val received = myTypechecker.typecheckStatement(AssignmentStmt((VarDeclaration(IntTypes, "x")), IntegerExp(1)), Map(), false)
    assert(expected == received)
  }

  def testAssingmentStatementFalse(): Unit ={
    val expected = Map("x" -> StrTypes)
    val received = myTypechecker.typecheckStatement(AssignmentStmt((VarDeclaration(IntTypes, "x")), IntegerExp(1)), Map(), false)
    assert(expected != received)
  }

  def testPlusExp(): Unit ={
    val expected = IntTypes
    val received = myTypechecker.typeof(PlusExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testOrExp(): Unit ={
    val expected = BoolTypes
    val received = myTypechecker.typeof(OrExp(BooleanExp(true), BooleanExp(false)), Map())
    assert(expected == received)
  }

  def testAndExp(): Unit ={
    val expected = BoolTypes
    val received = myTypechecker.typeof(AndExp(BooleanExp(true), BooleanExp(false)), Map())
    assert(expected == received)
  }

  def testGTExp(): Unit ={
    val expected = BoolTypes
    val received = myTypechecker.typeof(GTExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testGTEExp(): Unit ={
    val expected = BoolTypes
    val received = myTypechecker.typeof(GTEExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testLTExp(): Unit ={
    val expected = BoolTypes
    val received = myTypechecker.typeof(LTExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }

  def testLTEExp(): Unit ={
    val expected = BoolTypes
    val received = myTypechecker.typeof(LTEExp(IntegerExp(2), IntegerExp(1)), Map())
    assert(expected == received)
  }
  
  def main(args: Array[String]): Unit = {
    testIntegerExp()
    testAssingmentStatementTrue()
    testAssingmentStatementFalse()
    testPlusExp()
    testOrExp()
    testAndExp()
    testGTExp()
    testGTEExp()
    testLTExp()
    testLTEExp()
  }

}
