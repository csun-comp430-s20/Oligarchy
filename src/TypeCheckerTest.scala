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

  def main(args: Array[String]): Unit = {
    testIntegerExp()
    testAssingmentStatementTrue()
    testAssingmentStatementFalse()
  }

}
