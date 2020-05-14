import org.scalatest.funsuite.AnyFunSuite
class ParserTest extends AnyFunSuite {
  def testParses(input: List[Token], expectedProgram: Program ) {
    val parser = Parser(input)
    val (received, _) = parser.parseProgram(input)
    assert(received == expectedProgram)
  }

  def testParses[A](input: List[Token], expectedProgram: A,  parseOne: List[Token] => (A, List[Token])) {
    val (received, _) = parseOne(input)
    assert(received == expectedProgram)
  }

//@todo this errors out please fix
//  test("testClass()"){
//    val input = "Class testing { int myInt;" +
//      "constructor(bool myBool){1;}" +
//      "bool testMethod() return true;" +
//      "}"
//    val tokenizer = Lexer(input)
//    val receivedTokens = tokenizer.tokenize()
//    val program = Program(IntegerExp(1),
//      List(
//        DefClass("testing",
//          BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
//          List(InstanceDec(VarDeclaration(IntTypes,"myInt"))),
//          List(VarDeclaration(BoolTypes,"myBool")),
//          List(MethodDef(BoolTypes, "testMethod",null, List[VarDeclaration](), BooleanExp(true)))
//        )
//      )
//    )
//    testParses(receivedTokens,program)
//  }

  test("testIntegerExp()"){
    val lexerInput = "35"
    val lexer = Lexer(lexerInput)
    val receivedTokens = lexer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = IntegerExp(35)
    testParses(receivedTokens,expected, parser.parseExp)
  }

  test("testBooleanExp()"){
    val lexerInput = "true"
    val lexer = Lexer(lexerInput)
    val receivedTokens = lexer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = BooleanExp(true)
    testParses(receivedTokens,expected, parser.parseExp)
  }
  test("testStringExp()"){
    val lexerInput = "\"testString\""
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = StringExp("testString")
    testParses(receivedTokens,expected, parser.parseExp)
  }
  test("testVariableExp()"){
    val lexerInput = "variableCheck"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = VariableExp("variableCheck")
    testParses(receivedTokens,expected, parser.parseExp)
  }
  test("testSimpleGroupedExp()"){
    val lexerInput = "(14)"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = GroupedExp(IntegerExp(14))
    testParses(receivedTokens,expected, parser.parseExp)
  }
  test("testing SimpleGroupedExp throws missing  grouped expression"){
    assertThrows[ParserException] {
      val lexerInput = "(14"
      val tokenizer = Lexer(lexerInput)
      val receivedTokens = tokenizer.tokenize()
      val parser = Parser(receivedTokens)
      parser.parseExp(receivedTokens)
    }
  }
  test("testPowerExp()"){
    val lexerInput = "2^3"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = PowerExp(IntegerExp(2),IntegerExp(3))
    testParses(receivedTokens,expected, parser.parseExp)
  }


  test("testMultiplyExp()"){
    val lexerInput = "5*3"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = MultiplyExp(IntegerExp(5),IntegerExp(3))
    testParses(receivedTokens,expected, parser.parseExp)
  }
  test("testDivideExp()"){
    val lexerInput = "4/2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = DivideExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  test("testPlusExp()"){
    val lexerInput = "4+2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = PlusExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  test("testSubtractExp()"){
    val lexerInput = "4-2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = SubtractExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  test("testEqualsExp()") {
    val lexerInput = "4==2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = EqualsExp(IntegerExp(4), IntegerExp(2))
    testParses(receivedTokens, expected, parser.parseExp)
  }
  test("testIntType()"){
    val input = "int "
    val expectedProgram = IntTypes
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseTypes)
  }

  test("testBooleanType()"){
    val input = "bool "
    val expectedProgram = BoolTypes
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseTypes)
  }

  test("testClassNameType()"){
    val input = "className "
    val expectedProgram = ClassTypes("className")
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseTypes)
  }

  test("testVarDec()"){
    val input = "int num "
    val expectedProgram = VarDeclaration(IntTypes, "num")
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseVarDec)
  }

  test("testing not var declaration"){
    assertThrows[ParserException] {
      val input = "int void "
      val tokenizer = Lexer(input)
      val receivedTokens = tokenizer.tokenize()
      val parser = new Parser(receivedTokens)
      parser.parseVarDec(receivedTokens)
    }
  }

  test("testMethodDefWithNoParameter()"){
    val input = "bool testMethod() return true;"
    val expectedProgram = MethodDef(BoolTypes, "testMethod",null, List[VarDeclaration](), BooleanExp(true))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseMethodDef)
  }

  test("testMethodDefWith1Parameter()"){
    val input = "bool testMethod(int foo) int i =10; return true;"
    val expectedProgram = MethodDef(BoolTypes, "testMethod", AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(10)),
      List[VarDeclaration](VarDeclaration(IntTypes, "foo")), BooleanExp(true))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseMethodDef)
  }

  test("testMethodDefWith1Parameter witth no return"){
    val input = "bool testMethod(int foo) int i =10; return;"
    val expectedProgram = MethodDef(BoolTypes, "testMethod", AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(10)),
      List[VarDeclaration](VarDeclaration(IntTypes, "foo")), null)
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseMethodDef)
  }

  test("testMethodDefWith2Parameter()"){
    val input = "bool testMethod(int foo, bool bar) return true;"
    val expectedProgram = MethodDef(BoolTypes, "testMethod", null,
      List[VarDeclaration](VarDeclaration(IntTypes, "foo"), VarDeclaration(BoolTypes, "bar")), BooleanExp(true))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseMethodDef)
  }

  test("testMethodDefWith2Parameter no return"){
    val input = "bool testMethod(int foo, bool bar) return;"
    val expectedProgram = MethodDef(BoolTypes, "testMethod", null,
      List[VarDeclaration](VarDeclaration(IntTypes, "foo"), VarDeclaration(BoolTypes, "bar")), BooleanExp(false))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseMethodDef)
  }

  test("testInstanceDec()"){
    val input = "bool foobar;"
    val expectedProgram = InstanceDec(VarDeclaration(BoolTypes, "foobar"))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseInstanceDec)
  }

  test("testing InstanceDec throws exception for missing semicolon "){
    assertThrows[ParserException] {
      val input = "bool foobar"
      val tokenizer = Lexer(input)
      val receivedTokens = tokenizer.tokenize()
      val parser = new Parser(receivedTokens)
      parser.parseInstanceDec(receivedTokens)
    }
  }

  test("testMethodNameExp()"){
    val input = "foobar(5)"
    val expectedProgram = MethodExp(IntegerExp(5), null, "foobar", List[Exp]())
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseExp)
  }

  test("testMethodNameExp2()"){
    val input = "foobar(5, 6, 7, \"varClassName\" )"
    val expectedProgram = MethodExp(IntegerExp(5), "varClassName", "foobar", List[Exp](IntegerExp(6), IntegerExp(7)))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseExp)
  }

  test("testOrExp()"){
    val lexerInput = "4||2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = OrExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  test("testAndExp()"){
    val lexerInput = "4&&2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = AndExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  test("testGreaterThanExp()"){
    val lexerInput = "4>2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = GTExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  test("testGreaterThanEqualToExp()"){
    val lexerInput = "4>=2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = GTEExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  test("testLessThanExp()"){
    val lexerInput = "4<2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = LTExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  test("testLessThanEqualToExp()"){
    val lexerInput = "4<=2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = LTEExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }


//  test("testPrecendenceExp()"){
//    val lexerInput = "1+2*5^6"
//    val tokenizer = Lexer(lexerInput)
//    val receivedTokens = tokenizer.tokenize()
//    val parser = Parser(receivedTokens)
//    val expected = PlusExp(IntegerExp(1),MultiplyExp(IntegerExp(2),PowerExp(IntegerExp(5),IntegerExp(6))))
//    testParses(receivedTokens,expected, parser.parseExp)
//  }

//  test("testPrecendenceLeftToRightExp()"){
//    val lexerInput = "1^2*5+6"
//    val tokenizer = Lexer(lexerInput)
//    val receivedTokens = tokenizer.tokenize()
//    val parser = Parser(receivedTokens)
//    val expected = PlusExp(MultiplyExp(PowerExp(IntegerExp(1),IntegerExp(2)),IntegerExp(5)),IntegerExp(6))
//    testParses(receivedTokens,expected, parser.parseExp)
//  }

//  test("testHighOrderFunctionCallExp()"){
//    val lexerInput = "int hofc( myHighOrderFunction, \"mySecondExp\")"
//    val tokenizer = Lexer(lexerInput)
//    val receivedTokens = tokenizer.tokenize()
//    val parser = Parser(receivedTokens)
//    val expected = CallHighOrderExp(VariableExp("myHighOrderFunction"),IntTypes, StringExp("mySecondExp"))
//    testParses(receivedTokens,expected, parser.parseExp)
//  }

  test("test HighOrderFunctionCallExp throws a parser exception when missing a comma"){
    assertThrows[ParserException] {
      val lexerInput = "hofc( myHighOrderFunction \"mySecondExp\")"
      val tokenizer = Lexer(lexerInput)
      val receivedTokens = tokenizer.tokenize()
      val parser = Parser(receivedTokens)
      parser.parseExp(receivedTokens)
    }
  }

  test("test HighOrderFunctionCallExp throws a parser exception when missing a right paren"){
    assertThrows[ParserException] {
      val lexerInput = "hofc( myHighOrderFunction , \"mySecondExp\""
      val tokenizer = Lexer(lexerInput)
      val receivedTokens = tokenizer.tokenize()
      val parser = Parser(receivedTokens)
      parser.parseExp(receivedTokens)
    }
  }

  test("testing call high order expression throws parser exception when missing right paren "){
    assertThrows[ParserException] {
      val lexerInput = "(int myVariable  => myVariable + 2"
      val tokenizer = Lexer(lexerInput)
      val receivedTokens = tokenizer.tokenize()
      val parser = Parser(receivedTokens)
      parser.parseExp(receivedTokens)
    }
  }

//  test("testCreateHighOrderFunctionExp()") {
//    val lexerInput = "int (int myVariable ) => myVariable + 2"
//    val tokenizer = Lexer(lexerInput)
//    val receivedTokens = tokenizer.tokenize()
//    val parser = Parser(receivedTokens)
//    val expected = HighOrderExp(,"myVariable" ,null, IntTypes, PlusExp(VariableExp("myVariable"),IntegerExp(2)))
//    testParses(receivedTokens,expected, parser.parseExp)
//  }

//  test("testMethodNameExp1()"){
//    val input = "foobar(5, 6)"
//    val expectedProgram = MethodExp(IntegerExp(5), "foobar", List[Exp](IntegerExp(6)))
//    val tokenizer = Lexer(input)
//    val receivedTokens = tokenizer.tokenize()
//    val parser = new Parser(receivedTokens)
//    testParses(receivedTokens, expectedProgram, parser.parseExp)
//  }


  test("testStmtVarDec()"){
    val input = "int i = 0;"
    val expectedProgram = AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(0))
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseStmt)

  }
  test("testStmtFor()"){
    val input = "for(i = 0; i < 10; i = i + 10;) 1;"
    val expectedProgram = ForStmt(VarStmt("i", IntegerExp(0)), LTExp(VariableExp("i"),IntegerExp(10)), VarStmt("i", PlusExp(VariableExp("i"), IntegerExp(10))), ExpStmt(IntegerExp(1)))
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseStmt)
  }
  test("testing StmtFor throw parser exception when missing a right paren token"){
    assertThrows[ParserException] {
      val input = "for(i = 0; i < 10; i = i + 10; 1;"
      val tokenizer = Lexer(input)
      val tokens = tokenizer.tokenize()
      val parser = Parser(tokens)
      parser.parseStmt(tokens)
    }
  }
  test("testing StmtFor throw parser exception when missing a semicolon at the end of the expression "){
    assertThrows[ParserException]{
      val input = "for(i = 0; i < 10 i = i + 10;) 1"
      val tokenizer = Lexer(input)
      val tokens = tokenizer.tokenize()
      val parser = Parser(tokens)
      parser.parseStmt(tokens)
    }
  }

  test("testBlockStmt()"){
    val input = "{int myInt = 10;}"
    val tokenizer = Lexer(input)
    val expectedProgram = BlockStmt(List[Stmt](AssignmentStmt(VarDeclaration(IntTypes, "myInt"), IntegerExp(10))))
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseStmt)
  }

  test("testIfStmt()"){
    val input = "if (x == 10) int w = 10; else  int w = 11;"
    val tokenizer = Lexer(input)
    val expectedProgram = ConditionalStmt(EqualsExp(VariableExp("x"), IntegerExp(10)) ,AssignmentStmt(VarDeclaration(IntTypes, "w"), IntegerExp(10)), AssignmentStmt(VarDeclaration(IntTypes, "w"), IntegerExp(11)))
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseStmt)
  }
  test("testing IfStmt throws parser exception when missing an else"){
    assertThrows[ParserException] {
      val input = "if (x == 10) int w = 10;"
      val tokenizer = Lexer(input)
      val tokens = tokenizer.tokenize()
      val parser = Parser(tokens)
      parser.parseStmt(tokens)
    }
  }
  test("testing IfStmt throws parser exception when missing a right paren token"){
    assertThrows[ParserException] {
      val input = "if (x == 10 int w = 10;"
      val tokenizer = Lexer(input)
      val tokens = tokenizer.tokenize()
      val parser = Parser(tokens)
      parser.parseStmt(tokens)
    }
  }
  test("testing ExpStmt throws parser exception when missing a semicolon"){
    assertThrows[ParserException] {
      val input = "1"
      val tokenizer = Lexer(input)
      val tokens = tokenizer.tokenize()
      val parser = Parser(tokens)
      parser.parseStmt(tokens)
    }
  }

  test("testing AssignmentStmt throws parser exception when missing a semicolon"){
    assertThrows[ParserException] {
      val input = "int i = 0"
      val tokenizer = Lexer(input)
      val tokens = tokenizer.tokenize()
      val parser = Parser(tokens)
      parser.parseStmt(tokens)
    }
  }

  test("testing AssignmentStmt throws parser exception when missing a equals"){
    assertThrows[ParserException] {
      val input = "int i "
      val tokenizer = Lexer(input)
      val tokens = tokenizer.tokenize()
      val parser = Parser(tokens)
      parser.parseStmt(tokens)
    }
  }

  test("testing not a valid stmt "){
    assertThrows[ParserException] {
      val input = "void"
      val tokenizer = Lexer(input)
      val tokens = tokenizer.tokenize()
      val parser = Parser(tokens)
      parser.parseStmt(tokens)
    }
  }

//  test("testStmtReturnVoid()"){
//    val input = "return;"
//    val tokenizer = Lexer(input)
//    val expectedProgram = VoidStmt
//    val tokens = tokenizer.tokenize()
//    val parser = Parser(tokens)
//    testParses(tokens, expectedProgram, parser.parseStmt)
//
//  }
//
//  test("testStmtReturnExp()"){
//    val input = "return 1 + 2;"
//    val tokenizer = Lexer(input)
//    val expectedProgram = ReturnStmt(PlusExp(IntegerExp(1),IntegerExp(2)))
//    val tokens = tokenizer.tokenize()
//    val parser = Parser(tokens)
//    testParses(tokens, expectedProgram, parser.parseStmt)
//
//  }

  test("testNewClassExp()"){
    val input = "new myClass(1 - 2)"
    val tokenizer = Lexer(input)
    val expectedProgram = NewClassExp("myClass", List(SubtractExp(IntegerExp(1), IntegerExp(2))))
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseExp)
  }

  test("testing NewClassExp throws parser exception "){
    val input = "myClass(print(1), 1 - 2)"
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    parser.parseExp(tokens)
  }

  test("testPrintExp()"){
    val input = "print(45) "
    val expectedProgram = PrintExp(IntegerExp(45))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseExp)
  }

  test("testing PrintExp throws parser exception for missing right paren"){
    assertThrows[ParserException] {
      val input = "print(45 "
      val tokenizer = Lexer(input)
      val receivedTokens = tokenizer.tokenize()
      val parser = new Parser(receivedTokens)
      parser.parseExp(receivedTokens)
    }
  }

  test("testing void types"){
    val input = "void"
    val expectedProgram = VoidTypes
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseTypes)
  }

  test("testing groupedExp"){
    val input = "(0)"
    val expectedProgram = GroupedExp(IntegerExp(0))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseGroupedExpression)
  }

  test("testing groupedExp throws a parser exception for missing exp"){
    assertThrows[ParserException] {
      val input = "10"
      val tokenizer = Lexer(input)
      val receivedTokens = tokenizer.tokenize()
      val parser = new Parser(receivedTokens)
      parser.parseGroupedExpression(receivedTokens)
    }
  }

  test("testing groupedExp throws a parser exception for missing right paren"){
    assertThrows[ParserException] {
      val input = "( 10"
      val tokenizer = Lexer(input)
      val receivedTokens = tokenizer.tokenize()
      val parser = new Parser(receivedTokens)
      parser.parseGroupedExpression(receivedTokens)
    }
  }

  test("testing class extends"){
    val input = "Class testing extends otherClass{" +
      "int i;" +
      "constructor(int w) 1; " +
      "bool testMethod(int foo) return true; }"
    val expectedProgram = DefExtClass("testing", "otherClass", ExpStmt(IntegerExp(1)), List(InstanceDec(VarDeclaration(IntTypes, "i"))), List(VarDeclaration(IntTypes, "w")), List(MethodDef(BoolTypes, "testMethod", null,
      List[VarDeclaration](VarDeclaration(IntTypes, "foo")), BooleanExp(true))))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseClass)
  }

  test("testing class def"){
    val input = "Class testing extends testing2{" +
      "int i;" +
      "constructor(int w) 1; " +
      "bool testMethod(int foo) }"
    val expectedProgram = DefClass("testing", "testing2", ExpStmt(IntegerExp(1)), List(InstanceDec(VarDeclaration(IntTypes, "i"))),
      List(VarDeclaration(IntTypes, "w")), List(MethodDef(BoolTypes, "testMethod", null,
      List[VarDeclaration](VarDeclaration(IntTypes, "foo")), BooleanExp(true))))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseClass)
  }

  //  def main(args: Array[String]) {
//    //    testPrecendenceLeftToRightExp()
//    testStmtFor()
//    testStmtVarDec()
//    testStmtBreak()
////    testStmtReturnExp()
////    testStmtReturnVoid()
//    testBlockStmt()
//    testIfStmt()
//    testNewClassExp()
//    testCastExp()
//    testIntType()
//    testStrType()
//    testBooleanType()
//    testClassNameType()
//    testVarDec()
//    testMethodDefWithNoParameter()
//    testMethodDefWith1Parameter()
//    testMethodDefWith2Parameter()
//    testInstanceDec()
//    testMethodNameExp()
//    testMethodNameExp1()
//    testMethodNameExp2()
//    testPrintExp()
//    testCreateHighOrderFunctionExp()
//    testHighOrderFunctionCallExp()
//    testPrecendenceExp()
//    testLessThanEqualToExp()
//    testLessThanExp()
//    testGreaterThanEqualToExp()
//    testGreaterThanExp()
//    testAndExp()
//    testOrExp()
//    testEqualsExp()
//    testSubtractExp()
//    testPlusExp()
//    testDivideExp()
//    testMultiplyExp()
//    testPowerExp()
//    testSimpleGroupedExp()
//    testVariableExp()
//    testStringExp()
//    testIntegerExp()
//    testBooleanExp()
//  }
}
