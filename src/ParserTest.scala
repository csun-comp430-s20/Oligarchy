package src

object ParserTest {
  def testParses(input: List[Token], expectedProgram: Program ) {
    val parser = Parser(input)
    val (received, _) = parser.parseProgram(input)
    assert(received == expectedProgram)
  }

  def testParses[A](input: List[Token], expectedProgram: A,  parseOne: List[Token] => (A, List[Token])) {
    val (received, _) = parseOne(input)
    assert(received == expectedProgram)
  }


  //daniel
  def testOneClass(): Unit ={
    val lexerInput = "Class testing extends extendName { int myInt;" +
      "constructor(bool myBool){1;}" +
      "int myMethod(str myString){1;}" +
      "}"
    val lexer = Lexer(lexerInput)
    val receivedTokens = lexer.tokenize()
    /*
    //println(receivedTokens)
    for(element<-receivedTokens)
    {
      println(element)
    } */
    val parser = Parser(receivedTokens)
    val expected = DefExtClass("testing", "extendName",
          BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
          List(DecInstance(VarDeclaration(IntTypes,"myInt"))),
          List(VarDeclaration(BoolTypes,"myBool")),
          List(DefMethod(IntTypes,"myMethod",BlockStmt(List(ExpStmt(IntegerExp(1)))),List(VarDeclaration(StrTypes,"myString"))))
    )
/*
    val testProgramList = parser.parseClass(receivedTokens)
    println(testProgramList)
    println("(" + expected)
*/
    testParses(receivedTokens, expected, parser.parseClass)
  }
  //daniel
  def testSimpleProgram(): Unit ={
    val lexerInput = "Class testing extends extendName { int myInt;" +
      "constructor(bool myBool){1;}" +
      "int myMethod(str myString){1;}" +
      "} 1"
    val lexer = Lexer(lexerInput)
    val receivedTokens = lexer.tokenize()
    /*
    //println(receivedTokens)
    for(element<-receivedTokens)
    {
      println(element)
    } */
    val parser = Parser(receivedTokens)
    val expected = Prgm(IntegerExp(1),
      List(
        DefExtClass("testing", "extendName",
          BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
          List(DecInstance(VarDeclaration(IntTypes,"myInt"))),
          List(VarDeclaration(BoolTypes,"myBool")),
          List(DefMethod(IntTypes,"myMethod",BlockStmt(List(ExpStmt(IntegerExp(1)))),List(VarDeclaration(StrTypes,"myString"))))
        )
      )
    )

    //val testProgramList = parser.parseProgram(receivedTokens)
    //println(testProgramList)
    //println("(" + expected)

    testParses(receivedTokens, expected, parser.parseProgram)
  }



  def testClass(): Unit ={
    val input = "Class testing { int myInt;" +
      "constructor(bool myBool){1;}" +
      "int myMethod(str myString){1;}" +
      "Return;}"
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val program = Program(IntegerExp(1),
      List(
        DefClass("testing",
          BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
          List(InstanceDec(VarDeclaration(IntTypes,"myInt"))),
          List(VarDeclaration(BoolTypes,"myBool")),
          List(MethodDef(StrTypes,"myMethod",ExpStmt(IntegerExp(1) ),List(VarDeclaration(StrTypes,"myString")), BooleanExp(false)))
        )
      )
    )
    testParses(receivedTokens,program)
  }

  def testIntegerExp(): Unit ={
    val lexerInput = "35"
    val lexer = Lexer(lexerInput)
    val receivedTokens = lexer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = IntegerExp(35)
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testBooleanExp(): Unit ={
    val lexerInput = "true"
    val lexer = Lexer(lexerInput)
    val receivedTokens = lexer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = BooleanExp(true)
    testParses(receivedTokens,expected, parser.parseExp)
  }
  def testStringExp(): Unit ={
    val lexerInput = "\"testString\""
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = StringExp("testString")
    testParses(receivedTokens,expected, parser.parseExp)
  }
  def testVariableExp(): Unit ={
    val lexerInput = "variableCheck"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = VariableExp("variableCheck")
    testParses(receivedTokens,expected, parser.parseExp)
  }
  def testSimpleGroupedExp(): Unit ={
    val lexerInput = "(14)"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = GroupedExp(IntegerExp(14))
    testParses(receivedTokens,expected, parser.parseExp)
  }
  def testPowerExp(): Unit ={
    val lexerInput = "2^3"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = PowerExp(IntegerExp(2),IntegerExp(3))
    testParses(receivedTokens,expected, parser.parseExp)
  }


  def testMultiplyExp(): Unit ={
    val lexerInput = "5*3"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = MultiplyExp(IntegerExp(5),IntegerExp(3))
    testParses(receivedTokens,expected, parser.parseExp)
  }
  def testDivideExp(): Unit ={
    val lexerInput = "4/2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = DivideExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testPlusExp(): Unit ={
    val lexerInput = "4+2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = PlusExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testSubtractExp(): Unit ={
    val lexerInput = "4-2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = SubtractExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testEqualsExp(): Unit = {
    val lexerInput = "4==2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = EqualsExp(IntegerExp(4), IntegerExp(2))
    testParses(receivedTokens, expected, parser.parseExp)
  }
  def testIntType(): Unit ={
    val input = "int "
    val expectedProgram = IntTypes
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseTypes)
  }

  def testStrType(): Unit ={
    val input = "str "
    val expectedProgram = StrTypes
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseTypes)
  }

  def testBooleanType(): Unit ={
    val input = "bool "
    val expectedProgram = BoolTypes
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseTypes)
  }

  def testClassNameType(): Unit ={
    val input = "className "
    val expectedProgram = ClassTypes("className")
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseTypes)
  }

  def testVarDec(): Unit ={
    val input = "int num "
    val expectedProgram = VarDeclaration(IntTypes, "num")
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseVarDec)
  }

  def testMethodDefWithNoParameter(): Unit ={
    val input = "bool testMethod() return true;"
    val expectedProgram = MethodDef(BoolTypes, "testMethod",null, List[VarDeclaration](), BooleanExp(true))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseMethodDef)
  }

  def testMethodDefWith1Parameter(): Unit ={
    val input = "bool testMethod(int foo) int i =10; return true;"
    val expectedProgram = MethodDef(BoolTypes, "testMethod", AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(10)),
      List[VarDeclaration](VarDeclaration(IntTypes, "foo")), BooleanExp(true))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseMethodDef)
  }

  def testMethodDefWith2Parameter(): Unit ={
    val input = "bool testMethod(int foo, bool bar) return true;"
    val expectedProgram = MethodDef(BoolTypes, "testMethod", null,
      List[VarDeclaration](VarDeclaration(IntTypes, "foo"), VarDeclaration(BoolTypes, "bar")), BooleanExp(true))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseMethodDef)
  }

  def testInstanceDec(): Unit ={
    val input = "bool foobar;"
    val expectedProgram = InstanceDec(VarDeclaration(BoolTypes, "foobar"))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseInstanceDec)
  }

  def testMethodNameExp(): Unit ={
    val input = "foobar(5)"
    val expectedProgram = MethodExp(IntegerExp(5), "foobar", List[Exp]())
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseExp)
  }


  def testMethodNameExp2(): Unit ={
    val input = "foobar(5, 6, 7)"
    val expectedProgram = MethodExp(IntegerExp(5), "foobar", List[Exp](IntegerExp(6), IntegerExp(7)))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseExp)
  }

  def testOrExp(): Unit ={
    val lexerInput = "4||2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = OrExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testAndExp(): Unit ={
    val lexerInput = "4&&2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = AndExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testGreaterThanExp(): Unit ={
    val lexerInput = "4>2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = GTExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testGreaterThanEqualToExp(): Unit ={
    val lexerInput = "4>=2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = GTEExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testLessThanExp(): Unit ={
    val lexerInput = "4<2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = LTExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testLessThanEqualToExp(): Unit ={
    val lexerInput = "4<=2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = LTEExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testPrecendenceExp(): Unit ={
    val lexerInput = "1+2*5^6"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = PlusExp(IntegerExp(1),MultiplyExp(IntegerExp(2),PowerExp(IntegerExp(5),IntegerExp(6))))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testPrecendenceLeftToRightExp(): Unit ={
    val lexerInput = "1^2*5+6"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = PlusExp(MultiplyExp(PowerExp(IntegerExp(1),IntegerExp(2)),IntegerExp(5)),IntegerExp(6))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testHighOrderFunctionCallExp(): Unit ={
    val lexerInput = "hofc(myHighOrderFunction, mySecondExp)"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = CallHighOrderExp(VariableExp("myHighOrderFunction"),VariableExp("mySecondExp"))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testCreateHighOrderFunctionExp(): Unit ={
    val lexerInput = "(int myVariable ) => myVariable + 2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = HighOrderExp(IntTypes,"myVariable",PlusExp(VariableExp("myVariable"),IntegerExp(2)))
    testParses(receivedTokens,expected, parser.parseExp)
  }

  def testMethodNameExp1(): Unit ={
    val input = "foobar(5, 6)"
    val expectedProgram = MethodExp(IntegerExp(5), "foobar", List[Exp](IntegerExp(6)))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseExp)
  }


  def testStmtVarDec(): Unit={
    val input = "int i = 0;"
    val expectedProgram = AssignmentStmt(VarDeclaration(IntTypes, "i"), IntegerExp(0))
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseStmt)

  }
  def testStmtFor(): Unit={
    val input = "for(i = 0; i < 10; i = i + 10;) 1;"
    val expectedProgram = ForStmt(VarStmt("i", IntegerExp(0)), LTExp(VariableExp("i"),IntegerExp(10)), VarStmt("i", PlusExp(VariableExp("i"), IntegerExp(10))), ExpStmt(IntegerExp(1)))
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseStmt)
  }

  def testStmtBreak(): Unit={
    val input = "break;"
    val expectedProgram = BreakStmt
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseStmt)
  }

  def testBlockStmt(): Unit ={
    val input = "{int myInt = 10;}"
    val tokenizer = Lexer(input)
    val expectedProgram = BlockStmt(List[Stmt](AssignmentStmt(VarDeclaration(IntTypes, "myInt"), IntegerExp(10))))
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseStmt)
  }

  def testIfStmt(): Unit ={
    val input = "if (x == 10) int w = 10; else  int w = 11;"
    val tokenizer = Lexer(input)
    val expectedProgram = ConditionalStmt(EqualsExp(VariableExp("x"), IntegerExp(10)) ,AssignmentStmt(VarDeclaration(IntTypes, "w"), IntegerExp(10)), AssignmentStmt(VarDeclaration(IntTypes, "w"), IntegerExp(11)))
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseStmt)
  }

//  def testStmtReturnVoid(): Unit={
//    val input = "return;"
//    val tokenizer = Lexer(input)
//    val expectedProgram = VoidStmt
//    val tokens = tokenizer.tokenize()
//    val parser = Parser(tokens)
//    testParses(tokens, expectedProgram, parser.parseStmt)
//
//  }
//
//  def testStmtReturnExp(): Unit={
//    val input = "return 1 + 2;"
//    val tokenizer = Lexer(input)
//    val expectedProgram = ReturnStmt(PlusExp(IntegerExp(1),IntegerExp(2)))
//    val tokens = tokenizer.tokenize()
//    val parser = Parser(tokens)
//    testParses(tokens, expectedProgram, parser.parseStmt)
//
//  }

  def testNewClassExp(): Unit ={
    val input = "new myClass(print(1), 1 - 2)"
    val tokenizer = Lexer(input)
    val expectedProgram = NewClassExp("myClass", List[Exp](PrintExp(IntegerExp(1)), SubtractExp(IntegerExp(1), IntegerExp(2))))
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseExp)
  }

  def testCastExp(): Unit ={
    val input = "(int) true"
    val tokenizer = Lexer(input)
    val expectedProgram = CastExp(IntTypes, BooleanExp(true))
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseExp)
  }


  def testPrintExp(): Unit ={
    val input = "print(45) "
    val expectedProgram = PrintExp(IntegerExp(45))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParses(receivedTokens, expectedProgram, parser.parseExp)
  }

  def main(args: Array[String]): Unit = {
    //    testPrecendenceLeftToRightExp()
    testStmtFor()
    testStmtVarDec()
    testStmtBreak()
//    testStmtReturnExp()
//    testStmtReturnVoid()
    testBlockStmt()
    testIfStmt()
    testNewClassExp()
    testCastExp()
    testIntType()
    testStrType()
    testBooleanType()
    testClassNameType()
    testVarDec()
    testMethodDefWithNoParameter()
    testMethodDefWith1Parameter()
    testMethodDefWith2Parameter()
    testInstanceDec()
    testMethodNameExp()
    testMethodNameExp1()
    testMethodNameExp2()
    testPrintExp()
    testCreateHighOrderFunctionExp()
    testHighOrderFunctionCallExp()
    testPrecendenceExp()
    testLessThanEqualToExp()
    testLessThanExp()
    testGreaterThanEqualToExp()
    testGreaterThanExp()
    testAndExp()
    testOrExp()
    testEqualsExp()
    testSubtractExp()
    testPlusExp()
    testDivideExp()
    testMultiplyExp()
    testPowerExp()
    testSimpleGroupedExp()
    testVariableExp()
    testStringExp()
    testIntegerExp()
    testBooleanExp()
    //daniel
    testOneClass()
    //daniel
    testSimpleProgram()
  }
} // ParserTest

