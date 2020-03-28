
object ParserTest {
  def testParses(input: List[Token], expectedProgram: Prgm ) {
    val parser = Parser(input)
    val (received, _) = parser.parseProgram(input)
    assert(received == expectedProgram)
  }

  def testParses[A](input: List[Token], expectedProgram: A,  parseOne: List[Token] => (A, List[Token])) {
    val (received, _) = parseOne(input)
    assert(received == expectedProgram)
  }

  def testClass(): Unit ={
    val input = "Class testing { int myInt;" +
      "constructor(bool myBool){1;}" +
      "int myMethod(str myString){1;}" +
      "}"
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val (received, _) = parser.parseClass(receivedTokens)
    received
  }

  def testExps(): Unit ={
    val input = "1^3 + 1^3 "
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val (received, _) = parser.parseExp(receivedTokens)
    received
  }



  def testUsingLexer(): Unit ={
    val input = "Class testing { int myInt;" +
      "constructor(bool myBool){1;}" +
      "int myMethod(str myString){1;}" +
      "}1"
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val program = Prgm(IntegerExp(1),
                    List(
                      DefClass("testing",
                      BlockStmt(List(ExpStmt(IntegerExp(1)))), //stmt after the method
                      List(DecInstance(VarDeclaration(IntTypes,"myInt"))),
                      List(VarDeclaration(BoolTypes,"myBool")),
                      List(DefMethod(StrTypes,"myMethod",ExpStmt(IntegerExp(1) ),List(VarDeclaration(StrTypes,"myString"))))
                      )
                    )
                  )
    testParses(receivedTokens,program)
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

  def testStmtReturnVoid(): Unit={
    val input = "return;"
    val tokenizer = Lexer(input)
    val expectedProgram = VoidStmt
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseStmt)

  }

  def testStmtReturnExp(): Unit={
    val input = "return 1 + 2;"
    val tokenizer = Lexer(input)
    val expectedProgram = ReturnStmt(PlusExp(IntegerExp(1),IntegerExp(2)))
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    testParses(tokens, expectedProgram, parser.parseStmt)

  }

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
  def testEx() {
//
//    val tokens=List(
//      ClassToken,
//      VarToken("test"),
//      LeftCurlyToken,
//      IntTypeToken,
//      VarToken("intVariable"),
//      ConstructorToken,
//      LeftParenToken,
//      BooleanTypeToken,
//      VarToken("booleanVariable"),
//      RightParenToken,
//      LeftCurlyToken,
//      IntegerToken(1),
//      SemicolonToken,
//      RightCurlyToken,
//      StringTypeToken,
//      VarToken("stringMethod"),
//      LeftParenToken,
//      IntTypeToken,
//      VarToken("testMethodVar"),
//      RightParenToken,
//      IntegerToken(1),
//      SemicolonToken,
//      RightCurlyToken,
//      IntegerToken(1)
//    )
//
//    val program = Prgm(IntegerExp(1), DefClass("test",BlockStmt(ExpStmt(IntegerExp(1))),
//      List(DecInstance(VarDeclaration(IntTypes,"intVariable"))),
//      List(VarDeclaration(BoolTypes,"booleanVariable")),
//      List(DefMethod(StrTypes,"testMethodVar",ExpStmt(IntegerExp(1))))
//      )
//    )
//    testParses(tokens, program)
  }


  def main(args: Array[String]): Unit = {
//    testExps()
//    testUsingLexer()
//    testClass()
    testStmtFor()
    testStmtVarDec()
    testStmtBreak()
    testStmtReturnExp()
    testStmtReturnVoid()
    testBlockStmt()
    testIfStmt()
    testNewClassExp()
    testCastExp()
  } // main
} // LexerTest

