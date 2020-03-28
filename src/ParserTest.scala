object ParserTest {
  def testParses(input: List[Token], expectedProgram: Prgm ) {
    val parser = Parser(input)
    val (received, _) = parser.parseProgram(input)
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
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    val (received, _ )  = parser.parseStmt(tokens)
    received
  }
  def testStmtFor(): Unit={
    val input = "for(i = 0; i < 10; i = i +1;) 1;"
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    val (received, _ )  = parser.parseStmt(tokens)
    received
  }
  def testStmtBreak(): Unit={
    val input = "break;"
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    val (received, _ )  = parser.parseStmt(tokens)
    received
  }

  def testBlockStmt(): Unit ={
    val input = "{int myInt = 10; {int newInt = 11;}}"
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    val (received, _ )  = parser.parseStmt(tokens)
    received
  }

  def testIfStmt(): Unit ={
    val input = "if (x + 5 == 10) int w = 10; else  int w = 11;"
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    val (received, _ )  = parser.parseStmt(tokens)
    received
  }

  def testStmtReturnVoid(): Unit={
    val input = "return;"
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    val (received, _ )  = parser.parseStmt(tokens)
    received
  }

  def testStmtReturnExp(): Unit={
    val input = "return (1 + 2);"
    val tokenizer = Lexer(input)
    val tokens = tokenizer.tokenize()
    val parser = Parser(tokens)
    val (received, _ )  = parser.parseStmt(tokens)
    received
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
    testStmtVarDec()
    testStmtBreak()
    testStmtReturnExp()
    testStmtReturnVoid()
    testStmtFor()
    testBlockStmt()
    testIfStmt()
  } // main
} // LexerTest

