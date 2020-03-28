object ParserTest {
  def testParses(input: List[Token], expectedProgram: Prgm ) {
    val parser = Parser(input)
    val (received, _) = parser.parseProgram(input)
    assert(received == expectedProgram)
  }

  def testParserMethod[A](input: List[Token], expectedProgram: A,  parseOne: List[Token] => (A, List[Token])) {
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

  def testIntType(): Unit ={
    val input = "int "
    val expectedProgram = IntTypes
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParserMethod(receivedTokens, expectedProgram, parser.parseTypes)
  }

  def testStrType(): Unit ={
    val input = "str "
    val expectedProgram = StrTypes
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParserMethod(receivedTokens, expectedProgram, parser.parseTypes)
  }

  def testBooleanType(): Unit ={
    val input = "bool "
    val expectedProgram = BoolTypes
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParserMethod(receivedTokens, expectedProgram, parser.parseTypes)
  }

  def testClassNameType(): Unit ={
    val input = "className "
    val expectedProgram = ClassTypes("className")
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParserMethod(receivedTokens, expectedProgram, parser.parseTypes)
  }

  def testVarDec(): Unit ={
    val input = "int num "
    val expectedProgram = VarDeclaration(IntTypes, "num")
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParserMethod(receivedTokens, expectedProgram, parser.parseVarDec)
  }

  def testMethodDefWithNoParameter(): Unit ={
    val input = "bool testMethod() return true;"
    val expectedProgram = DefMethod(BoolTypes, "testMethod", ReturnStmt(BooleanExp(true)), List[VarDec]())
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParserMethod(receivedTokens, expectedProgram, parser.parseMethodDef)
  }

  def testMethodDefWith1Parameter(): Unit ={
    val input = "bool testMethod(int foo) return true;"
    val expectedProgram = DefMethod(BoolTypes, "testMethod", ReturnStmt(BooleanExp(true)),
      List[VarDec](VarDeclaration(IntTypes, "foo")))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParserMethod(receivedTokens, expectedProgram, parser.parseMethodDef)
  }

  def testMethodDefWith2Parameter(): Unit ={
    val input = "bool testMethod(int foo, bool bar) return true;"
    val expectedProgram = DefMethod(BoolTypes, "testMethod", ReturnStmt(BooleanExp(true)),
      List[VarDec](VarDeclaration(IntTypes, "foo"), VarDeclaration(BoolTypes, "bar")))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParserMethod(receivedTokens, expectedProgram, parser.parseMethodDef)
  }

  def testInstanceDec(): Unit ={
    val input = "bool foobar;"
    val expectedProgram = DecInstance(VarDeclaration(BoolTypes, "foobar"))
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val parser = new Parser(receivedTokens)
    testParserMethod(receivedTokens, expectedProgram, parser.parseInstanceDec)
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

  def main(args: Array[String]): Unit = {
    testClass()
    testIntType()
    testStrType()
    testBooleanType()
    testClassNameType()
    testVarDec()
    testMethodDefWithNoParameter()
    testMethodDefWith1Parameter()
    testMethodDefWith2Parameter()
    testInstanceDec()
  } // main
} // LexerTest

