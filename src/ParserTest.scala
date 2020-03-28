object ParserTest {
  def testParses(input: List[Token], expectedProgram: Prgm ) {
    val parser = Parser(input)
    val (received, _) = parser.parseProgram(input)
    assert(received == expectedProgram)
  }

  def testUsingLexer(): Unit ={
    val input = "Class testing { int myInt;" +
      "constructor(bool myBool){1;}" +
      "int myMethod(str myString){1;}" +
      "}1"
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    val program = Prgm(IntegerExp(1),
                    DefClass("testing",
                      BlockStmt(ExpStmt(IntegerExp(1))), //stmt after the method
                      List(DecInstance(VarDeclaration(IntTypes,"myInt"))),
                      List(VarDeclaration(BoolTypes,"myBool")),
                      List(DefMethod(StrTypes,"myMethod",ExpStmt(IntegerExp(1))))
                    )
                  )
    testParses(receivedTokens,program)
  }

  def testEx() {

    val tokens=List(
      ClassToken,
      VarToken("test"),
      LeftCurlyToken,
      IntTypeToken,
      VarToken("intVariable"),
      ConstructorToken,
      LeftParenToken,
      BooleanTypeToken,
      VarToken("booleanVariable"),
      RightParenToken,
      LeftCurlyToken,
      IntegerToken(1),
      SemicolonToken,
      RightCurlyToken,
      StringTypeToken,
      VarToken("stringMethod"),
      LeftParenToken,
      IntTypeToken,
      VarToken("testMethodVar"),
      RightParenToken,
      IntegerToken(1),
      SemicolonToken,
      RightCurlyToken,
      IntegerToken(1)
    )

    val program = Prgm(IntegerExp(1), DefClass("test",BlockStmt(ExpStmt(IntegerExp(1))),
      List(DecInstance(VarDeclaration(IntTypes,"intVariable"))),
      List(VarDeclaration(BoolTypes,"booleanVariable")),
      List(DefMethod(StrTypes,"testMethodVar",ExpStmt(IntegerExp(1))))
      )
    )
    testParses(tokens, program)
  }


  def main(args: Array[String]): Unit = {
    testEx()
    testUsingLexer()
  } // main
} // LexerTest

