import org.scalatest.funsuite.AnyFunSuite

class LexerTest extends AnyFunSuite {
  def testTokenizes(input: String, expectedTokens: Token* ) {
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    assert(receivedTokens == expectedTokens.toSeq)
  }

  test("example test name") {
    testTokenizes("})", RightCurlyToken, RightParenToken)
  }

  test("testRightCurly()") {
    testTokenizes("}", RightCurlyToken)
  }

  test("testGreaterThan()") {
    testTokenizes(">", GreaterThanToken)
  }

  test("testPlus()"){
    testTokenizes("+", PlusToken)
  }

  test("testPrint()"){
    testTokenizes("print", PrintToken)
  }

  test("testPositiveLargeInt()") {
    testTokenizes("2147483647", IntegerToken(2147483647))
  }

  test("testPositiveInt()") {
    testTokenizes("0", IntegerToken(0))
  }

//  test("testNegativeInt()") {
//    testTokenizes("-1", SubtractToken, IntegerToken(1))
//  }
//
//  test("testNegativeLargeInt()") {
//    testTokenizes("-2147483648", IntegerToken(-2147483648))
//  }

  test("testStringToken()"){
    testTokenizes("\"testString\"", StrToken("testString"))
  }
  test("testInitializeString()"){
    testTokenizes(" str s = \"string\";", StringTypeToken, VarToken("s"), EqualsToken,  StrToken("string"),SemicolonToken)
  }
  test("testFuncPrintString()"){
    testTokenizes("func toString(){" +
      "print(\"hello, \n world \")" +
      "}",FuncToken, VarToken("toString"), LeftParenToken,RightParenToken,LeftCurlyToken,PrintToken,
        LeftParenToken, StrToken("hello, \n world "),RightParenToken,RightCurlyToken)
  }

  test("testLeftParen()") {
    testTokenizes("(", LeftParenToken)
  }


  test("testRightParen()") {
    testTokenizes(")", RightParenToken)
  }

  test("testVariableAlone()") {
    testTokenizes("x", VarToken("x"))
  }

  test("testVariableWithWhitespaceBefore()") {
    testTokenizes(" x", VarToken("x"))
  }

  test("testVariableWithWhitespaceAfter()") {
    testTokenizes("x ", VarToken("x"))
  }

  test("testVariableContainingReservedWords()") {
    testTokenizes("ifelse", VarToken("ifelse"))
  }

  test("testTwoReservedWords()") {
    testTokenizes("if else", IfToken, ElseToken)
  }

  test("testAnd()") {
    testTokenizes("&", AndToken)
  }

  test("testMultiplication()") {
    testTokenizes("*", MultiplicationToken)
  }

  test("testBreak()") {
    testTokenizes("break", BreakToken)
  }

  test("testLessThan()"){
    testTokenizes("<", LessThanToken)
  }

  test("testFunc()"){
    testTokenizes("func", FuncToken)
  }

  test("testFor()"){
    testTokenizes("for", ForToken)
  }

  test("testSubtract()"){
    testTokenizes("-", SubtractToken)
  }

  test("testLeftCurly()"){
    testTokenizes("{", LeftCurlyToken)
  }

  test("testConstructor()"){
    testTokenizes("constructor", ConstructorToken)
  }

  test("testPeriod()") {
    testTokenizes(".", PeriodToken)
  }

  test("testSemiColon()") {
    testTokenizes(";", SemicolonToken)
  }

  test("testDivision()") {
    testTokenizes("/", DivisionToken)
  }

  test("testOr()") {
    testTokenizes("|", OrToken)
  }

  test("testPeriodWithWhitespaceBefore()") {
    testTokenizes(" .", PeriodToken)
  }

  test("testPeriodWithWhitespaceAfter()") {
    testTokenizes(". ", PeriodToken)
  }

  test("testSemiColonWithWhitespaceBefore()") {
    testTokenizes(" ;", SemicolonToken)
  }

  test("testSemicolonWithWhitespaceAfter()") {
    testTokenizes("; ", SemicolonToken)
  }

  test("testDivisionWithWhitespaceBefore()") {
    testTokenizes(" /", DivisionToken)
  }

  test("testDivisionWithWhitespaceAfter()") {
    testTokenizes("/ ", DivisionToken)
  }

  test("testOrWithWhitespaceBefore()") {
    testTokenizes(" |", OrToken)
  }

  test("testOrWithWhitespaceAfter()") {
    testTokenizes("| ", OrToken)
  }

  test("testBooleanTrue()"){
    testTokenizes("true", BooleanToken(true))
  }

  test("testBooleanFalse()"){
    testTokenizes("false", BooleanToken(false))
  }

  test("testBooleanWithWhiteSpaces()"){
    testTokenizes(" true ", BooleanToken(true))
  }

  test("testBooleanWithLeadingWhiteSpace()"){
    testTokenizes(" false", BooleanToken(false))
  }

  test("testBooleanWithEndingWhiteSpace()"){
    testTokenizes("true ", BooleanToken(true))
  }

  test("testClass()"){
    testTokenizes("Class", ClassToken)
  }

  test("testClassWithWhiteSpaces()"){
    testTokenizes(" Class ", ClassToken)
  }

  test("testEquals()"){
    testTokenizes("=", EqualsToken)
  }

  test("testingStatementIf()"){
    testTokenizes("if (i = 10){" +
                            "return true;" +
                          "}",
      IfToken, LeftParenToken, VarToken("i"), EqualsToken, IntegerToken(10), RightParenToken, LeftCurlyToken,
      ReturnToken, BooleanToken(true),SemicolonToken,
      RightCurlyToken)
  }
  test("testingStatementIfElse()"){
    testTokenizes("if (i = 10 | i = 5){" +
                            "return true;" +
                          "} " +
                          "else{" +
                          "i++" +
                          "} ",
      IfToken, LeftParenToken, VarToken("i"), EqualsToken, IntegerToken(10), OrToken, VarToken("i"), EqualsToken, IntegerToken(5), RightParenToken, LeftCurlyToken,
      ReturnToken, BooleanToken(true), SemicolonToken,
      RightCurlyToken,
      ElseToken, LeftCurlyToken,
      VarToken("i"), PlusToken, PlusToken,
      RightCurlyToken)
  }

  test("testForLoop()"){
      testTokenizes("for (int i = 0; i <= 10; i++){ w = 10 }", ForToken, LeftParenToken,IntTypeToken, VarToken("i"), EqualsToken,
        IntegerToken(0), SemicolonToken, VarToken("i"), LessThanToken, EqualsToken, IntegerToken(10), SemicolonToken,
        VarToken("i") , PlusToken, PlusToken, RightParenToken, LeftCurlyToken, VarToken("w"), EqualsToken, IntegerToken(10),
        RightCurlyToken)
  }

  test("testPrintWithInteger()"){
    testTokenizes("print(10);", PrintToken, LeftParenToken, IntegerToken(10), RightParenToken, SemicolonToken)
  }

  test("testExp1()"){
//    testTokenizes("int i = 50;", TypeToken("int"), VarToken("i"), EqualsToken,IntegerToken(50), SemicolonToken)
  }

  test("testMath1()"){     // (5 * 10) + 3*(7) / 10 + 12-10
    testTokenizes("(5 * 10) + 3*(7) / 10 + 12-10", LeftParenToken, IntegerToken(5), MultiplicationToken,
      IntegerToken(10), RightParenToken, PlusToken, IntegerToken(3), MultiplicationToken, LeftParenToken,
      IntegerToken(7), RightParenToken, DivisionToken, IntegerToken(10), PlusToken, IntegerToken(12), SubtractToken,
      IntegerToken(10))
  }
  test("testSubtraction1()"){
    //testTokenizes("5-10", IntegerToken(5), SubtractToken, IntegerToken(10))
    testTokenizes("5-10", IntegerToken(5),  SubtractToken, IntegerToken(10))
  }

  def main(args: Array[String]) {
//    testLeftParen()
//    testRightParen()
//    testVariableAlone()
//    testVariableWithWhitespaceBefore()
//    testVariableWithWhitespaceAfter()
//    testVariableContainingReservedWords()
//    testTwoReservedWords()
//    testLessThan()
//    testFunc()
//    testFor()
//    testSubtract()
//    testLeftCurly()
//    testConstructor()
//    testPeriod()
//    testSemiColon()
//    testDivision()
//    testOr()
//    testPeriodWithWhitespaceBefore()
//    testPeriodWithWhitespaceAfter()
//    testSemiColonWithWhitespaceBefore()
//    testSemicolonWithWhitespaceAfter()
//    testDivisionWithWhitespaceBefore()
//    testDivisionWithWhitespaceAfter()
//    testOrWithWhitespaceBefore()
//    testOrWithWhitespaceAfter()
//    testBooleanTrue()
//    testBooleanFalse()
//    testBooleanWithLeadingWhiteSpace()
//    testBooleanWithEndingWhiteSpace()
//    testBooleanWithWhiteSpaces()
//    testClass()
//    testClassWithWhiteSpaces()
//    testEquals()
////    testNegativeInt()//
////    testNegativeLargeInt()
//    testRightCurly()
//    testPlus()
//    testGreaterThan()
//    testPrint()
//    testEx()
//    testingStatementIf()
//    testingStatementIfElse()
//    testForLoop()
//    testPrintWithInteger()
//    testExp1()
//    testStringToken()
//    testInitializeString()
//    testFuncPrintString()
//    testAnd()
//    testMultiplication()
//    testBreak()
//    testMath1()
//    testSubtraction1
  } // main
}
