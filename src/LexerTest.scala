object LexerTest {
  def testTokenizes(input: String, expectedTokens: Token* ) {
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    assert(receivedTokens == expectedTokens.toSeq)
  }

  def testEx() {
    testTokenizes("})", RightCurlyToken, RightParenToken)
  }

  def testRightCurly() {
    testTokenizes("}", RightCurlyToken)
  }

  def testGreaterThan() {
    testTokenizes(">", GreaterThanToken)
  }

  def testPlus(){
    testTokenizes("+", PlusToken)
  }

  def testPrint(){
    testTokenizes("print", PrintToken)
  }

  def testPositiveLargeInt() {
    testTokenizes("2147483647", IntegerToken(2147483647))
  }

  def testPositiveInt() {
    testTokenizes("0", IntegerToken(0))
  }

//  def testNegativeInt() {
//    testTokenizes("-1", SubtractToken, IntegerToken(1))
//  }
//
//  def testNegativeLargeInt() {
//    testTokenizes("-2147483648", IntegerToken(-2147483648))
//  }

  def testStringToken(){
    testTokenizes("\"testString\"", StrToken("testString"))
  }
  def testInitializeString(){
    testTokenizes(" str s = \"string\";", TypeToken("str"), VarToken("s"), EqualsToken,  StrToken("string"),SemicolonToken)
  }
  def testFuncPrintString(){
    testTokenizes("func toString(){" +
      "print(\"hello, \n world \")" +
      "}",FuncToken, VarToken("toString"), LeftParenToken,RightParenToken,LeftCurlyToken,PrintToken,
        LeftParenToken, StrToken("hello, \n world "),RightParenToken,RightCurlyToken)
  }

  def testLeftParen() {
    testTokenizes("(", LeftParenToken)
  }


  def testRightParen() {
    testTokenizes(")", RightParenToken)
  }

  def testVariableAlone() {
    testTokenizes("x", VarToken("x"))
  }

  def testVariableWithWhitespaceBefore() {
    testTokenizes(" x", VarToken("x"))
  }

  def testVariableWithWhitespaceAfter() {
    testTokenizes("x ", VarToken("x"))
  }

  def testVariableContainingReservedWords() {
    testTokenizes("ifelse", VarToken("ifelse"))
  }

  def testTwoReservedWords() {
    testTokenizes("if else", IfToken, ElseToken)
  }

  def testAnd() {
    testTokenizes("&", AndToken)
  }

  def testMultiplication() {
    testTokenizes("*", MultiplicationToken)
  }

  def testBreak() {
    testTokenizes("break", BreakToken)
  }

  def testLessThan(){
    testTokenizes("<", LessThanToken)
  }

  def testFunc(){
    testTokenizes("func", FuncToken)
  }

  def testFor(){
    testTokenizes("for", ForToken)
  }

  def testSubtract(){
    testTokenizes("-", SubtractToken)
  }

  def testLeftCurly(){
    testTokenizes("{", LeftCurlyToken)
  }

  def testConstructor(){
    testTokenizes("constructor", ConstructorToken)
  }
  
  def testPeriod(): Unit = {
    testTokenizes(".", PeriodToken)
  }
  
  def testSemiColon(): Unit = {
    testTokenizes(";", SemicolonToken)
  }
  
  def testDivision(): Unit = {
    testTokenizes("/", DivisionToken)
  }
  
  def testOr(): Unit = {
    testTokenizes("|", OrToken)
  }
  
  def testPeriodWithWhitespaceBefore(): Unit = {
    testTokenizes(" .", PeriodToken)
  }
  
  def testPeriodWithWhitespaceAfter(): Unit = {
    testTokenizes(". ", PeriodToken)
  }
  
  def testSemiColonWithWhitespaceBefore(): Unit = {
    testTokenizes(" ;", SemicolonToken)
  }
  
  def testSemicolonWithWhitespaceAfter(): Unit = {
    testTokenizes("; ", SemicolonToken)
  }
  
  def testDivisionWithWhitespaceBefore(): Unit = {
    testTokenizes(" /", DivisionToken)
  }
  
  def testDivisionWithWhitespaceAfter(): Unit = {
    testTokenizes("/ ", DivisionToken)
  }
  
  def testOrWithWhitespaceBefore(): Unit = {
    testTokenizes(" |", OrToken)
  }
  
  def testOrWithWhitespaceAfter(): Unit = {
    testTokenizes("| ", OrToken)
  }

  def testBooleanTrue(){
    testTokenizes("true", BooleanToken(true))
  }

  def testBooleanFalse(){
    testTokenizes("false", BooleanToken(false))
  }

  def testBooleanWithWhiteSpaces(){
    testTokenizes(" true ", BooleanToken(true))
  }

  def testBooleanWithLeadingWhiteSpace(){
    testTokenizes(" false", BooleanToken(false))
  }

  def testBooleanWithEndingWhiteSpace(){
    testTokenizes("true ", BooleanToken(true))
  }

  def testClass(){
    testTokenizes("Class", ClassToken)
  }

  def testClassWithWhiteSpaces(){
    testTokenizes(" Class ", ClassToken)
  }

  def testEquals(){
    testTokenizes("=", EqualsToken)
  }

  def testingStatementIf(){
    testTokenizes("if (i = 10){" +
                            "return true;" +
                          "}",
      IfToken, LeftParenToken, VarToken("i"), EqualsToken, IntegerToken(10), RightParenToken, LeftCurlyToken,
      ReturnToken, BooleanToken(true),SemicolonToken,
      RightCurlyToken)
  }
  def testingStatementIfElse(){
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

  def testForLoop(){
      testTokenizes("for (int i = 0; i <= 10; i++){ w = 10 }", ForToken, LeftParenToken,TypeToken("int"), VarToken("i"), EqualsToken,
        IntegerToken(0), SemicolonToken, VarToken("i"), LessThanToken, EqualsToken, IntegerToken(10), SemicolonToken,
        VarToken("i") , PlusToken, PlusToken, RightParenToken, LeftCurlyToken, VarToken("w"), EqualsToken, IntegerToken(10),
        RightCurlyToken)
  }

  def testPrintWithInteger(){
    testTokenizes("print(10);", PrintToken, LeftParenToken, IntegerToken(10), RightParenToken, SemicolonToken)
  }

  def testExp1(){
    testTokenizes("int i = 50;", TypeToken("int"), VarToken("i"), EqualsToken,IntegerToken(50), SemicolonToken)
  }

  def testMath1(){     // (5 * 10) + 3*(7) / 10 + 12-10
    testTokenizes("(5 * 10) + 3*(7) / 10 + 12-10", LeftParenToken, IntegerToken(5), MultiplicationToken,
      IntegerToken(10), RightParenToken, PlusToken, IntegerToken(3), MultiplicationToken, LeftParenToken,
      IntegerToken(7), RightParenToken, DivisionToken, IntegerToken(10), PlusToken, IntegerToken(12), SubtractToken,
      IntegerToken(10))
  }
  def testSubtraction1(){
    //testTokenizes("5-10", IntegerToken(5), SubtractToken, IntegerToken(10))
    testTokenizes("5-10", IntegerToken(5),  SubtractToken, IntegerToken(10))
  }

  def main(args: Array[String]) {
    testLeftParen()
    testRightParen()
    testVariableAlone()
    testVariableWithWhitespaceBefore()
    testVariableWithWhitespaceAfter()
    testVariableContainingReservedWords()
    testTwoReservedWords()
    testLessThan()
    testFunc()
    testFor()
    testSubtract()
    testLeftCurly()
    testConstructor()
    testPeriod()
    testSemiColon()
    testDivision()
    testOr()
    testPeriodWithWhitespaceBefore()
    testPeriodWithWhitespaceAfter()
    testSemiColonWithWhitespaceBefore()
    testSemicolonWithWhitespaceAfter()
    testDivisionWithWhitespaceBefore()
    testDivisionWithWhitespaceAfter()
    testOrWithWhitespaceBefore()
    testOrWithWhitespaceAfter()
    testBooleanTrue()
    testBooleanFalse()
    testBooleanWithLeadingWhiteSpace()
    testBooleanWithEndingWhiteSpace()
    testBooleanWithWhiteSpaces()
    testClass()
    testClassWithWhiteSpaces()
    testEquals()
//    testNegativeInt()//
//    testNegativeLargeInt()
    testRightCurly()
    testPlus()
    testGreaterThan()
    testPrint()
    testEx()
    testingStatementIf()
    testingStatementIfElse()
    testForLoop()
    testPrintWithInteger()
    testExp1()
    testStringToken()
    testInitializeString()
    testFuncPrintString()
    testAnd()
    testMultiplication()
    testBreak()
    testMath1()
    testSubtraction1
  } // main
} // LexerTest

