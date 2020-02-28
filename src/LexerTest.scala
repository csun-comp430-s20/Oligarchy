object LexerTest {
  def testTokenizes(input: String, expectedTokens: Token*) {
    val tokenizer = Lexer(input)
    val receivedTokens = tokenizer.tokenize()
    assert(receivedTokens == expectedTokens.toSeq)
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

  //daniel
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


  def main(args: Array[String]) {
    testLeftParen()
    testRightParen()
    testVariableAlone()
    testVariableWithWhitespaceBefore()
    testVariableWithWhitespaceAfter()
    testVariableContainingReservedWords()
    testTwoReservedWords()

    //daniel
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


  } // main
} // LexerTest