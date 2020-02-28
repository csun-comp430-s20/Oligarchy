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
  def main(args: Array[String]) {
    testLeftParen()
    testRightParen()
    testVariableAlone()
    testVariableWithWhitespaceBefore()
    testVariableWithWhitespaceAfter()
    testVariableContainingReservedWords()
    testTwoReservedWords()
    testBooleanTrue()
    testBooleanFalse()
    testBooleanWithLeadingWhiteSpace()
    testBooleanWithEndingWhiteSpace()
    testBooleanWithWhiteSpaces()
    testClass()
    testClassWithWhiteSpaces()
    testEquals()

  } // main
} // LexerTest

