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
  } // main
} // LexerTest