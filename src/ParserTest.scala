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

  def testEqualsExp(): Unit ={
    val lexerInput = "4==2"
    val tokenizer = Lexer(lexerInput)
    val receivedTokens = tokenizer.tokenize()
    val parser = Parser(receivedTokens)
    val expected = EqualsExp(IntegerExp(4),IntegerExp(2))
    testParses(receivedTokens,expected, parser.parseExp)
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

  def main(args: Array[String]): Unit = {
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
  } // main
} // LexerTest

