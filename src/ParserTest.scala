object ParserTest {
  def testParses(input: List[Token], expectedProgram: Prgm ) {
    val parser = Parser(input)
    val (received, _) = parser.parseProgram(input)
    assert(received == expectedProgram)
  }

  def testEx() {
    testParses(/* a list of tokens*/, /*a prgm*/)
  }


  def main(args: Array[String]) {

  } // main
} // LexerTest

