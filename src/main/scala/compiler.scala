import scala.io.Source

object compiler {
  def main(args: Array[String]) {
    var input = "class test{" +
      "bool testMethod(int foo) return true;" +
      "}"

    val filename = "test.txt"
    input = Source.fromFile(filename).getLines().mkString

    val tokenizer = Lexer(input)
    val tokenizerOutput:List[Token] = tokenizer.tokenize()
    val parser = Parser(tokenizerOutput)
    val parserOutput = parser.parseProgram(tokenizerOutput)
    val typechecker = Typechecker(parserOutput._1)
    val codeGen = ClassGenerator(parserOutput._1)
  }
}
