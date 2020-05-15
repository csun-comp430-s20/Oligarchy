import scala.io.Source

object compiler {
  def main(args: Array[String]) {
    val filename = "/Users/stephanie/Documents/Comp430/Oligarchy/src/main/scala/test.txt"
    val input = Source.fromFile(filename).getLines().mkString

    val tokenizer = Lexer(input)
    val tokenizerOutput:List[Token] = tokenizer.tokenize()
    val parser = Parser(tokenizerOutput)
    val parserOutput = parser.parseProgram(tokenizerOutput)
    val typechecker = Typechecker(parserOutput._1)
    val codeGen = ClassGenerator(parserOutput._1)
    codeGen.apply(parserOutput._1)
    codeGen.writeClasses("/Users/stephanie/Documents/Comp430/Oligarchy/src/main/scala/")
  }
}
