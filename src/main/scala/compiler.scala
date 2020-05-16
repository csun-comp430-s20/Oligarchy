import scala.io.Source

object compiler {
  def main(args: Array[String]) {
//    val filename = "C:\\Users\\edpre\\JavaProjects\\Oligarchy\\src\\main\\scala\\createExtendedClass.txt"
    val filename = "C:\\Users\\edpre\\JavaProjects\\Oligarchy\\src\\main\\scala\\createAndCallHighOrderFunction.txt"
    val source = Source.fromFile(filename)
    val input = source.getLines().mkString
    source.close()
    val tokenizer = Lexer(input)
    val tokenizerOutput:List[Token] = tokenizer.tokenize()
    val parser = Parser(tokenizerOutput)
    val parserOutput = parser.parseProgram(tokenizerOutput)
    val typechecker = Typechecker(parserOutput._1)
    val codeGen = ClassGenerator(parserOutput._1)
    codeGen.writeClasses("C:\\Users\\edpre\\JavaProjects\\Oligarchy\\src\\main\\scala\\")
  }
}
