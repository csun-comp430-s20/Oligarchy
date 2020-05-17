import scala.io.Source

object compiler {
  def main(args: Array[String]) {
//    val filenameS = "C:\\Users\\edpre\\JavaProjects\\Oligarchy\\src\\main\\scala\\createExtendedClass.txt"
//    val filenameS = "/Users/stephanie/Documents/Comp430/Oligarchy/src/main/scala/ForLoopStatement.txt"
    val filenameS = "C:\\Users\\edpre\\JavaProjects\\Oligarchy\\src\\main\\scala\\createAndCallHighOrderFunction.txt"
    val source = Source.fromFile(filenameS)
    val input = source.getLines().mkString
    val tokenizer = Lexer(input)
    val tokenizerOutput:List[Token] = tokenizer.tokenize()
    val parser = Parser(tokenizerOutput)
    val parserOutput = parser.parseProgram(tokenizerOutput)
    val typechecker = Typechecker(parserOutput._1)
    val codeGen = ClassGenerator(parserOutput._1)
    val pathE = "C:\\Users\\edpre\\JavaProjects\\Oligarchy\\src\\main\\scala\\"
//    val pathS = "/Users/stephanie/Documents/Comp430/Oligarchy/src/main/scala/"
//    codeGen.writeClasses(pathS)
    codeGen.writeClasses(pathE)
  }
}
