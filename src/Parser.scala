
sealed trait MathOp
case object PlusMathOp extends MathOp
case object MinusMathOp extends MathOp
case object MultiplicationMathOp extends MathOp
case object DivisionMathOp extends MathOp
case object CaretMathOp extends MathOp
case object LessThanMathOp extends MathOp
case object GreatThanMathOp extends MathOp
case object LessThanEqualsMathOp extends MathOp
case object GreaterThanEqualsMathOp extends MathOp

sealed trait Logic
case object AndLogic extends Logic
case object OrLogic extends Logic
case object EqualsLogic extends Logic

sealed trait Types
case object IntTypes extends Types
case object BoolTypes extends Types
case object StrTypes extends Types

sealed trait VarDec
case class Declaration(types: Var)extends VarDec

sealed trait Exp
case class LogicExp(e1:Exp , l1: Logic, e2: Exp) extends Exp
case class MathExp(e1:Exp , m1: MathOp, e2: Exp) extends Exp
case class PrintExp(e1:Exp) extends Exp
case class MethodExp(e1:Exp , methodName: Variable, e2: Exp) extends Exp
case class NewClassExp(className: Variable, e1:Exp* ) extends Exp
case class CastExp(t1: Types , e2: Exp) extends Exp
case class HighOrderExp(t1: Types , v1: Variable, e2: Exp) extends Exp
case class CallHighOrderExp(e1:Exp , e2: Exp) extends Exp

sealed trait Variable
case class Var(name:String) extends Variable




case class ParserException(msg: String) extends Exception(msg)

object Parser {
  def apply(input: Seq[Token]): Parser = {
    new Parser(input)
  }
}

class Parser(private var input: Seq[Token]) {

}