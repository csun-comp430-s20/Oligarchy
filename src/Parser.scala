sealed trait MathOp
sealed trait Logic
sealed trait Types
sealed trait VarDec

case object PlusMathOp extends MathOp
case object MinusMathOp extends MathOp
case object MultiplicationMathOp extends MathOp
case object DivisionMathOp extends MathOp
case object CaretMathOp extends MathOp
case object LessThanMathOp extends MathOp
case object GreatThanMathOp extends MathOp
case object LessThanEqualsMathOp extends MathOp
case object GreaterThanEqualsMathOp extends MathOp
case object AndLogic extends Logic
case object OrLogic extends Logic
case object EqualsLogic extends Logic
case object IntTypes extends Types
case object BoolTypes extends Types
case object StrTypes extends Types
case class Declaration(types: Var)extends VarDec