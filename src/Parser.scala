


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
case class IntegerExp(value:Int) extends Exp
case class BooleanExp(value: Boolean) extends Exp
case class VariableExp(value: Var) extends Exp
//case class LogicExp(e1:Exp , l1: Logic, e2: Exp) extends Exp
//case class MathExp(e1:Exp , m1: MathOp, e2: Exp) extends Exp
case class PrintExp(e1:Exp) extends Exp
case class MethodExp(e1:Exp , methodName: Variable, e2: Exp*) extends Exp
case class NewClassExp(className: Variable, e1:Exp* ) extends Exp
case class CastExp(t1: Types , e2: Exp) extends Exp
case class GroupedExp(e: Exp) extends Exp
case class HighOrderExp(t1: Types , v1: Variable, e2: Exp) extends Exp
case class CallHighOrderExp(e1:Exp , e2: Exp) extends Exp
case class LTEExp(exp: Exp, value: Exp) extends Exp
case class LTExp(exp: Exp, value: Exp) extends Exp
case class GTEExp(exp: Exp, value: Exp) extends Exp
case class GTExp(exp: Exp, value: Exp) extends Exp
case class AndExp(exp: Exp, value: Exp) extends Exp
case class OrExp(exp: Exp, value: Exp) extends Exp
case class PlusExp(exp: Exp, value: Exp) extends Exp
case class SubtractExp(exp: Exp, value: Exp) extends Exp
case class MultiplyExp(exp: Exp, value: Exp) extends Exp
case class DivideExp(exp: Exp, value: Exp) extends Exp
case class PowerExp(exp: Exp, value: Exp) extends Exp
case class EqualsExp(exp: Exp, value: Exp) extends Exp

sealed trait Variable
case class Var(name:String) extends Variable

sealed trait Stmt
case class ExpStmt(e1: Exp) extends Stmt
case class AssignmentStmt(v1: Variable, exp: Exp) extends Stmt
case class AssignmentStmtVardec(vd1: VarDec, exp: Exp) extends Stmt
case class ForStmt(v1: VarDec, e1: Exp, inc: Stmt, forBody: Stmt) extends Stmt
case object BreakStmt extends Stmt
case class BlockStmt(s1: Stmt) extends Stmt
case class ConditionalStmt(e1: Exp, condition: Stmt, ifBody: Stmt) extends Stmt
case class ReturnStmt(e1: Exp) extends Stmt
case object VoidStmt extends Stmt

sealed trait Method
case class DefMethod(types:Types, methodName: Variable,  stmt: Stmt, parameters: VarDec*) extends Method

sealed trait Instance
case class DecInstance(v1: VarDec) extends Instance

sealed trait ClassBody
case class InstanceClassBody(in1: DecInstance*) extends ClassBody
case class MethodClassBody(md1: DefMethod*) extends ClassBody
case class DeclarationClassBody(vd1: VarDec*) extends ClassBody

sealed trait Class
case class DefClass(v1: Variable, st1: Stmt, cb1: ClassBody*) extends Class
case class DefExtClass(classname: Variable, extendedClass: Variable, st1: Stmt, cb1: ClassBody*) extends Class

sealed trait Program
case class Prgm(e1: Exp, c1: DefClass*) extends Program

case class ParserException(msg: String) extends Exception(msg)

object Parser {
  def apply(input: List[Token]): Parser = {
    new Parser(input)
  }
}

class Parser(private var input: List[Token]) {
  private def parseExp(tokens: List[Token]): (Exp,List[Token]) = {
      try{
        tokens match{
        case PrintToken:: LeftParenToken :: tail => {
          val (printedExp, restTokens) = parseExp(tail)
          restTokens match {
            case RightParenToken::finalTokens =>{
              (PrintExp(printedExp),finalTokens)
            }
            case _ =>{
              throw ParserException("not a print expression")
            }
          }
        }
        case (method: VarToken):: LeftParenToken::tail => {
          val (baseExp, restTokens) = parseExp(tail)
          val (parameters, restTokens2) = parseRep1(restTokens,parseExp,skipCommas)
          restTokens2 match {
            case RightParenToken::tail =>
              (MethodExp(baseExp,Var(method.name),parameters),tail)
          }
        }
        case NewToken :: (className: VarToken) ::LeftParenToken::tail =>{
          val (parameters, restTokens) = parseRep1(tail,parseExp,skipCommas)
          restTokens match {
            case RightParenToken:: tail =>{
              (NewClassExp(Var(className.name),parameters),tail)
            }
          }
        }
        case LeftParenToken ::tail=> {
            val (nextType, restTokens)= parseType(tail)
            restTokens match {
              case RightParenToken::tail => {
                val(expToBeCasted, restTokens2) =  parseExp(tail)
                (CastExp(nextType,expToBeCasted),restTokens2)
              }
              case (highOrderFunction: VarToken)::RightParenToken:: EqualsToken:: GreaterThanToken::tail => {
                val (innerExp, restTokens2) = parseExp(tail)
                (HighOrderExp(nextType,Var(highOrderFunction.name),innerExp),restTokens2)
              }
              case _ => throw ParserException("is not a cast or high order function instantiation")
            }
        }
        case HOFCToken :: tail =>{
          val (preFunction,restTokens) =  parseExp(tail)
          restTokens match {
            case CommaToken:: tail => {
              val (postFunction,restTokens2) = parseExp(tail)
              restTokens2 match {
                case RightParenToken:: tail => {
                  (CallHighOrderExp(preFunction,postFunction), tail)
                }
                case _ => throw ParserException("not a high order function call")
              }
            }
            case _ => throw ParserException("not a high order function call")
          }
        }
        case _ => {
          val (finalExp, restTokens) = parseBinaryOperator(tokens)
          (finalExp,restTokens)
        }
      }
      }catch{
        case _:ParserException =>{
          val (finalExp, restTokens) = parseBinaryOperator(tokens)
          (finalExp,restTokens)

        }
    }
  }

  def cascadifyHelper(expression: Exp, tokens: List[Token], mkClass: (Exp, Exp) => Exp): (Exp, List[Token]) = {
    val (followingExps: List[Exp], restTokens) = parseRepeat(tokens, parseBinaryOperator)
    var finalResult: Exp = expression
    for (currentExp <- followingExps) {
      finalResult = mkClass(finalResult, currentExp)
    }
    (finalResult, restTokens)
  }

  def parseBinaryOperator(tokens: List[Token]): (Exp, List[Token])={
    val (expression,restTokens) = parseAdditiveExpression(tokens)
    def cascadify(tokens: List[Token], mkClass: (Exp, Exp) => Exp): (Exp, List[Token]) = cascadifyHelper(expression, tokens, mkClass)
    restTokens match {
      case LessThanToken::EqualsToken::tail => cascadify(tail,  LTEExp.apply)
      case LessThanToken:: tail =>cascadify(tail,  LTEExp.apply)
      case GreaterThanToken::EqualsToken :: tail => cascadify(tail,  GTEExp.apply)
      case GreaterThanToken:: tail => cascadify(tail,  GTExp.apply)
      case AndToken::AndToken:: tail => cascadify(tail,  AndExp.apply)
      case OrToken::OrToken::tail => cascadify(tail,  OrExp.apply)
      case EqualsToken::EqualsToken::tail => cascadify(tail,  EqualsExp.apply)
    }
  }

  def parseAdditiveExpression(tokens: List[Token]): (Exp, List[Token])={
    val (expression,restTokens) = parseMultiplicativeExpression(tokens)
    def cascadify(tokens: List[Token], mkClass: (Exp, Exp) => Exp): (Exp, List[Token]) = cascadifyHelper(expression, tokens, mkClass)
    restTokens match {
      case PlusToken::tail =>cascadify(tail,  PlusExp.apply)
      case SubtractToken:: tail =>cascadify(tail,  SubtractExp.apply)
    }
  }

  def parseMultiplicativeExpression(tokens: List[Token]): (Exp, List[Token])={
    val (expression,restTokens) = parseExponentialExpression(tokens)
    def cascadify(tokens: List[Token], mkClass: (Exp, Exp) => Exp): (Exp, List[Token]) = cascadifyHelper(expression, tokens, mkClass)
    restTokens match {
      case MultiplicationToken::tail =>cascadify(tail,  MultiplyExp.apply)
      case DivisionToken:: tail =>cascadify(tail,  DivideExp.apply)
    }
  }
  def parseExponentialExpression(tokens: List[Token]): (Exp, List[Token])={
    val (expression,restTokens) = parsePrimaryExpression(tokens)
    def cascadify(tokens: List[Token], mkClass: (Exp, Exp) => Exp): (Exp, List[Token]) = cascadifyHelper(expression, tokens, mkClass)
    restTokens match {
      case CaretToken::tail =>cascadify(tail,  PowerExp.apply)
    }
  }
  def parsePrimaryExpression(tokens: List[Token]): (Exp, List[Token])={
    tokens match {
      case (head:IntegerToken)::tail =>
        (IntegerExp(head.value),tail)
      case (head:BooleanToken)::tail =>
        (BooleanExp(head.name),tail)
      case (head:VarToken)::tail =>
        (VariableExp(Var(head.name)),tail)
      case LeftParenToken::tail  =>{
        val (groupedExpression, restTokens) = parseBinaryOperator(tail)
        restTokens match {
          case RightParenToken::tail =>
            (GroupedExp(groupedExpression),tail)
        }
      }
    }
  }
  def parseGroupedExpression(tokens: List[Token]): (Exp, List[Token])={
    tokens match {
      case LeftParenToken::tail =>{
        val (groupedExp, restTokens) = parseExp(tail)
        restTokens match {
          case RightParenToken :: tail => (GroupedExp(groupedExp),tail)
        }
      }
    }
  }

  def parseType(value: List[Token]):(Types,List[Token]) = {

  }

  def skipCommas(tokens: List[Token]):(Any,List[Token]) = {
    tokens match {
      case CommaToken::restTokens => ("Skipped",restTokens)
      case _ => throw ParserException("No commas to skip")
    }
  }

  def parseRep1[A,B](tokens: List[Token],
                   parseWanted: List[Token] => (A, List[Token]),
                   parseSkip: List[Token] => (B,List[Token])):
              (List[A], List[Token])= {
    try{
      val (a, restTokens) = parseWanted(tokens)
      val(_, restTokens2) = parseRepeat(restTokens, parseSkip)
      val(restAs2, finalTokens) = parseRep1(restTokens2,parseWanted,parseSkip)
      (a:: restAs2, finalTokens)
    }
    catch{
      case _: ParserException => {

        (List(), tokens)
      }
    }

  }


  def parseRepeat[A] (tokens: List[Token], parseOne: List[Token] => (A, List[Token])): (List[A], List[Token]) ={
    try{
      val (a, restTokens) = parseOne(tokens)
      val(restAs, finalRestTokens) = parseRepeat(restTokens, parseOne)
      (a:: restAs, finalRestTokens)
    }
    catch{
      case _: ParserException => (List(), tokens)
    }
  }
}

