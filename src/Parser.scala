sealed trait Types

case object IntTypes extends Types

case object BoolTypes extends Types

case object StrTypes extends Types

case object VoidTypes extends Types // should be removed

case class ClassTypes(className: String) extends Types

case class MethodTypes(paramTypes: List[Types] , returnTypes: Types) extends Types







case class VarDeclaration(types: Types, varName: String)





sealed trait Exp

case class IntegerExp(value:Int) extends Exp

case class StringExp(value:String) extends Exp

case class BooleanExp(value: Boolean) extends Exp

case class VariableExp(value: String) extends Exp

case class PrintExp(e1:Exp) extends Exp

case class MethodExp(e1:Exp , methodName: String, e2: List[Exp]) extends Exp

//@todo lets covert it so that it is just a list of expressions IE parameters

//case class MethodExp(methodName: String, parameters: List[Exp]) extends Exp

case class NewClassExp(className: String, e1:List[Exp] ) extends Exp

//@todo changed t1 to newTypes

case class CastExp(newTypes: Types , e2: Exp) extends Exp

case class GroupedExp(e: Exp) extends Exp

//@todo changed to have parameters declared followed by the experession

case class HighOrderExp(params: Seq[VarDeclaration], exp: Exp) extends Exp

//@todo changed exp1  to function and exp2 to params and so that it takes multiple parameters

case class CallHighOrderExp(function: Exp, params: Seq[Exp]) extends Exp

case class LTEExp(leftExp: Exp, rightExp: Exp) extends Exp

case class LTExp(leftExp: Exp, rightExp: Exp) extends Exp

case class GTEExp(leftExp: Exp, rightExp: Exp) extends Exp

case class GTExp(leftExp: Exp, rightExp: Exp) extends Exp

case class AndExp(leftExp: Exp, rightExp: Exp) extends Exp

case class OrExp(leftExp: Exp, rightExp: Exp) extends Exp

case class PlusExp(leftExp: Exp, rightExp: Exp) extends Exp

case class SubtractExp(leftExp: Exp, rightExp: Exp) extends Exp

case class MultiplyExp(leftExp: Exp, rightExp: Exp) extends Exp

case class DivideExp(leftExp: Exp, rightExp: Exp) extends Exp

case class PowerExp(leftExp: Exp, rightExp: Exp) extends Exp

case class EqualsExp(leftExp: Exp, rightExp: Exp) extends Exp



sealed trait Stmt

case class ExpStmt(e1: Exp) extends Stmt

case class AssignmentStmt(varDec: VarDeclaration, exp: Exp) extends Stmt

case class ForStmt(assign: Stmt, e1: Exp, inc: Stmt, forBody: Stmt) extends Stmt

case object BreakStmt extends Stmt

case class BlockStmt(statements: List[Stmt]) extends Stmt

case class ConditionalStmt(condition: Exp, ifTrue: Stmt, ifFalse: Stmt) extends Stmt

case class ReturnStmt(returnExp: Exp) extends Stmt

case object VoidStmt extends Stmt

case class VarStmt(variableName:String, newValue:Exp) extends Stmt





case class MethodDef(types:Types, methodName: String,  stmt: Stmt, parameters: List[VarDeclaration], returnExpression: Exp)



case class InstanceDec(v1: VarDeclaration)



sealed trait Class

case class DefClass(className: String,

                    statements: Stmt,

                    instance: List[InstanceDec],

                    parameters: List[VarDeclaration],

                    methods: List[MethodDef]) extends Class

case class DefExtClass(classname: String,

                       extendedClass: String,

                       statements: Stmt,

                       instances: List[InstanceDec],

                       parameters: List[VarDeclaration],

                       methods: List[MethodDef]) extends Class







case class Program(entryPoint: Exp, classes: List[Class])


case class ParserException(msg: String) extends Exception(msg)

object Parser {
  def apply(input: List[Token]): Parser = {
    new Parser(input)
  }
}

class Parser(private var input: List[Token]) {

  def parseTypes(tokens: List[Token]): (Types, List[Token]) = {
    tokens match {
      case IntTypeToken :: tail =>
        (IntTypes, tail)
      case StringTypeToken :: tail =>
        (StrTypes, tail)
      case BooleanTypeToken :: tail =>
        (BoolTypes, tail)
      case (className: VarToken) :: tail =>
        (ClassTypes((className.name)), tail)
      case VoidToken :: tail =>
        (VoidTypes, tail)
      case _ => throw ParserException("not a type")
    }
  }

  def parseVarDec(tokens: List[Token]): (VarDec, List[Token]) = {
    val (types, restTokens) = parseTypes(tokens)
    restTokens match {
      case (variable: VarToken) :: tail => {
        (VarDeclaration(types, (variable.name)), tail)
      }
      case _ => throw ParserException("not a var dec")
    }
  }

  def parseInstanceDec(tokens: List[Token]): (Instance, List[Token]) = {
    val (varDec, restTokens) = parseVarDec(tokens)
    restTokens match {
      case SemicolonToken::returnTokens =>(DecInstance(varDec), returnTokens)
      case _ => throw ParserException("missing semicolon")
    }

  }


  def parseMethodDef(tokens: List[Token]): (Method, List[Token]) = {
    val (types, restTokens) = parseTypes(tokens)
    //testing daniel
    //println(types)
    restTokens match {
      case (variable: VarToken) :: tail => {
        tail match {
          case LeftParenToken :: tail => {
            val (vardeclarations, restokens2) = parseRep1(tail, parseVarDec, skipCommas)
            restokens2 match {
              case RightParenToken :: restokens2 => {
                val (stmt, restokens3) = parseStmt(restokens2)
                (DefMethod(types, variable.name, stmt, vardeclarations), restokens3)
              }
            }
          }
        }
      }
    }
  }

  //Daniel
  def parseProgram(tokens: List[Token]): (Prgm, List[Token]) = {
    val (classes: List[Class], restTokens: List[Token]) = parseRepeat(tokens, parseClass)  //TODO
    val (exp: Exp, restTokens2: List[Token]) = parseExp(restTokens)
    (Prgm(exp, classes), restTokens2)
  } //ParseProgram

  //Daniel
  def parseClass(tokens: List[Token]): (Class, List[Token]) = {
    tokens match {
      case ClassToken :: VarToken(classname: String) :: ExtendsToken :: VarToken(extendclassname: String) :: LeftCurlyToken :: tail => {
        var (instances: List[Instance], restTokens: List[Token]) = parseRepeat(tail, parseInstanceDec)
        restTokens match {
          case ConstructorToken :: LeftParenToken :: tail => {
            var (declarations: List[VarDeclaration], restTokens2: List[Token]) = parseRepeat(tail, parseVarDec)
            restTokens2 match {
              //daniel modified: case RightParenToken :: tail => {  TODO
              case RightParenToken :: tail => {
                val (stmt, restTokens3) = parseStmt(tail)
                var (methods: List[Method], restTokens4: List[Token]) = parseRepeat(restTokens3, parseMethodDef)
                restTokens4 match {
                  case RightCurlyToken :: tail => {
                    (DefExtClass(classname, extendclassname, stmt, instances, declarations, methods), tail)
                  }
                }
              }
              case _ => throw ParserException("Not a DefExtClass")
            }
          }
          case _ => throw ParserException("Expected Constructor and LeftParen")
        }
      } //Extended Class
      case ClassToken :: VarToken(classname: String) :: LeftCurlyToken :: tail => {
        var (instances: List[Instance], restTokens: List[Token]) = parseRepeat(tail, parseInstanceDec)
        restTokens match {
          case ConstructorToken :: LeftParenToken :: tail => {
            var (declarations: List[VarDec], restTokens2: List[Token]) = parseRepeat(tail, parseVarDec)
            restTokens2 match {
              case RightParenToken :: tail => {
                val (stmt, restTokens3) = parseStmt(tail)
                var (methods: List[Method], restTokens4: List[Token]) = parseRepeat(restTokens3, parseMethodDef)
                restTokens4 match {
                  case RightCurlyToken ::afterClassTokens => (DefClass(classname, stmt, instances, declarations, methods), afterClassTokens)
                }

              }
              case _ => throw ParserException("Not a DefClass")
            }
          }
          case _ => throw ParserException("Expected Constructor and LeftParen")
        }
      } //Basic Class
      case _ => throw ParserException("Not a proper Class Definition")
    }
  } //Parse Classes

  def parseStmt(tokens: List[Token]): (Stmt, List[Token]) = {
    tokens match { //for (vardec; exp; stmt) stmt
      case ForToken :: LeftParenToken :: tail => {
        val (stmt: Stmt, restTokens: List[Token]) = parseStmt(tail)
        val (exp: Exp, restTokens2: List[Token]) = parseExp(restTokens)
        restTokens2 match {
          case SemicolonToken :: restTokens3 => {
            val (stmt1: Stmt, restTokens4: List[Token]) = parseStmt(restTokens3)
            restTokens4 match {
              case RightParenToken :: restTokens5 => {
                val (stmt2: Stmt, finalTokens: List[Token]) = parseStmt(restTokens5)
                (ForStmt(stmt, exp, stmt1, stmt2), finalTokens)
              }
              case _ => throw ParserException("missing RightParenToken in ForStatement")
            }
          }
          case _ => throw ParserException("missing semicolon after exp in ForStatement")
        }
      }

      case BreakToken :: SemicolonToken :: tail => {
        (BreakStmt, tail)
      }
      case IfToken :: LeftParenToken :: tail => {
        val (exp: Exp, restTokens: List[Token]) = parseExp(tail)
        restTokens match {
          case RightParenToken :: restTokens2 => {
            val (stmt1: Stmt, restTokens3: List[Token]) = parseStmt(restTokens2)
            restTokens3 match {
              case ElseToken :: restTokens4 => {
                val (stmt2: Stmt, finalTokens: List[Token]) = parseStmt(restTokens4)
                (ConditionalStmt(exp, stmt1, stmt2), finalTokens)
              }
              case _ => throw ParserException("Missing ElseToken")
            }
          }
          case _ => throw ParserException("missing RightParen Token after Exp in If Statment")
        }
      }
      case ReturnToken :: tail => {
        tail match {
          case SemicolonToken :: restTokens => {
            (VoidStmt, restTokens)
          }
          case _ => {
            try {
              val (exp: Exp, restTokens: List[Token]) = parseExp(tail)
              restTokens match {
                case SemicolonToken :: finalTokens => {
                  (ReturnStmt(exp), finalTokens)
                }
                case _ => throw ParserException("No SemicolonToken after expression in return statement")
              }
            }
            catch {
              case _ => throw ParserException("Invalid return statement")
            }
          }
        }
      }
      case LeftCurlyToken :: tail => {
        val (stmts: List[Stmt], restTokens: List[Token]) = parseRepeat(tail, parseStmt)
        restTokens match {
          case RightCurlyToken :: finalTokens => {
            (BlockStmt(stmts), finalTokens)
          }
        }
      }
      case (varName : VarToken) :: EqualsToken :: restTokens=>{
        val (exp: Exp, restTokens2: List[Token]) = parseExp(restTokens)
        restTokens2 match{
          case SemicolonToken :: finalTokens =>{
            (VarStmt(varName.name, exp), finalTokens)
          }
        }
      }
      case _ => {
        try {
          val (exp: Exp, finalTokens: List[Token]) = parseExp(tokens)
          finalTokens match {
            case SemicolonToken :: finalTokens => {
              (ExpStmt(exp), finalTokens)
            }
            case _ => throw ParserException("no SemicolonToken after expression in Exp Statement")
          }
        }
        catch {
          case _ : ParserException => {
            try {
              val (vardec: VarDec, restTokens: List[Token]) = parseVarDec(tokens)
              restTokens match {
                case EqualsToken :: restTokens2 => {
                  val (exp: Exp, restTokens3: List[Token]) = parseExp(restTokens2)
                  restTokens3 match {
                    case SemicolonToken :: finalTokens => {
                      (AssignmentStmt(vardec, exp), finalTokens)
                    }
                    case _ => throw ParserException("No SemicolonToken after expression in AssignmentStatement")
                  }
                }
                case _ => throw ParserException("No EqualsToken in Assignment Statement")
              }
            }
            catch {
              case _ => throw ParserException("Not Statement")
            }
          }
          case e => {
            throw e
          }
        }
      }
    }
  }
  def parseExp(tokens: List[Token]): (Exp,List[Token]) = {
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
          restTokens match {
            case CommaToken::afterCommaTail => {
              val (parameters, restTokens2) = parseRep1(afterCommaTail,parseExp,skipCommas)
              restTokens2 match {
                case RightParenToken::tail => (MethodExp(baseExp, method.name, parameters), tail)
                case _ => throw ParserException("not a method expression")
              }
            }
            case RightParenToken::tail => (MethodExp(baseExp, method.name, List()), tail)
            case _ => throw ParserException("not a method expression")
          }
        }
        case NewToken :: (className: VarToken) ::LeftParenToken::tail =>{
          val (parameters, restTokens) = parseRep1(tail,parseExp,skipCommas)
          restTokens match {
            case RightParenToken:: tail =>{
              (NewClassExp(className.name,parameters),tail)
            }
            case _ => throw ParserException("miss NewToken ")
          }
        }
        case LeftParenToken ::tail=> {
            val (nextType, restTokens)= parseTypes(tail)
            restTokens match {
              case RightParenToken::tail => {
                val(expToBeCasted, restTokens2) =  parseExp(tail)
                (CastExp(nextType,expToBeCasted),restTokens2)
              }
              case (highOrderFunction: VarToken)::RightParenToken:: EqualsToken:: GreaterThanToken::tail => {
                val (innerExp, restTokens2) = parseExp(tail)
                (HighOrderExp(nextType,highOrderFunction.name,innerExp),restTokens2)
              }
              case _ => throw ParserException("is not a cast or high order function instantiation")
            }
        }
        case HOFCToken :: LeftParenToken::tail =>{
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
        case _ => throw ParserException("Probably Not an Expression")
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

//  def parseAdditiveExpHelper(tokens: List[Token]):(Exp, List[Token])={
//    var result: List[Exp] = List()
//    var break: Boolean = true
//    while (break) {
//      try {
//
//      } catch {
//        case _:ParserException =>
//        break = false;
//      }
//    }
//  } // parseAdditiveExpHelper
  def parseBinaryOperator(tokens: List[Token]): (Exp, List[Token])={
    val (expression,restTokens) = parseAdditiveExpression(tokens)

    def cascadify(tokens: List[Token], mkClass: (Exp, Exp) => Exp): (Exp, List[Token]) = cascadifyHelper(expression, tokens, mkClass)
    restTokens match {
      case LessThanToken::EqualsToken::tail => cascadify(tail,  LTEExp.apply)
      case LessThanToken:: tail =>cascadify(tail,  LTExp.apply)
      case GreaterThanToken::EqualsToken :: tail => cascadify(tail,  GTEExp.apply)
      case GreaterThanToken:: tail => cascadify(tail,  GTExp.apply)
      case AndToken::AndToken:: tail => cascadify(tail,  AndExp.apply)
      case OrToken::OrToken::tail => cascadify(tail,  OrExp.apply)
      case EqualsToken::EqualsToken::tail => cascadify(tail,  EqualsExp.apply)
      case _ =>(expression,restTokens)
    }
  }

  def parseAdditiveExpression(tokens: List[Token]): (Exp, List[Token])={
    val (expression,restTokens) = parseMultiplicativeExpression(tokens)
    def cascadify(tokens: List[Token], mkClass: (Exp, Exp) => Exp): (Exp, List[Token]) = cascadifyHelper(expression, tokens, mkClass)
    restTokens match {
      case PlusToken::tail =>cascadify(tail,  PlusExp.apply)
      case SubtractToken:: tail =>cascadify(tail,  SubtractExp.apply)
      case _ => (expression,restTokens)
    }
  }

  def parseMultiplicativeExpression(tokens: List[Token]): (Exp, List[Token])={
    val (expression,restTokens) = parseExponentialExpression(tokens)
    def cascadify(tokens: List[Token], mkClass: (Exp, Exp) => Exp): (Exp, List[Token]) = cascadifyHelper(expression, tokens, mkClass)
    restTokens match {
      case MultiplicationToken::tail =>cascadify(tail,  MultiplyExp.apply)
      case DivisionToken:: tail =>cascadify(tail,  DivideExp.apply)
      case _ => (expression,restTokens)
    }
  }
  def parseExponentialExpression(tokens: List[Token]): (Exp, List[Token])={
    val (expression,restTokens) = parsePrimaryExpression(tokens)
    def cascadify(tokens: List[Token], mkClass: (Exp, Exp) => Exp): (Exp, List[Token]) = cascadifyHelper(expression, tokens, mkClass)

    restTokens match {
      case CaretToken::tail => cascadify(tail,  PowerExp.apply)
      case _ => (expression,restTokens)
    }
  }
  def parsePrimaryExpression(tokens: List[Token]): (Exp, List[Token])= {
    tokens match {
      case (head: IntegerToken) :: tail =>
        (IntegerExp(head.value), tail)
      case (head: BooleanToken) :: tail =>
        (BooleanExp(head.name), tail)
      case (head: StrToken) :: tail =>
        (StringExp(head.value), tail)
      case (head: VarToken) :: tail =>
        (VariableExp(head.name), tail)
      case LeftParenToken :: tail =>
        val (groupedExpression, restTokens) = parseBinaryOperator(tail)
        restTokens match {
          case RightParenToken :: tail => (GroupedExp(groupedExpression), tail)
          case _ => throw ParserException("miss grouped expression")
        }
      case _ => throw ParserException("not a primary expression")
    }
  }
  def parseGroupedExpression(tokens: List[Token]): (Exp, List[Token])={
    tokens match {
      case LeftParenToken::tail =>{
        val (groupedExp, restTokens) = parseExp(tail)
        restTokens match {
          case RightParenToken :: tail => {
            (GroupedExp(groupedExp),tail)
          }
          case _ => throw ParserException("missing grouped expression")
        }
      }
      case _ => throw ParserException("not a grouped expression")
    }
  }
        
  private def skipCommas(tokens: List[Token]):(Any,List[Token]) = {
    tokens match {
      case CommaToken::restTokens => ("Skipped",restTokens)
      case _ => throw ParserException("No commas to skip")
    }
  }

  private def parseRep1[A,B](tokens: List[Token],
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
  private def parseRepeat[A] (tokens: List[Token], parseOne: List[Token] => (A, List[Token])): (List[A], List[Token]) ={
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
