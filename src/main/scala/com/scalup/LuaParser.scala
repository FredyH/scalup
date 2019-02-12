package com.scalup

import java.io.File
import java.nio.file.Files

import com.scalup.LuaToken._

import scala.util.parsing.combinator.PackratParsers

object LuaParser extends LuaLexer with PackratParsers {
  implicit def keywordsAreParsers[T <: KeywordOrOperator](token: T): PackratParser[T] = keyword(token)

  private lazy val name: PackratParser[String] = identifier ^^ {
    _.name
  }

  private lazy val luaNil: PackratParser[Expression] = keyword(NIL) ^^ { _ => LuaNil }

  private lazy val luaString: PackratParser[LuaString] = string ^^ { token => LuaString(token.content, token.raw) }

  private lazy val luaTrue: PackratParser[Expression] = keyword(TRUE) ^^ { _ => LuaTrue }

  private lazy val luaFalse: PackratParser[Expression] = keyword(FALSE) ^^ { _ => LuaFalse }

  private lazy val varArgs: PackratParser[Expression] = keyword(TRIPLE_DOT) ^^ { _ => VarArgs }

  private lazy val luaNumber: PackratParser[Expression] = number ^^ { token => LuaNumber(token.number) }

  private lazy val block: PackratParser[Block] = (statement <~ SEMI_COLON.?).* ~ (lastStatement <~ SEMI_COLON.?).? ^^ {
    case list ~ last => Block(list ++ last)
  }

  private lazy val expression: PackratParser[Expression] = orOperators

  private lazy val statement: PackratParser[Statement] =
    (functionCall ||| globalDeclaration) | doBlock | whileLoop | repeatLoop | ifStatement | forLoop | forEachLoop | functionDefinition |
      localFunctionDefinition | localDeclaration

  private lazy val globalDeclaration: PackratParser[GlobalDeclaration] = (variableList <~ ASSIGNMENT) ~ expressionList ^^ {
    case vars ~ exprs => GlobalDeclaration(vars, exprs)
  }

  private lazy val localDeclaration: PackratParser[LocalDeclaration] =
    (LOCAL ~> nameList) ~ (ASSIGNMENT ~> expressionList).? ^^ {
      case names ~ expressions => LocalDeclaration(names, expressions.getOrElse(Nil))
    }

  private lazy val functionName: PackratParser[FunctionName] = name ~ (DOT ~> name).* ~ (COLON ~> name).? ^^ {
    case first ~ rest ~ None => FunctionName(first :: rest, withSelf = false)
    case first ~ rest ~ Some(last) => FunctionName((first :: rest) :+ last, withSelf = true)
  }

  private lazy val functionDefinition: PackratParser[FunctionDefinition] = (FUNCTION ~> functionName) ~ functionBody ^^ {
    case n ~ b => FunctionDefinition(n, b, false)
  }

  private lazy val localFunctionDefinition: PackratParser[FunctionDefinition] = (LOCAL ~ FUNCTION) ~> name ~ functionBody ^^ {
    case n ~ b => FunctionDefinition(FunctionName(List(n), false), b, true)
  }

  private lazy val elseIfStatement: PackratParser[ElseIfStatement] =
    (ELSEIF ~> expression <~ THEN) ~ block ^^ { case c ~ b => ElseIfStatement(c, b) }

  private lazy val elseStatement: PackratParser[Block] = ELSE ~> block

  private lazy val ifStatement: PackratParser[IfStatement] =
    ((IF ~> expression <~ THEN) ~ block ~ elseIfStatement.* ~ elseStatement.?) <~ END ^^ {
      case ifCond ~ ifBlock ~ elseIfStatements ~ elseOpt => IfStatement(ifCond, ifBlock, elseIfStatements, elseOpt)
    }

  private lazy val doBlock: PackratParser[DoBlock] =
    DO ~> block <~ END ^^ DoBlock

  private lazy val whileLoop: PackratParser[WhileLoop] = ((WHILE ~> expression) <~ DO) ~ (block <~ END) ^^ {
    case condition ~ b => WhileLoop(condition, b)
  }

  private lazy val repeatLoop: PackratParser[RepeatLoop] = ((REPEAT ~> block) <~ UNTIL) ~ expression ^^ {
    case b ~ condition => RepeatLoop(b, condition)
  }

  private lazy val forLoop: PackratParser[ForLoop] =
    (FOR ~> name) ~ (ASSIGNMENT ~> expression) ~ (COMMA ~> expression) ~ (COMMA ~> expression).? ~ (DO ~> block <~ END) ^^ {
      case varName ~ first ~ second ~ thirdOpt ~ b => ForLoop(varName, first, second, thirdOpt, b)
    }

  private lazy val forEachLoop: PackratParser[ForEachLoop] =
    (FOR ~> nameList) ~ (IN ~> expressionList) ~ (DO ~> block <~ END) ^^ {
      case varList ~ exprList ~ b => ForEachLoop(varList, exprList, b)
    }

  private def operatorToken(operators: List[KeywordOrOperator]): PackratParser[KeywordOrOperator] = operators match {
    case Nil => throw new RuntimeException("Empty operator list")
    case x :: xs => operators.foldLeft(x: PackratParser[KeywordOrOperator]) {
      case (parser, token) => parser | token
    }
  }

  private def listOfOperators(currentLevel: => PackratParser[Expression],
                              nextLevel: PackratParser[Expression],
                              operators: List[KeywordOrOperator]): PackratParser[Expression] = {
    nextLevel ~ (operatorToken(operators) ~ currentLevel).* ^^ {
      case first ~ Nil => first
      case first ~ ((firstOp ~ second) :: rest) =>
        rest.foldLeft(BinaryOperator.fromLuaToken(firstOp, first, second)) {
          case (current, op ~ expr) =>
            BinaryOperator.fromLuaToken(op, current, expr)
        }
    }
  }

  private lazy val constants: PackratParser[Expression] = luaNil | luaNumber | luaString | luaTrue | luaFalse | varArgs

  //TODO:
  //lazy val exponentiations: PackratParser[Expression] = ???

  private lazy val unaryOperators: PackratParser[Expression] = operatorToken(List(NUMBER_SIGN, MINUS, LOGICAL_NOT)) ~ unaryOperators ^^ {
    case NUMBER_SIGN ~ expr => UnaryCount(expr)
    case MINUS ~ expr => UnaryMinus(expr)
    case LOGICAL_NOT ~ expr => UnaryNot(expr)
    case _ => ??? // Can't happen
  }
  private lazy val unaryTerms: PackratParser[Expression] = prefixExpression | constants | function | tableConstructor | (PAR_OPEN ~> orOperators <~ PAR_CLOSE) | unaryOperators

  private lazy val factors: PackratParser[Expression] =
    listOfOperators(factors, unaryTerms, List(MULTIPLY, DIVIDED_BY, MODULUS))

  private lazy val terms: PackratParser[Expression] =
    listOfOperators(terms, factors, List(PLUS, MINUS))

  //TODO: This is right associative
  private lazy val concatenations: PackratParser[Expression] =
    listOfOperators(concatenations, terms, List(CONCATENATION))
  private lazy val comparisons: PackratParser[Expression] =
    listOfOperators(comparisons, concatenations, List(LESS_THAN, LESS_EQUALS, GREATER_THAN, GREATER_EQUALS, EQUALS, NOT_EQUALS))
  private lazy val andOperators: PackratParser[Expression] =
    listOfOperators(andOperators, comparisons, List(LOGICAL_AND))
  private lazy val orOperators: PackratParser[Expression] =
    listOfOperators(orOperators, andOperators, List(LOGICAL_OR))

  private lazy val functionCall: PackratParser[FunctionCall] =
    (prefixExpression ~ argumentList ^^ { case prefix ~ args => FunctionCallWithoutSelf(prefix, args) }) |
      ((prefixExpression <~ COLON) ~ name ~ argumentList ^^ {
        case prefix ~ n ~ args => FunctionCallWithSelf(prefix, n, args)
      })

  private lazy val prefixExpression: PackratParser[PrefixExpression] =
    (variable ||| functionCall) | ((PAR_OPEN ~> expression <~ PAR_CLOSE) ^^ ExpressionAsPrefix)

  private lazy val fieldWithIndex: PackratParser[Field] = ((INDEX_OPEN ~> expression <~ INDEX_CLOSE) <~ ASSIGNMENT) ~ expression ^^ {
    case indexExpr ~ valueExpr => FieldByIndex(Some(indexExpr), valueExpr)
  }

  private lazy val fieldWithName: PackratParser[Field] = (name <~ ASSIGNMENT) ~ expression ^^ {
    case n ~ expr => FieldByName(n, expr)
  }

  private lazy val fieldWithoutIndex: PackratParser[Field] = expression ^^ {
    expr => FieldWithoutIndex(expr)
  }

  private lazy val field: PackratParser[Field] = fieldWithIndex | fieldWithName | fieldWithoutIndex

  private lazy val fieldSeparator: PackratParser[Unit] = (SEMI_COLON | COMMA) ^^ { _ => () }

  private lazy val fieldList: PackratParser[List[Field]] = (field ~ (fieldSeparator ~> field).*) <~ fieldSeparator.? ^^ {
    case first ~ list => first :: list
  }

  private lazy val tableConstructor: PackratParser[TableConstructor] =
    TABLE_OPEN ~> fieldList.? <~ TABLE_CLOSE ^^ {
      list => TableConstructor(list.getOrElse(Nil))
    }

  private lazy val functionBody: PackratParser[FunctionBody] =
    (PAR_OPEN ~> parameterList.? <~ PAR_CLOSE) ~ (block <~ END) ^^ {
      case list ~ functionBlock => FunctionBody(list.getOrElse(ParameterList(Nil, false)), functionBlock)
    }

  private lazy val function: PackratParser[FunctionExpression] = FUNCTION ~> functionBody ^^ { body => FunctionExpression(body) }

  private lazy val parameterListWithVarArgs: PackratParser[ParameterList] = (nameList ~ (COMMA ~> TRIPLE_DOT).?) ^^ {
    case list ~ _ => ParameterList(list.map(Parameter), true)
  }

  private lazy val parameterListWithoutVarArgs: PackratParser[ParameterList] = TRIPLE_DOT ^^ { _ => ParameterList(Nil, false) }

  private lazy val parameterList: PackratParser[ParameterList] = parameterListWithoutVarArgs | parameterListWithVarArgs

  private lazy val argumentList: PackratParser[List[Expression]] =
    ((PAR_OPEN ~> expressionList.? <~ PAR_CLOSE) ^^ { l => l.getOrElse(Nil) }) |
      (tableConstructor ^^ { tc => List(tc) }) |
      (luaString ^^ { tc => List(tc) })

  private lazy val expressionList: PackratParser[List[Expression]] = expression ~ (COMMA ~> expression).* ^^ {
    case first ~ rest => first :: rest
  }

  private lazy val nameList: PackratParser[List[String]] = name ~ (COMMA ~> name).* ^^ {
    case first ~ rest => first :: rest
  }

  private lazy val variableWithDot: PackratParser[Variable] = log(prefixExpression <~ DOT)("PREFIX") ~ name ^^ {
    case prefix ~ n => PrefixedVariable(prefix, n)
  }

  private lazy val variableWithIndex: PackratParser[Variable] =
    prefixExpression ~ (INDEX_OPEN ~> expression <~ INDEX_CLOSE) ^^ {
      case prefix ~ expr => IndexedVariable(prefix, expr)
    }

  private lazy val variable: PackratParser[Variable] =
    variableWithDot | variableWithIndex | (name ^^ { n => NamedVariable(n) })

  private lazy val variableList: PackratParser[List[Variable]] = variable ~ (COMMA ~> variable).* ^^ {
    case first ~ rest => first :: rest
  }

  private lazy val breakStatement: PackratParser[LastStatement] = BREAK ^^ { _ => BreakStatement }
  private lazy val continueStatement: PackratParser[LastStatement] = CONTINUE ^^ { _ => ContinueStatement }
  private lazy val returnStatement: PackratParser[LastStatement] = RETURN ~> expressionList ^^ {
    list => ReturnStatement(list)
  }

  private lazy val lastStatement: PackratParser[LastStatement] = breakStatement | continueStatement | returnStatement

  def main(args: Array[String]): Unit = {
    val input = new String(Files.readAllBytes(new File("test.lua").toPath))
    parseAll(functionCall, input) match {
      case NoSuccess(msg, next) => println("LOL FAIL", msg)
      case Success(result, next) => println(result)
    }
  }
}
