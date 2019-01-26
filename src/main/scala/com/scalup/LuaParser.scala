package com.scalup

import com.scalup.LuaToken._

import scala.util.parsing.combinator.{PackratParsers, Parsers, RegexParsers}

object LuaParser extends Parsers with PackratParsers {
  override type Elem = LuaToken

  private lazy val name: Parser[String] = {
    accept("name", { case id: IDENTIFIER => id.name })
  }

  private lazy val luaNil: Parser[Expression] = {
    accept("nil", { case NIL => LuaNil })
  }

  private lazy val luaString: Parser[LuaString] = {
    accept("name", { case id: STRING => LuaString(id.content, id.multiline) })
  }

  private lazy val luaTrue: Parser[Expression] = {
    accept("true", { case TRUE => LuaTrue })
  }

  private lazy val luaFalse: Parser[Expression] = {
    accept("false", { case FALSE => LuaFalse })
  }

  private lazy val varArgs: Parser[Expression] = {
    accept("false", { case TRIPLE_DOT => VarArgs })
  }

  private lazy val luaNumber: Parser[Expression] = {
    accept("number", { case n: NUMBER => LuaNumber(n.number) })
  }

  lazy val block: PackratParser[Block] = (statement <~ SEMI_COLON.?).* ~ (lastStatement <~ SEMI_COLON.?).? ^^ {
    case list ~ last => Block(list ++ last)
  }

  lazy val expression: PackratParser[Expression] =
    luaNil | luaFalse | luaTrue | luaNumber | luaString | varArgs | function | prefixExpression | tableConstructor |
      binaryOperator | unaryOperator

  lazy val statement: PackratParser[Statement] =
    doBlock | whileLoop | repeatLoop | ifStatement | forLoop | forEachLoop | functionDefinition |
      localFunctionDefinition | localDeclaration

  lazy val localDeclaration: PackratParser[LocalDeclaration] =
    (LOCAL ~> nameList) ~ (EQUALS ~> expressionList).? ^^ {
      case names ~ expressions => LocalDeclaration(names, expressions.getOrElse(Nil))
    }

  lazy val functionName: PackratParser[FunctionName] = name ~ (DOT ~> name).* ~ (COLON ~> name).? ^^ {
    case first ~ rest ~ None => FunctionName(first :: rest, withSelf = false)
    case first ~ rest ~ Some(last) => FunctionName((first :: rest) :+ last, withSelf = true)
  }

  lazy val functionDefinition: PackratParser[FunctionDefinition] = (FUNCTION ~> functionName) ~ functionBody ^^ {
    case n ~ b => FunctionDefinition(n, b, false)
  }

  lazy val localFunctionDefinition: PackratParser[FunctionDefinition] = (LOCAL ~ FUNCTION) ~> name ~ functionBody ^^ {
    case n ~ b => FunctionDefinition(FunctionName(List(n), false), b, true)
  }

  lazy val elseIfStatement: PackratParser[ElseIfStatement] =
    (ELSEIF ~> expression <~ THEN) ~ block ^^ { case c ~ b => ElseIfStatement(c, b) }

  lazy val elseStatement: PackratParser[Block] = ELSE ~> block

  lazy val ifStatement: PackratParser[IfStatement] =
    ((IF ~> expression <~ THEN) ~ block ~ elseIfStatement.* ~ elseStatement.?) <~ END ^^ {
      case ifCond ~ ifBlock ~ elseIfStatements ~ elseOpt => IfStatement(ifCond, ifBlock, elseIfStatements, elseOpt)
    }

  lazy val doBlock: PackratParser[DoBlock] = DO ~> block <~ END ^^ { b =>
    DoBlock(b)
  }

  lazy val whileLoop: PackratParser[WhileLoop] = ((WHILE ~> expression) <~ DO) ~ (block <~ END) ^^ {
    case condition ~ b => WhileLoop(condition, b)
  }

  lazy val repeatLoop: PackratParser[RepeatLoop] = ((REPEAT ~> block) <~ UNTIL) ~ expression ^^ {
    case b ~ condition => RepeatLoop(b, condition)
  }

  lazy val forLoop: PackratParser[ForLoop] =
    (FOR ~> name) ~ (EQUALS ~> expression) ~ (COMMA ~> expression) ~ (COMMA ~> expression).? ~ (DO ~> block <~ END) ^^ {
      case varName ~ first ~ second ~ None ~ b => ForLoop(varName, first, None, second, b)
      case varName ~ first ~ second ~ Some(third) ~ b => ForLoop(varName, first, Some(third), second, b)
    }

  lazy val forEachLoop: PackratParser[ForEachLoop] =
    (FOR ~> nameList) ~ (IN ~> expressionList) ~ (DO ~> block <~ END) ^^ {
      case varList ~ exprList ~ b => ForEachLoop(varList, exprList, b)
    }

  lazy val binaryOperator: PackratParser[BinaryOperator] = ???

  lazy val unaryMinus: PackratParser[UnaryMinus] = MINUS ~> expression ^^ UnaryMinus
  lazy val unaryNot: PackratParser[UnaryNot] = LOGICAL_NOT ~> expression ^^ UnaryNot
  lazy val unaryCount: PackratParser[UnaryCount] = MINUS ~> expression ^^ UnaryCount
  lazy val unaryOperator: PackratParser[UnaryOperator] = unaryMinus | unaryNot | unaryCount

  lazy val functionCall: PackratParser[FunctionCall] =
    (prefixExpression ~ argumentList ^^ { case prefix ~ args => FunctionCallWithoutSelf(prefix, args) }) |
      ((prefixExpression <~ SEMI_COLON) ~ name ~ argumentList ^^ {
        case prefix ~ n ~ args => FunctionCallWithSelf(prefix, n, args)
      })

  lazy val prefixExpression: PackratParser[PrefixExpression] =
    variable | functionCall | ((PAR_OPEN ~> expression <~ PAR_CLOSE) ^^ ExpressionAsPrefix)

  lazy val fieldWithIndex: PackratParser[Field] = ((INDEX_OPEN ~> expression <~ INDEX_CLOSE) <~ EQUALS) ~ expression ^^ {
    case indexExpr ~ valueExpr => FieldByIndex(Some(indexExpr), valueExpr)
  }

  lazy val fieldWithName: PackratParser[Field] = (name <~ EQUALS) ~ expression ^^ {
    case n ~ expr => FieldByName(n, expr)
  }

  lazy val fieldWithoutIndex: PackratParser[Field] = expression ^^ {
    expr => FieldWithoutIndex(expr)
  }

  lazy val field: PackratParser[Field] = fieldWithIndex | fieldWithName | fieldWithoutIndex

  lazy val fieldSeparator: PackratParser[Unit] = (SEMI_COLON | COMMA) ^^ { _ => () }

  lazy val fieldList: PackratParser[List[Field]] = (field ~ (fieldSeparator ~> field).*) <~ fieldSeparator.? ^^ {
    case first ~ list => first :: list
  }

  lazy val tableConstructor: PackratParser[TableConstructor] =
    TABLE_OPEN ~> fieldList.? <~ TABLE_CLOSE ^^ {
      list => TableConstructor(list.getOrElse(Nil))
    }

  lazy val functionBody: PackratParser[FunctionBody] =
    (PAR_OPEN ~> parameterList.? <~ PAR_CLOSE) ~ (block <~ END) ^^ {
      case list ~ functionBlock => FunctionBody(list.getOrElse(ParameterList(Nil, false)), functionBlock)
    }

  lazy val function: PackratParser[FunctionExpression] = FUNCTION ~> functionBody ^^ { body => FunctionExpression(body) }

  lazy val parameterListWithVarArgs: PackratParser[ParameterList] = (nameList ~ (COMMA ~> TRIPLE_DOT).?) ^^ {
    case list ~ _ => ParameterList(list.map(Parameter), true)
  }

  lazy val parameterListWithoutVarArgs: PackratParser[ParameterList] = TRIPLE_DOT ^^ { _ => ParameterList(Nil, false) }

  lazy val parameterList: PackratParser[ParameterList] = parameterListWithoutVarArgs | parameterListWithVarArgs

  lazy val argumentList: PackratParser[List[Expression]] =
    ((PAR_OPEN ~> expressionList.? <~ PAR_CLOSE) ^^ { l => l.getOrElse(Nil) }) |
      (tableConstructor ^^ { tc => List(tc) }) |
      (luaString ^^ { tc => List(tc) })

  lazy val expressionList: PackratParser[List[Expression]] = expression ~ (COMMA ~> expression).* ^^ {
    case first ~ rest => first :: rest
  }

  lazy val nameList: PackratParser[List[String]] = name ~ (COMMA ~> name).* ^^ {
    case first ~ rest => first :: rest
  }

  lazy val variableWithDot: PackratParser[Variable] = (prefixExpression <~ DOT) ~ name ^^ {
    case prefix ~ n => PrefixedVariable(prefix, n)
  }

  lazy val variableWithIndex: PackratParser[Variable] =
    prefixExpression ~ (INDEX_OPEN ~> expression <~ INDEX_CLOSE) ^^ {
      case prefix ~ expr => IndexedVariable(prefix, expr)
    }

  lazy val variable: PackratParser[Variable] =
    (name ^^ { n => NamedVariable(n) }) | variableWithDot | variableWithIndex

  lazy val variableList: PackratParser[List[Variable]] = variable ~ (COMMA ~> variable).* ^^ {
    case first ~ rest => first :: rest
  }

  lazy val breakStatement: PackratParser[LastStatement] = BREAK ^^ { _ => BreakStatement }
  lazy val continueStatement: PackratParser[LastStatement] = CONTINUE ^^ { _ => ContinueStatement }
  lazy val returnStatement: PackratParser[LastStatement] = RETURN ~> expressionList ^^ {
    list => ReturnStatement(list)
  }

  lazy val lastStatement: PackratParser[LastStatement] = breakStatement | continueStatement | returnStatement
}
