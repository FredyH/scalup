package com.scalup

import java.io.File
import java.nio.file.Files

import com.scalup.LuaToken._
import fastparse._
import LuaWhitespace._
import com.scalup.visitors.{GlobalVariableVisitor, LocalVariableSubVisitor}

object LuaParser extends LuaLexer {
  private def name[_: P]: P[String] = P(identifier.map(_.name))

  private def luaNil[_: P]: P[Expression] = P(`nil`.map(_ => LuaNil))

  private def luaString[_: P]: P[LuaString] = P(string.map { token => LuaString(token.content, token.raw) })

  private def luaTrue[_: P]: P[Expression] = P(`true`.map(_ => LuaTrue))

  private def luaFalse[_: P]: P[Expression] = P(`false`.map(_ => LuaFalse))

  private def varArgs[_: P]: P[Expression] = P(`...`.map(_ => VarArgs))

  private def luaNumber[_: P]: P[Expression] = P(`number`.map(n => LuaNumber(n.value, n.raw)))


  private def doBlock[_: P]: P[DoBlock] =
    P(`do` ~ block ~ `end`).map(DoBlock)

  private def statement[_: P]: P[Statement] =
    P(functionCall | forLoop | globalDeclaration | doBlock | whileLoop | repeatLoop | ifStatement |
      forEachLoop | functionDefinition | localFunctionDefinition | localDeclaration)


  private def block[_: P]: P[Block] =
    P((statement ~ `;`.?).rep ~ (lastStatement ~ `;`.?).?).map {
      case (list, last) => Block((list ++ last).toList)
    }

  //The Pass() ~ part makes sure that whitespaces/comments at the start of the code are ignored
  private def chunk[_: P]: P[Block] = P(Pass() ~ block ~ End)

  private def breakStatement[_: P]: P[LastStatement] = `break`.map(_ => BreakStatement)

  private def continueStatement[_: P]: P[LastStatement] = `continue`.map(_ => ContinueStatement)

  private def returnStatement[_: P]: P[LastStatement] = P(`return` ~ expressionList.?).map {
    list => ReturnStatement(list.getOrElse(Nil))
  }

  private def lastStatement[_: P]: P[LastStatement] = breakStatement | continueStatement | returnStatement

  private def expression[_: P]: P[Expression] = orOperators

  private def globalDeclaration[_: P]: P[GlobalDeclaration] = P(variableList ~ `=` ~ expressionList).map {
    case (vars, exprs) => GlobalDeclaration(vars, exprs)
  }

  private def localDeclaration[_: P]: P[LocalDeclaration] =
    P(`local` ~ nameList ~ (`=` ~ expressionList).?).map {
      case (names, expressions) => LocalDeclaration(names, expressions.getOrElse(Nil))
    }

  private def functionName[_: P]: P[FunctionName] = P(name ~ (`.` ~ name).rep ~ (`:` ~ name).?).map {
    case (first, rest, None) => FunctionName(first :: rest.toList, withSelf = false)
    case (first, rest, Some(last)) => FunctionName((first :: rest.toList) :+ last, withSelf = true)
  }

  private def functionDefinition[_: P]: P[FunctionDefinition] = P(`function` ~ functionName ~ functionBody).map {
    case (n, b) => FunctionDefinition(n, b, false)
  }

  private def localFunctionDefinition[_: P]: P[FunctionDefinition] = P(`local` ~ `function` ~ name ~ functionBody).map {
    case (n, b) => FunctionDefinition(FunctionName(List(n), false), b, true)
  }

  private def elseIfStatement[_: P]: P[ElseIfStatement] =
    P(`elseif` ~ expression ~ `then` ~ block).map { case (c, b) => ElseIfStatement(c, b) }

  private def elseStatement[_: P]: P[Block] = P(`else` ~ block)

  private def ifStatement[_: P]: P[IfStatement] =
    P(`if` ~ expression ~ `then` ~ block ~ elseIfStatement.rep ~ elseStatement.? ~ `end`).map {
      case (ifCond, ifBlock, elseIfStatements, elseOpt) => IfStatement(ifCond, ifBlock, elseIfStatements.toList, elseOpt)
    }

  private def whileLoop[_: P]: P[WhileLoop] = P(`while` ~ expression ~ `do` ~ block ~ `end`).map {
    case (condition, b) => WhileLoop(condition, b)
  }

  private def repeatLoop[_: P]: P[RepeatLoop] = P(`repeat` ~ block ~ `until` ~ expression).map {
    case (b, condition) => RepeatLoop(b, condition)
  }

  private def forLoop[_: P]: P[ForLoop] =
    P(`for` ~ name ~ `=` ~ expression ~ `,` ~ expression ~ (`,` ~ expression).? ~ (`do` ~ block ~ `end`)).map {
      case (varName, first, second, thirdOpt, b) => ForLoop(varName, first, second, thirdOpt, b)
    }

  private def forEachLoop[_: P]: P[ForEachLoop] =
    P(`for` ~ nameList ~ `in` ~ expressionList ~ `do` ~ block ~ `end`).map {
      case (varList, exprList, b) => ForEachLoop(varList, exprList, b)
    }

  private def operatorToken[_: P](operators: List[() => P[KeywordOrOperator]]): P[KeywordOrOperator] = operators match {
    case Nil => throw new RuntimeException("Empty operator list")
    case x :: xs =>
      operators.foldLeft(x.apply()) {
        case (parser, token) => parser | token.apply()
      }
  }

  private def listOfOperators[_: P](currentLevel: => P[Expression],
                                    nextLevel: => P[Expression],
                                    operators: List[() => P[KeywordOrOperator]]): P[Expression] = {
    P(nextLevel ~ (operatorToken(operators) ~ currentLevel).rep).map {
      case (first, Nil) => first
      case (first, (firstOp, second) +: rest) =>
        rest.foldLeft(BinaryOperator.fromLuaToken(firstOp, first, second)) {
          case (current, (op, expr)) =>
            BinaryOperator.fromLuaToken(op, current, expr)
        }
    }
  }

  private def constants[_: P]: P[Expression] = luaNil | luaNumber | luaString | luaTrue | luaFalse | varArgs

  private def unaryOperators[_: P]: P[Expression] = P(operatorToken(List(() => `#`, () => `-`, () => `!`)) ~ unaryTerms).map {
    case (NUMBER_SIGN, expr) => UnaryCount(expr)
    case (MINUS, expr) => UnaryMinus(expr)
    case (LOGICAL_NOT, expr) => UnaryNot(expr)
    case _ => ??? // Can't happen
  }

  private def unaryTerms[_: P]: P[Expression] =
    P(prefixExpression | unaryOperators | constants | anonymousFunction | tableConstructor | (`(` ~ orOperators ~ `)`))

  //TODO: The precedence here is not correct, for example 3^-3^4 vs 3^(-3)^4
  private def exponentiations[_: P]: P[Expression] =
    P(unaryTerms ~ (`^` ~ exponentiations).rep).map {
      case (first, Seq()) => first
      case (first, rest) =>
        val together = first +: rest
        together.slice(0, together.size - 1).foldRight(together.last) {
          case (left, right) => Exponentiation(left, right)
        }
    }

  private def factors[_: P]: P[Expression] =
    listOfOperators(factors, exponentiations, List(() => `*`, () => `/`, () => `%`))

  private def terms[_: P]: P[Expression] =
    listOfOperators(terms, factors, List(() => `+`, () => `-`))

  private def concatenations[_: P]: P[Expression] =
    listOfOperators(concatenations, terms, List(() => `..`))

  private def comparisons[_: P]: P[Expression] =
    listOfOperators(comparisons, concatenations, List(() => `<=`, () => `<`, () => `>=`, () => `>`, () => `==`, () => `!=`))

  private def andOperators[_: P]: P[Expression] =
    listOfOperators(andOperators, comparisons, List(() => `&&`))

  private def orOperators[_: P]: P[Expression] =
    listOfOperators(orOperators, andOperators, List(() => `||`))

  private def namePrefix[_: P]: P[(PrefixExpression, List[PrefixPart])] =
    P(name ~ prefixExpressionRest).map {
      case (name, rest) => (NamedVariable(name), rest)
    }

  private def expressionPrefix[_: P]: P[(PrefixExpression, List[PrefixPart])] =
    P(`(` ~ expression ~ `)` ~ prefixExpressionRest).map {
      case (expr, rest) => (ExpressionAsPrefix(expr), rest)
    }

  private def prefixExpression[_: P]: P[PrefixExpression] =
    P(namePrefix | expressionPrefix).map { case (start, expressionParts) =>
      expressionParts.foldLeft(start: PrefixExpression) {
        case (previous, NamePrefixPart(name)) => PrefixedVariable(previous, name)
        case (previous, IndexPrefixPart(index)) => IndexedVariable(previous, index)
        case (previous, FunctionCallPrefixPart(args)) => FunctionCallWithoutSelf(previous, args)
        case (previous, FunctionCallWithSelfPrefixPart(name, args)) => FunctionCallWithSelf(previous, name, args)
      }
    }

  private def indexBracketsPrefix[_: P]: P[List[PrefixPart]] =
    P(`[` ~ expression ~ `]` ~ prefixExpressionRest).map {
      case (index, rest) => IndexPrefixPart(index) :: rest
    }

  private def indexDotPrefix[_: P]: P[List[PrefixPart]] =
    P(`.` ~ name ~ prefixExpressionRest).map {
      case (name, rest) => NamePrefixPart(name) :: rest
    }

  private def functionCallPrefix[_: P]: P[List[PrefixPart]] =
    P(argumentList ~ prefixExpressionRest).map {
      case (args, rest) => FunctionCallPrefixPart(args) :: rest
    }

  private def selfFunctionCallPrefix[_: P]: P[List[PrefixPart]] =
    P(`:` ~ name ~ argumentList ~ prefixExpressionRest).map {
      case (name, args, rest) => FunctionCallWithSelfPrefixPart(name, args) :: rest
    }

  private def prefixExpressionRest[_: P]: P[List[PrefixPart]] =
    P(selfFunctionCallPrefix | functionCallPrefix | indexDotPrefix | indexBracketsPrefix | Pass(Nil))

  private def fieldWithIndex[_: P]: P[Field] = P(`[` ~ expression ~ `]` ~ `=` ~ expression).map {
    case (indexExpr, valueExpr) => FieldByIndex(indexExpr, valueExpr)
  }

  private def fieldWithName[_: P]: P[Field] = P(name ~ `=` ~ expression).map {
    case (n, expr) => FieldByName(n, expr)
  }

  private def fieldWithoutIndex[_: P]: P[Field] = P(expression).map {
    expr => FieldWithoutIndex(expr)
  }

  private def field[_: P]: P[Field] = fieldWithIndex | fieldWithName | fieldWithoutIndex

  private def fieldSeparator[_: P]: P[Unit] = P(`;` | `,`).map { _ => () }

  private def fieldList[_: P]: P[List[Field]] = P((field ~ (fieldSeparator ~ field).rep) ~ fieldSeparator.?).map {
    case (first, list) => first :: list.toList
  }

  private def tableConstructor[_: P]: P[TableConstructor] =
    P(`{` ~ fieldList.? ~ `}`).map {
      list => TableConstructor(list.getOrElse(Nil))
    }

  private def functionBody[_: P]: P[FunctionBody] =
    P(`(` ~ parameterList.? ~ `)` ~ block ~ `end`).map {
      case (list, functionBlock) => FunctionBody(list.getOrElse(ParameterList(Nil, false)), functionBlock)
    }

  private def anonymousFunction[_: P]: P[FunctionExpression] = P(`function` ~ functionBody).map { body =>
    FunctionExpression(body)
  }

  private def parameterListWithVarArgs[_: P]: P[ParameterList] = P(nameList ~ (`,` ~ `...`).?).map {
    list => ParameterList(list.map(Parameter), true)
  }

  private def parameterListWithoutVarArgs[_: P]: P[ParameterList] = P(`...`).map { _ => ParameterList(Nil, false) }

  private def parameterList[_: P]: P[ParameterList] = parameterListWithoutVarArgs | parameterListWithVarArgs

  private def argumentList[_: P]: P[List[Expression]] =
    P((`(` ~ expressionList.? ~ `)`).map { l => l.getOrElse(Nil) } |
      tableConstructor.map { tc => List(tc) } |
      luaString.map { tc => List(tc) })

  private def expressionList[_: P]: P[List[Expression]] = P(expression ~ (`,` ~ expression).rep).map {
    case (first, rest) => first :: rest.toList
  }

  private def nameList[_: P]: P[List[String]] = P(name ~ (`,` ~ name).rep).map {
    case (first, rest) => first :: rest.toList
  }

  private def functionCall[_: P]: P[FunctionCall] =
    prefixExpression.filter(_.isInstanceOf[FunctionCall]).map(_.asInstanceOf[FunctionCall])

  private def variable[_: P]: P[Variable] =
    prefixExpression.filter(_.isInstanceOf[Variable]).map(_.asInstanceOf[Variable])

  private def variableList[_: P]: P[List[Variable]] =
    P(variable ~ (`,` ~ variable).rep).map {
      case (first, rest) => first :: rest.toList
    }

  def parseFolder(folder: File): Unit = {
    import scala.collection.JavaConverters._
    val start = System.currentTimeMillis()
    for (path <- Files.walk(folder.toPath).iterator().asScala;
         f = path.toFile if f.getName.endsWith(".lua")) {
      val input = new String(Files.readAllBytes(path))
      parse(input, chunk(_)) match {
        case Parsed.Success(value, _) => //println("Successfully parsed file: " + f.getName)
        case Parsed.Failure(label, _, extra) => println(s"Failed to parse file: ${f.getName}")
      }
    }
    println(System.currentTimeMillis() - start)
  }

  def parseString(s: String) = {
    parse(s.trim(), chunk(_)) match {
      case Parsed.Success(value, _) => Some(value)
      case Parsed.Failure(label, _, extra) => println("FAILURE", label, extra); None
    }
  }

  def main(args: Array[String]): Unit = {
   //parseFolder(new File("C:\\server\\garrysmod\\gamemodes"))
    val input = new String(Files.readAllBytes(new File("test.lua").toPath)).trim
    val start = System.currentTimeMillis()
    parse(input, chunk(_)) match {
      case Parsed.Success(value, _) => println("SUCCESS: ", value)
        val compacted = new Compactor().compactBlock(value)
        println(new GlobalVariableVisitor().visitBlock(value, ()))
      case Parsed.Failure(label, _, extra) => println("FAILURE", label, extra)
    }
    println(System.currentTimeMillis() - start)
  }
}