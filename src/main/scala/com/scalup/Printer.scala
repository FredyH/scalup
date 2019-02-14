package com.scalup

import com.scalup.Printer.PrinterConfig

object Printer {

  case class PrinterConfig(statementSeparator: String = "\n",
                           blockOpener: String = "\n",
                           blockCloser: String = "\n",
                           indentString: String = "\t",
                           listSeparator: String = ", ",
                           padding: String = " ",
                           wrapConditions: Boolean = true)

}


class Printer(config: PrinterConfig = PrinterConfig()) {
  private def wrapIfHigherPrecedence(expression: Expression, precedence: Int): String = {
    if (expression.precedence < precedence)
      s"(${printExpression(expression)})"
    else
      printExpression(expression)
  }

  private def wrapCondition(condition: Expression, indentIndex: Int): String = {
    //Do not wrap by default! If it is wrapped in code it is an ExpressionAsPrefixExpression and is wrapped through that
    printExpression(condition, indentIndex)
    //wrapString(printExpression(condition, indentIndex))
  }


  private def printField(field: Field): String = {
    field match {
      case FieldWithoutIndex(value) => printExpression(value)
      case FieldByIndex(key, value) => "[" + printExpression(key) + "]" + wrapInPadding("=") + printExpression(value)
      case FieldByName(key, value) => key + wrapInPadding("=") + printExpression(value)
    }
  }

  private def printVariable(variable: Variable): String =
    variable match {
      case NamedVariable(name) => name
      case PrefixedVariable(prefix, name) => printExpression(prefix) + "." + name
      case IndexedVariable(prefix, expression) => printExpression(prefix) + "[" + printExpression(expression) + "]"
    }

  private def printFunctionCall(call: FunctionCall): String =
    call match {
      case FunctionCallWithoutSelf(prefix, arguments) =>
        arguments.map(e => printExpression(e)).mkString(printExpression(prefix) + "(", config.listSeparator, ")")

      case FunctionCallWithSelf(prefix, name, arguments) =>
        arguments.map(e => printExpression(e)).mkString(s"${printExpression(prefix)}:$name(", config.listSeparator, ")")
    }

  private def printPrefixExpression(prefixExpression: PrefixExpression): String =
    prefixExpression match {
      case call: FunctionCall => printFunctionCall(call)
      case variable: Variable => printVariable(variable)
      case ExpressionAsPrefix(expr) => "(" + printExpression(expr) + ")"
    }

  private def wrapInPadding(str: String): String = {
    config.padding + str + config.padding
  }

  private def printExpression(expression: Expression, indentIndex: Int = 0): String = {
    expression match {
      case operator: BinaryOperator =>
        wrapIfHigherPrecedence(operator.left, operator.precedence) +
          wrapInPadding(operator.token.keywords.head) +
          wrapIfHigherPrecedence(operator.right, operator.precedence)

      case operator: UnaryOperator =>
        operator.token + wrapIfHigherPrecedence(operator.subExpression, operator.precedence)

      case FunctionExpression(functionBody) =>
        functionBody.parameters.paramsWithVarArg.mkString("function(", config.listSeparator, ")") +
          printBlock(functionBody.body, indentIndex + 1) + indentString(indentIndex)("end")

      case expression: PrefixExpression =>
        printPrefixExpression(expression)

      case LuaNil => "nil"
      case LuaFalse => "false"
      case LuaTrue => "true"
      case LuaNumber(value, raw) => raw
      case LuaString(value, raw) => raw
      case VarArgs => "..."
      case TableConstructor(fields) => fields.map(printField).mkString("{", config.listSeparator, "}")
    }
  }

  private def indentString(indentIndex: Int)(str: String): String = {
    (config.indentString * indentIndex) + str
  }


  private def printStatement(statement: Statement, indentIndex: Int): String = {
    val indent = indentString(indentIndex) _
    statement match {
      case GlobalDeclaration(variables, expressionList) =>
        indent(variables.map(printVariable).mkString(", ") + wrapInPadding("=") + expressionList.map(e => printExpression(e, indentIndex: Int))
          .mkString(config.listSeparator))

      case LocalDeclaration(names, expressionList) =>
        indent(names.mkString("local ", config.listSeparator, wrapInPadding("=") + expressionList.map(e => printExpression(e, indentIndex: Int))
          .mkString(config.listSeparator)))
      case lastStatement: LastStatement =>
        lastStatement match {
          case ContinueStatement => "continue"
          case BreakStatement => "break"
          case ReturnStatement(expressions) => expressions.map(e => printExpression(e, indentIndex: Int))
            .mkString(indent("return "), config.listSeparator, "")
        }

      case call: FunctionCall =>
        indent(printFunctionCall(call))
      case DoBlock(body) =>
        indent("do") + printBlock(body, indentIndex + 1) + indent("end")
      case WhileLoop(condition, body) =>
        indent("while" + wrapCondition(condition, indentIndex) + "do") + printBlock(body, indentIndex + 1) + indent("end")
      case RepeatLoop(body, condition) =>
        indent("repeat") + printBlock(body, indentIndex + 1) + indent("until") + wrapCondition(condition, indentIndex)
      case IfStatement(condition, body, elseIfs, elseBlock) =>
        val ifHeader = indent("if" + config.padding + wrapCondition(condition, indentIndex) + config.padding + "then")
        val elseIfStrs = elseIfs.map{ elseIf =>
          val elseIfHeader = indent("elseif" + config.padding + wrapCondition(condition, indentIndex) + config.padding + "then")
          elseIfHeader + printBlock(body, indentIndex + 1)
        }.mkString(config.statementSeparator)
        val elseStr = elseBlock.map { elseBlock =>
          indent("else") + printBlock(elseBlock, indentIndex + 1)
        }.getOrElse("")
        ifHeader + printBlock(body, indentIndex + 1) + elseIfStrs + elseStr + indent("end")

      case ForLoop(variableName, initialValue, upperBound, stepValue, block) =>
        val initialStr = printExpression(initialValue, indentIndex)
        val upperStr = printExpression(upperBound, indentIndex)
        val stepStr = stepValue.map(e => config.listSeparator + printExpression(e, indentIndex)).getOrElse("")
        val header = s"for $variableName${wrapInPadding("=")}$initialStr${config.listSeparator}$upperStr$stepStr do"
        indent(header) + printBlock(block, indentIndex + 1) + indent("end")

      case ForEachLoop(variables, expressions, block) =>
        val header = indent(s"for ${variables.mkString(config.listSeparator)} in ${expressions.map(e => printExpression(e, indentIndex)).mkString(config.listSeparator)} do")
        s"$header${printBlock(block, indentIndex + 1)}${indent("end")}"

      case FunctionDefinition(name, body, local) =>
        val prefix = if (local) "local function" else "function"
        val header = indent(s"$prefix $name(${body.parameters.paramsWithVarArg.mkString(config.listSeparator)})")
        val bodyStr = printBlock(body.body, indentIndex + 1)
        header + bodyStr + indent("end")
    }
  }

  private def printStatements(statements: List[Statement], indentIndex: Int): String =
    statements.map(s => printStatement(s, indentIndex)).mkString(config.statementSeparator)

  def printBlock(block: Block, indentIndex: Int = 0): String = {
    config.blockOpener + printStatements(block.statements, indentIndex) + config.blockCloser
  }

}