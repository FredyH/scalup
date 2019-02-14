package com.scalup

import com.scalup.LuaAST

class Printer(statementSeparator: Int => String, blockOpener: String, conditionWrapper: (String, String)) {

	private def wrapIfHigherPrecedence(expression: Expression, precedence: Int): String = {
		if (expression.precedence > precedence)
			f"(${printExpression(expression)})"
		else
			printExpression(expression)
	}

	private def wrapCondition(condition: Expression): String =
		conditionWrapper._1 + printExpression(condition) + conditionWrapper._2


	private def printField(field: Field): String = {
		field match {
			case FieldWithoutIndex(value) => printExpression(value)
			case FieldByIndex(key, value) => "[" + printExpression(key) + "]=" + printExpression(value)
			case FieldByName(key, value) => key + "=" + printExpression(value)
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
				arguments.map(printExpression).mkString(printExpression(prefix) + "(", ",", ")")

			case FunctionCallWithSelf(prefix, name, arguments) =>
				arguments.map(printExpression).mkString(f"${printExpression(prefix)}:$name(", ",", ")")
		}

	private def printPrefixExpression(prefixExpression: PrefixExpression): String =
		prefixExpression match {
			case call: FunctionCall => printFunctionCall(call)
			case variable: Variable => printVariable(variable)
			case ExpressionAsPrefix(expr) => "(" + printExpression(expr) + ")."
		}

	private def printExpression(expression: Expression): String = {
		expression match {
			case operator: BinaryOperator =>
				wrapIfHigherPrecedence(operator.left, operator.precedence) +
					operator.token.keywords.head +
				wrapIfHigherPrecedence(operator.right, operator.precedence)

			case operator: UnaryOperator =>
				operator.token + wrapIfHigherPrecedence(operator.subExpression, operator.precedence)

			case FunctionExpression(functionBody) =>
				functionBody.parameters.paramsWithVarArg.mkString("function(", ",", ")")

			case expression: PrefixExpression =>
				printPrefixExpression(expression)

			case LuaNil => "nil"
			case LuaFalse => "false"
			case LuaTrue => "true"
			case LuaNumber(value) => value.toString
			case LuaString(value, raw) => raw
			case VarArgs => "..."
			case TableConstructor(fields) => fields.map(printField).mkString("{", ",", "}")
		}
	}


	private def printStatement(statement: Statement): String = {
		statement match {
			case GlobalDeclaration(variables, expressionList) =>
				variables.map(printVariable).mkString(",") + "=" + expressionList.map(printExpression).mkString(",")

			case LocalDeclaration(names, expressionList) =>
				names.mkString("local ", ",", "=" + expressionList.map(printExpression).mkString(","))

			case lastStatement: LastStatement =>
				lastStatement match {
					case ContinueStatement => "continue"
					case BreakStatement => "break"
					case ReturnStatement(expressions) => expressions.map(printExpression).mkString("return ", ",", "")
				}

			case call: FunctionCall => printFunctionCall(call)

			case DoBlock(body) =>
				"do" + blockOpener + printBlock(body, 2) + "end"

			case WhileLoop(condition, body) =>
				"while" + wrapCondition(condition) + "do" + blockOpener + printBlock(body, 2) + "end"

			case RepeatLoop(body, condition) => "repeat" + blockOpener + printBlock(body, 2) + "until" + wrapCondition(condition)

			case IfStatement(condition, body, elseIfs, elseBlock) =>
				"if" + wrapCondition(condition) + "then" + blockOpener + printBlock(body, 2) +
					elseIfs.map(elseif => "elseif" + wrapCondition(elseif.condition) + "then" + blockOpener + printBlock(elseif.body, 2)).mkString(statementSeparator(2))
					elseBlock.map(e => printStatements(e.statements, 2)).getOrElse("") + "end"

			case ForLoop(variableName, initialValue, upperBound, stepValue, block) =>
				"for" + conditionWrapper._1 +
					variableName + "=" + printExpression(initialValue) + "," +
					printExpression(upperBound) +
					stepValue.map("," + printExpression(_)).getOrElse("") +
					conditionWrapper._2 + "do" +
					printBlock(block, 2) + "end"

			case ForEachLoop(variables, expressions, block) =>
				"for" + conditionWrapper._1 +
				variables.mkString(",") +
				" in" + expressions.map(printExpression).mkString(",") +
				conditionWrapper._2 + "do" +
				printBlock(block, 2) + "end"

			case FunctionDefinition(name, body, local) =>
				{ if(local) "local function " else "function " } +
					body.parameters.paramsWithVarArg.mkString(f"$name(", ",", ")")
		}
	}

	private def printStatements(statements: List[Statement], indentIndex: Int): String =
		statements.map(printStatement).mkString(statementSeparator(indentIndex))

	def printBlock(block: Block, indentIndex: Int = 1): String = {
		printStatements(block.statements, indentIndex)
	}

}
