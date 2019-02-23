package com.scalup.visitors

import com.scalup._


class ReducerVisitor[P, R](returnTypeReducer: (R, R) => R, emptyReturnValue: R) extends AbstractVisitor [P, R] {

  def visitField(field: Field, passthrough: P): R = {
    field match {
      case FieldWithoutIndex(value) => visitExpression(value, passthrough)
      case FieldByIndex(key, value) => returnTypeReducer(visitExpression(key, passthrough), visitExpression(value, passthrough))
      case FieldByName(key, value) => visitExpression(value, passthrough)
    }
  }

  def visitVariable(variable: Variable, passthrough: P, global: Boolean = false): R =
    variable match {
      case NamedVariable(name) => emptyReturnValue
      case PrefixedVariable(prefix, name) => visitPrefixExpression(prefix, passthrough)
      case IndexedVariable(prefix, expression) =>
				returnTypeReducer(visitPrefixExpression(prefix, passthrough), visitExpression(expression, passthrough))
    }

  def visitFunctionCall(call: FunctionCall, passthrough: P): R =
    call match {
      case FunctionCallWithoutSelf(prefix, arguments) =>
        returnTypeReducer(arguments.map(e => visitExpression(e, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer), visitExpression(prefix, passthrough))

      case FunctionCallWithSelf(prefix, name, arguments) =>
				returnTypeReducer(arguments.map(e => visitExpression(e, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer), visitExpression(prefix, passthrough))
    }

  def visitPrefixExpression(prefixExpression: PrefixExpression, passthrough: P): R =
    prefixExpression match {
      case call: FunctionCall => visitFunctionCall(call, passthrough)
      case variable: Variable => visitVariable(variable, passthrough)
      case ExpressionAsPrefix(expr) => visitExpression(expr, passthrough)
    }

  def visitExpression(expression: Expression, passthrough: P): R = {
    expression match {
      case operator: BinaryOperator =>
				returnTypeReducer(visitExpression(operator.left, passthrough), visitExpression(operator.right, passthrough))

      case operator: UnaryOperator => visitExpression(operator.subExpression, passthrough)

      case FunctionExpression(functionBody) => visitBlock(functionBody.body, passthrough)

      case expression: PrefixExpression => visitPrefixExpression(expression, passthrough)

      case LuaNil => emptyReturnValue
      case LuaFalse => emptyReturnValue
      case LuaTrue => emptyReturnValue
      case LuaNumber(value, raw) => emptyReturnValue
      case LuaString(value, raw) => emptyReturnValue
      case VarArgs => emptyReturnValue
      case TableConstructor(fields) => fields.map(visitField(_, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer)
    }
  }

  def visitStatement(statement: Statement, passthrough: P): R = {

    statement match {
      case GlobalDeclaration(variables, expressionList) =>
				returnTypeReducer (
        	variables.map(visitVariable(_, passthrough, true)).foldLeft(emptyReturnValue)(returnTypeReducer),
					expressionList.map(e => visitExpression(e, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer)
				)

      case LocalDeclaration(names, expressionList) =>
				expressionList.map(visitExpression(_, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer)

      case lastStatement: LastStatement =>
        lastStatement match {
          case ContinueStatement => emptyReturnValue
          case BreakStatement => emptyReturnValue
          case ReturnStatement(expressions) => expressions.map(e => visitExpression(e, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer)
        }

      case call: FunctionCall => visitFunctionCall(call, passthrough)

      case DoBlock(body) => visitBlock(body, passthrough)

      case WhileLoop(condition, body) =>
        returnTypeReducer(visitExpression(condition, passthrough), visitBlock(body, passthrough))

      case RepeatLoop(body, condition) =>
        returnTypeReducer(visitBlock(body, passthrough), visitExpression(condition, passthrough))

      case IfStatement(condition, body, elseIfs, elseBlock) =>
				List(
					visitExpression(condition, passthrough),
					elseIfs.map(elseIf =>
						returnTypeReducer(visitExpression(elseIf.condition, passthrough), visitBlock(elseIf.body, passthrough))
					).reduceOption(returnTypeReducer).getOrElse(emptyReturnValue),
					elseBlock.map(visitBlock(_, passthrough)).getOrElse(emptyReturnValue),
					visitBlock(body, passthrough)
				).foldLeft(emptyReturnValue)(returnTypeReducer)

      case ForLoop(variableName, initialValue, upperBound, stepValue, block) =>
				List(
					visitExpression(initialValue, passthrough),
					visitExpression(upperBound, passthrough),
					stepValue.map(visitExpression(_, passthrough)).getOrElse(emptyReturnValue),
					visitBlock(block, passthrough)
				).foldLeft(emptyReturnValue)(returnTypeReducer)

      case ForEachLoop(variables, expressions, block) =>
				returnTypeReducer(
					expressions.map(visitExpression(_, passthrough)).reduceOption(returnTypeReducer).getOrElse(emptyReturnValue),
					visitBlock(block, passthrough)
				)

      case FunctionDefinition(name, body, local) =>
        visitBlock(body.body, passthrough)

    }
  }

  def visitStatements(statements: List[Statement], passthrough: P): List[R] =
    statements.map(s => visitStatement(s, passthrough))

  def visitBlock(block: Block, passthrough: P): R = {
    visitStatements(block.statements, passthrough).reduceOption(returnTypeReducer).getOrElse(emptyReturnValue)
  }

}