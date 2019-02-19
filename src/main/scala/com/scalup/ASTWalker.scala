package com.scalup


class ASTWalker[P, R](returnTypeReducer: (R, R) => R, emptyReturnValue: R) {

  def walkField(field: Field, passthrough: P): R = {
    field match {
      case FieldWithoutIndex(value) => walkExpression(value, passthrough)
      case FieldByIndex(key, value) => returnTypeReducer(walkExpression(key, passthrough), walkExpression(value, passthrough))
      case FieldByName(key, value) => walkExpression(value, passthrough)
    }
  }

  def walkVariable(variable: Variable, passthrough: P, global: Boolean = false): R =
    variable match {
      case NamedVariable(name) => emptyReturnValue
      case PrefixedVariable(prefix, name) => walkExpression(prefix, passthrough)
      case IndexedVariable(prefix, expression) =>
				returnTypeReducer(walkExpression(prefix, passthrough), walkExpression(expression, passthrough))
    }

  def walkFunctionCall(call: FunctionCall, passthrough: P): R =
    call match {
      case FunctionCallWithoutSelf(prefix, arguments) =>
        returnTypeReducer(arguments.map(e => walkExpression(e, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer), walkExpression(prefix, passthrough))

      case FunctionCallWithSelf(prefix, name, arguments) =>
				returnTypeReducer(arguments.map(e => walkExpression(e, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer), walkExpression(prefix, passthrough))
    }

  def walkPrefixExpression(prefixExpression: PrefixExpression, passthrough: P): R =
    prefixExpression match {
      case call: FunctionCall => walkFunctionCall(call, passthrough)
      case variable: Variable => walkVariable(variable, passthrough)
      case ExpressionAsPrefix(expr) => walkExpression(expr, passthrough)
    }

  def walkExpression(expression: Expression, passthrough: P): R = {
    expression match {
      case operator: BinaryOperator =>
				returnTypeReducer(walkExpression(operator.left, passthrough), walkExpression(operator.right, passthrough))

      case operator: UnaryOperator => walkExpression(operator.subExpression, passthrough)

      case FunctionExpression(functionBody) => walkBlock(functionBody.body, passthrough)

      case expression: PrefixExpression => walkPrefixExpression(expression, passthrough)

      case LuaNil => emptyReturnValue
      case LuaFalse => emptyReturnValue
      case LuaTrue => emptyReturnValue
      case LuaNumber(value, raw) => emptyReturnValue
      case LuaString(value, raw) => emptyReturnValue
      case VarArgs => emptyReturnValue
      case TableConstructor(fields) => fields.map(walkField(_, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer)
    }
  }

  def walkStatement(statement: Statement, passthrough: P): R = {

    statement match {
      case GlobalDeclaration(variables, expressionList) =>
				returnTypeReducer (
        	variables.map(walkVariable(_, passthrough, true)).foldLeft(emptyReturnValue)(returnTypeReducer),
					expressionList.map(e => walkExpression(e, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer)
				)

      case LocalDeclaration(names, expressionList) =>
				expressionList.map(walkExpression(_, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer)

      case lastStatement: LastStatement =>
        lastStatement match {
          case ContinueStatement => emptyReturnValue
          case BreakStatement => emptyReturnValue
          case ReturnStatement(expressions) => expressions.map(e => walkExpression(e, passthrough)).foldLeft(emptyReturnValue)(returnTypeReducer)
        }

      case call: FunctionCall => walkFunctionCall(call, passthrough)

      case DoBlock(body) => walkBlock(body, passthrough)

      case WhileLoop(condition, body) =>
        returnTypeReducer(walkExpression(condition, passthrough), walkBlock(body, passthrough))

      case RepeatLoop(body, condition) =>
        returnTypeReducer(walkBlock(body, passthrough), walkExpression(condition, passthrough))

      case IfStatement(condition, body, elseIfs, elseBlock) =>
				List(
					walkExpression(condition, passthrough),
					elseIfs.map(elseIf =>
						returnTypeReducer(walkExpression(elseIf.condition, passthrough), walkBlock(elseIf.body, passthrough))
					).reduceOption(returnTypeReducer).getOrElse(emptyReturnValue),
					elseBlock.map(walkBlock(_, passthrough)).getOrElse(emptyReturnValue),
					walkBlock(body, passthrough)
				).foldLeft(emptyReturnValue)(returnTypeReducer)

      case ForLoop(variableName, initialValue, upperBound, stepValue, block) =>
				List(
					walkExpression(initialValue, passthrough),
					walkExpression(upperBound, passthrough),
					stepValue.map(walkExpression(_, passthrough)).getOrElse(emptyReturnValue),
					walkBlock(block, passthrough)
				).foldLeft(emptyReturnValue)(returnTypeReducer)

      case ForEachLoop(variables, expressions, block) =>
				returnTypeReducer(
					expressions.map(walkExpression(_, passthrough)).reduceOption(returnTypeReducer).getOrElse(emptyReturnValue),
					walkBlock(block, passthrough)
				)

      case FunctionDefinition(name, body, local) =>
        walkBlock(body.body, passthrough)

    }
  }

  def walkStatements(statements: List[Statement], passthrough: P): R =
    statements.map(s => walkStatement(s, passthrough)).reduceOption(returnTypeReducer).getOrElse(emptyReturnValue)

  def walkBlock(block: Block, passthrough: P): R = {
    walkStatements(block.statements, passthrough)
  }

}