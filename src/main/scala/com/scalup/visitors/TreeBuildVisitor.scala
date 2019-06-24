package com.scalup.visitors

import com.scalup._

class TreeBuildVisitor[P] extends AbstractVisitor[P, LuaAST] {

  def visitField(field: Field, passthrough: P): Field = {
    field match {
      case FieldWithoutIndex(value) =>
        FieldWithoutIndex(visitExpression(value, passthrough))
      case FieldByIndex(key, value) =>
        FieldByIndex(
          visitExpression(key, passthrough),
          visitExpression(value, passthrough)
        )
      case FieldByName(key, value) =>
        FieldByName(key, visitExpression(value, passthrough))
    }
  }

  def visitVariable(variable: Variable,
                    passthrough: P,
                    global: Boolean = false): Variable =
    variable match {
      case NamedVariable(name) => NamedVariable(name)
      case PrefixedVariable(prefix, name) =>
        PrefixedVariable(visitPrefixExpression(prefix, passthrough), name)
      case IndexedVariable(prefix, expression) =>
        IndexedVariable(
          visitPrefixExpression(prefix, passthrough),
          visitExpression(expression, passthrough)
        )
    }

  def visitFunctionCall(call: FunctionCall, passthrough: P): FunctionCall =
    call match {
      case FunctionCallWithoutSelf(prefix, arguments) =>
        FunctionCallWithoutSelf(
          visitPrefixExpression(prefix, passthrough),
          arguments.map(visitExpression(_, passthrough))
        )

      case FunctionCallWithSelf(prefix, name, arguments) =>
        FunctionCallWithSelf(
          visitPrefixExpression(prefix, passthrough),
          name,
          arguments.map(visitExpression(_, passthrough))
        )
    }

  def visitPrefixExpression(prefixExpression: PrefixExpression,
                            passthrough: P): PrefixExpression =
    prefixExpression match {
      case call: FunctionCall => visitFunctionCall(call, passthrough)
      case variable: Variable => visitVariable(variable, passthrough)
      case ExpressionAsPrefix(expr) =>
        ExpressionAsPrefix(visitExpression(expr, passthrough))
    }

  def visitExpression(expression: Expression, passthrough: P): Expression = {
    expression match {
      case operator: BinaryOperator =>
        BinaryOperator.fromLuaToken(
          operator.token,
          visitExpression(operator.left, passthrough),
          visitExpression(operator.right, passthrough)
        )

      case operator: UnaryOperator =>
        UnaryOperator.fromLuaToken(
          operator.token,
          visitExpression(operator.subExpression, passthrough)
        )

      case FunctionExpression(functionBody) =>
        FunctionExpression(
          FunctionBody(
            functionBody.parameters,
            visitBlock(functionBody.body, passthrough)
          )
        )

      case expression: PrefixExpression =>
        visitPrefixExpression(expression, passthrough)

      case LuaNil                    => LuaNil
      case LuaFalse                  => LuaFalse
      case LuaTrue                   => LuaTrue
      case n @ LuaNumber(value, raw) => n
      case s @ LuaString(value, raw) => s
      case v @ VarArgs               => v
      case TableConstructor(fields) =>
        TableConstructor(fields.map(visitField(_, passthrough)))
    }
  }

  def visitStatement(statement: Statement, passthrough: P): Statement = {

    statement match {
      case GlobalDeclaration(variables, expressionList) =>
        GlobalDeclaration(
          variables.map(visitVariable(_, passthrough, global = true)),
          expressionList.map(e => visitExpression(e, passthrough))
        )

      case LocalDeclaration(names, expressionList) =>
        LocalDeclaration(
          names,
          expressionList.map(visitExpression(_, passthrough))
        )

      case lastStatement: LastStatement =>
        lastStatement match {
          case ContinueStatement => ContinueStatement
          case BreakStatement    => BreakStatement
          case ReturnStatement(expressions) =>
            ReturnStatement(
              expressions.map(e => visitExpression(e, passthrough))
            )
          case GotoStatement(name) => GotoStatement(name)
        }

      case LabelDeclaration(name) => LabelDeclaration(name)

      case call: FunctionCall => visitFunctionCall(call, passthrough)

      case DoBlock(body) => DoBlock(visitBlock(body, passthrough))

      case WhileLoop(condition, body) =>
        WhileLoop(
          visitExpression(condition, passthrough),
          visitBlock(body, passthrough)
        )

      case RepeatLoop(body, condition) =>
        RepeatLoop(
          visitBlock(body, passthrough),
          visitExpression(condition, passthrough)
        )

      case IfStatement(condition, body, elseIfs, elseBlock) =>
        IfStatement(
          visitExpression(condition, passthrough),
          visitBlock(body, passthrough),
          elseIfs.map(
            elseIf =>
              ElseIfStatement(
                visitExpression(elseIf.condition, passthrough),
                visitBlock(elseIf.body, passthrough)
            )
          ),
          elseBlock.map(visitBlock(_, passthrough))
        )

      case ForLoop(variableName, initialValue, upperBound, stepValue, block) =>
        ForLoop(
          variableName,
          visitExpression(initialValue, passthrough),
          visitExpression(upperBound, passthrough),
          stepValue.map(visitExpression(_, passthrough)),
          visitBlock(block, passthrough)
        )

      case ForEachLoop(variables, expressions, block) =>
        ForEachLoop(
          variables,
          expressions.map(visitExpression(_, passthrough)),
          visitBlock(block, passthrough)
        )

      case FunctionDefinition(name, body, local) =>
        FunctionDefinition(
          name,
          FunctionBody(body.parameters, visitBlock(body.body, passthrough)),
          local
        )
    }
  }

  def visitStatements(statements: List[Statement],
                      passthrough: P): List[Statement] =
    statements.map(s => visitStatement(s, passthrough))

  def visitBlock(block: Block, passthrough: P): Block = {
    Block(visitStatements(block.statements, passthrough))
  }

}
