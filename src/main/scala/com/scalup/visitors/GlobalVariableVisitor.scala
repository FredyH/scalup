package com.scalup.visitors

import com.scalup._

class GlobalVariableVisitor extends ReducerVisitor[Unit, Set[String]]((a, b) => a ++ b, Set.empty) {
  override def visitField(field: Field, passthrough: Unit): Set[String] = field match {
    case FieldWithoutIndex(value) => visitExpression(value, passthrough)
    case FieldByIndex(key, value) => visitExpression(key, passthrough) ++ visitExpression(key, passthrough)
    case FieldByName(key, value) => visitExpression(value, passthrough)
  }

  override def visitVariable(variable: Variable, passthrough: Unit, global: Boolean): Set[String] = variable match {
    case NamedVariable(name) => Set(name)
    case PrefixedVariable(prefix, name) => visitPrefixExpression(prefix, passthrough)
    case IndexedVariable(prefix, expression) =>
      visitPrefixExpression(prefix, passthrough) ++ visitExpression(expression, passthrough)
  }

  override def visitFunctionCall(call: FunctionCall, passthrough: Unit): Set[String] = {
    visitPrefixExpression(call.prefixExpression, passthrough) ++
      call.arguments.flatMap(arg => visitExpression(arg, passthrough))
  }


  override def visitExpression(expression: Expression, passthrough: Unit): Set[String] = expression match {
    case FunctionExpression(functionBody) =>
      visitBlock(functionBody.body, passthrough) -- functionBody.parameters.parameters.map(_.name)
    case _ => super.visitExpression(expression, passthrough)
  }

  override def visitStatement(statement: Statement, passthrough: Unit): Set[String] =
    throw new RuntimeException("This should never be called")

  override def visitStatements(statements: List[Statement], passthrough: Unit): List[Set[String]] =
    throw new RuntimeException("This should never be called")

  override def visitBlock(block: Block, passthrough: Unit): Set[String] =
    block.statements.foldLeft((Set.empty[String], Set.empty[String])) {
      case ((localVars, globalVars), b) =>
        b match {
          case GlobalDeclaration(variables, expressionList) =>
            val newVars = variables.flatMap(v => visitVariable(v, passthrough)) ++
              expressionList.flatMap(expr => visitExpression(expr, passthrough))
            (localVars, globalVars ++ (newVars.toSet -- localVars))
          case LocalDeclaration(names, expressionList) =>
            val newVars = expressionList.flatMap(expr => visitExpression(expr, passthrough))
            (localVars ++ names, globalVars ++ (newVars.toSet -- localVars))
          case ReturnStatement(exprs) =>
            val newVars = exprs.flatMap(expr => visitExpression(expr, passthrough))
            (localVars, globalVars ++ (newVars.toSet -- localVars))
          case ContinueStatement | BreakStatement =>
            (localVars, globalVars)
          case call: FunctionCall =>
            (localVars, globalVars ++ (visitFunctionCall(call, passthrough) -- localVars))
          case DoBlock(body) =>
            (localVars, globalVars ++ (visitBlock(body, passthrough) -- localVars))
          case WhileLoop(condition, body) =>
            val whileVars = visitExpression(condition, passthrough) ++ visitBlock(body, passthrough)
            (localVars, globalVars ++ (whileVars -- localVars))
          case RepeatLoop(body, condition) =>
            val repeatVars = visitExpression(condition, passthrough) ++ visitBlock(body, passthrough)
            (localVars, globalVars ++ (repeatVars -- localVars))
          case IfStatement(condition, body, elseIfs, elseBlock) =>
            val conditionVars = visitExpression(condition, passthrough)
            val elseIfVars = elseIfs.flatMap { e =>
              visitExpression(e.condition, passthrough) ++ visitBlock(e.body, passthrough)
            }
            val elseVars = elseBlock.map(b => visitBlock(b, passthrough)).getOrElse(List.empty)
            val totalVars = conditionVars ++ elseIfVars ++ elseVars
            (localVars, globalVars ++ (totalVars -- localVars))
          case ForLoop(variableName, initialValue, upperBound, stepValue, block) =>
            val initialVars = visitExpression(initialValue, passthrough)
            val upperVars = visitExpression(upperBound, passthrough)
            val stepVars = stepValue.map(expr => visitExpression(expr, passthrough)).getOrElse(Set.empty)
            val blockVars = visitBlock(block, passthrough)
            val totalVars = upperVars ++ stepVars ++ blockVars
            val newLocalVars = localVars + variableName
            (newLocalVars, globalVars ++ (totalVars -- newLocalVars))
          case ForEachLoop(variables, expressions, block) =>
            val expressionVars = expressions.flatMap(expr => visitExpression(expr, passthrough))
            val blockVars = visitBlock(block, passthrough)
            val totalVars = (expressionVars ++ blockVars).toSet
            val newLocalVars = localVars ++ variables
            (newLocalVars, globalVars ++ (totalVars -- newLocalVars))
          case FunctionDefinition(name, body, true) =>
            val firstFuncName = name.name.head
            val newLocalVars = (localVars + firstFuncName) ++ body.parameters.parameters.map(_.name) ++
              (if (name.withSelf) Set("self") else Set.empty)
            val newGlobalVars = visitBlock(body.body, passthrough)
            (newLocalVars, globalVars ++ (newGlobalVars -- newLocalVars))
          case FunctionDefinition(name, body, false) =>
            val firstFuncName = name.name.head
            val newLocalVars = localVars ++ body.parameters.parameters.map(_.name) ++
              (if (name.withSelf) Set("self") else Set.empty)
            val newGlobalVars = visitBlock(body.body, passthrough) + firstFuncName
            (newLocalVars, globalVars ++ (newGlobalVars -- newLocalVars))
        }
    }._2
}
