package com.scalup.visitors

import com.scalup._

abstract class AbstractVisitor[P, +R] {

  def visitField(field: Field, passthrough: P): R

  def visitVariable(variable: Variable,
                    passthrough: P,
                    global: Boolean = false): R

  def visitFunctionCall(call: FunctionCall, passthrough: P): R

  def visitPrefixExpression(prefixExpression: PrefixExpression,
                            passthrough: P): R

  def visitExpression(expression: Expression, passthrough: P): R

  def visitStatement(statement: Statement, passthrough: P): R

  def visitStatements(statements: List[Statement], passthrough: P): List[R]

  def visitBlock(block: Block, passthrough: P): R
}
