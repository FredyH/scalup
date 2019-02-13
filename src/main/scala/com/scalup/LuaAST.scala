package com.scalup

import com.scalup.LuaToken._

sealed trait LuaAST

sealed trait BinaryOperator extends Expression {
  def left: Expression

  def right: Expression
}

object BinaryOperator {
  def fromLuaToken(token: LuaToken, left: Expression, right: Expression): BinaryOperator = {
    val map = Map[LuaToken, (Expression, Expression) => BinaryOperator](
      LOGICAL_OR -> Or,
      LOGICAL_AND -> And,
      GREATER_THAN -> GreaterThan,
      GREATER_EQUALS -> GreaterEquals,
      LESS_THAN -> LessThan,
      LESS_EQUALS -> LessEquals,
      NOT_EQUALS -> NotEquals,
      EQUALS -> Equals,
      CONCATENATION -> Concatenation,
      PLUS -> Plus,
      MINUS -> Minus,
      MULTIPLY -> Multiplication,
      DIVIDED_BY -> Division,
      MODULUS -> Modulus
    )
    map(token)(left, right)
  }
}

sealed trait UnaryOperator extends Expression {
  def subExpression: Expression
}

sealed trait Statement extends LuaAST

case class GlobalDeclaration(variables: List[Variable], expressionList: List[Expression]) extends Statement

case class LocalDeclaration(names: List[String], expressionList: List[Expression]) extends Statement

sealed trait LastStatement extends Statement

case object ContinueStatement extends LastStatement

case object BreakStatement extends LastStatement

case class ReturnStatement(expressions: List[Expression]) extends LastStatement

sealed trait Expression extends LuaAST

sealed trait FunctionCall extends PrefixExpression with Statement {
  def prefixExpression: PrefixExpression

  def arguments: List[Expression]
}

case class FunctionCallWithoutSelf(prefixExpression: PrefixExpression, arguments: List[Expression]) extends FunctionCall

case class FunctionCallWithSelf(prefixExpression: PrefixExpression, name: String, arguments: List[Expression]) extends FunctionCall

case class Block(statements: List[Statement]) extends LuaAST

case class DoBlock(body: Block) extends Statement

case class WhileLoop(condition: Expression, body: Block) extends Statement

case class RepeatLoop(body: Block, condition: Expression) extends Statement

case class ElseIfStatement(condition: Expression, body: Block) extends LuaAST

case class IfStatement(condition: Expression, body: Block, elseIfs: List[ElseIfStatement], elseBlock: Option[Block]) extends Statement

case class ForLoop(variableName: String, initialValue: Expression, upperBound: Expression, stepValue: Option[Expression], block: Block) extends Statement

case class ForEachLoop(variables: List[String], expressions: List[Expression], block: Block) extends Statement

case class Parameter(name: String) extends LuaAST

case class ParameterList(parameters: List[Parameter], hasVarArgs: Boolean) extends LuaAST

case class FunctionName(name: List[String], withSelf: Boolean) extends LuaAST

case class FunctionDefinition(name: FunctionName, body: FunctionBody, local: Boolean) extends Statement

case class FunctionBody(parameters: ParameterList, body: Block) extends LuaAST

case class FunctionExpression(functionBody: FunctionBody) extends Expression


sealed trait PrefixPart
case class NamePrefixPart(name: String) extends PrefixPart
case class IndexPrefixPart(prefix: Expression) extends PrefixPart
case class FunctionCallPrefixPart(args: List[Expression]) extends PrefixPart
case class FunctionCallWithSelfPrefixPart(name: String, args: List[Expression]) extends PrefixPart

sealed trait PrefixExpression extends Expression

sealed trait Variable extends PrefixExpression

case class ExpressionAsPrefix(expr: Expression) extends PrefixExpression

case class NamedVariable(name: String) extends Variable

case class PrefixedVariable(prefix: PrefixExpression, name: String) extends Variable

case class IndexedVariable(prefix: PrefixExpression, expression: Expression) extends Variable

case object LuaNil extends Expression

case object LuaFalse extends Expression

case object LuaTrue extends Expression

case class LuaNumber(value: Double) extends Expression

case class LuaString(value: String, raw: String) extends Expression

case object VarArgs extends Expression

sealed trait Field extends LuaAST {
  def value: Expression
}

case class FieldWithoutIndex(value: Expression) extends Field

case class FieldByIndex(key: Option[Expression], value: Expression) extends Field

case class FieldByName(key: String, value: Expression) extends Field

case class TableConstructor(fields: List[Field]) extends Expression

case class Plus(left: Expression, right: Expression) extends BinaryOperator

case class Minus(left: Expression, right: Expression) extends BinaryOperator

case class Multiplication(left: Expression, right: Expression) extends BinaryOperator

case class Division(left: Expression, right: Expression) extends BinaryOperator

case class Modulus(left: Expression, right: Expression) extends BinaryOperator

case class Exponentiation(left: Expression, right: Expression) extends BinaryOperator

case class Concatenation(left: Expression, right: Expression) extends BinaryOperator

case class LessThan(left: Expression, right: Expression) extends BinaryOperator

case class LessEquals(left: Expression, right: Expression) extends BinaryOperator

case class GreaterThan(left: Expression, right: Expression) extends BinaryOperator

case class GreaterEquals(left: Expression, right: Expression) extends BinaryOperator

case class Equals(left: Expression, right: Expression) extends BinaryOperator

case class NotEquals(left: Expression, right: Expression) extends BinaryOperator

case class And(left: Expression, right: Expression) extends BinaryOperator

case class Or(left: Expression, right: Expression) extends BinaryOperator

case class UnaryMinus(subExpression: Expression) extends UnaryOperator

case class UnaryNot(subExpression: Expression) extends UnaryOperator

case class UnaryCount(subExpression: Expression) extends UnaryOperator