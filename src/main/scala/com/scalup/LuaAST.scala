package com.scalup

import com.scalup.LuaToken._

sealed trait LuaAST

sealed trait BinaryOperator extends Expression {
  def left: Expression

  def right: Expression

  def token: KeywordOrOperator
}

object BinaryOperator {
  val tokenMap = Map[LuaToken, (Expression, Expression) => BinaryOperator](
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

  def fromLuaToken(token: LuaToken,
                   left: Expression,
                   right: Expression): BinaryOperator =
    tokenMap(token)(left, right)

}

sealed trait UnaryOperator extends Expression {
  def subExpression: Expression

  override def precedence: Int = 8

  def token: LuaToken
}

object UnaryOperator {
  val tokenMap = Map[LuaToken, Expression => UnaryOperator](
    MINUS -> UnaryMinus,
    LOGICAL_NOT -> UnaryNot,
    NUMBER_SIGN -> UnaryCount
  )

  def fromLuaToken(token: LuaToken, expression: Expression): UnaryOperator =
    tokenMap(token)(expression)
}

sealed trait Statement extends LuaAST

case class GlobalDeclaration(variables: List[Variable],
                             expressionList: List[Expression])
    extends Statement

case class LocalDeclaration(names: List[String],
                            expressionList: List[Expression])
    extends Statement

sealed trait LastStatement extends Statement

case object ContinueStatement extends LastStatement

case object BreakStatement extends LastStatement

case class GotoStatement(label: String) extends LastStatement

case class ReturnStatement(expressions: List[Expression]) extends LastStatement

sealed trait Expression extends LuaAST {
  def precedence: Int = 10
}

sealed trait FunctionCall extends PrefixExpression with Statement {
  def prefixExpression: PrefixExpression

  def arguments: List[Expression]
}

case class FunctionCallWithoutSelf(prefixExpression: PrefixExpression,
                                   arguments: List[Expression])
    extends FunctionCall

case class FunctionCallWithSelf(prefixExpression: PrefixExpression,
                                name: String,
                                arguments: List[Expression])
    extends FunctionCall

case class Block(statements: List[Statement]) extends LuaAST

case class DoBlock(body: Block) extends Statement

case class WhileLoop(condition: Expression, body: Block) extends Statement

case class RepeatLoop(body: Block, condition: Expression) extends Statement

case class ElseIfStatement(condition: Expression, body: Block) extends LuaAST

case class IfStatement(condition: Expression,
                       body: Block,
                       elseIfs: List[ElseIfStatement],
                       elseBlock: Option[Block])
    extends Statement

case class ForLoop(variableName: String,
                   initialValue: Expression,
                   upperBound: Expression,
                   stepValue: Option[Expression],
                   block: Block)
    extends Statement

case class ForEachLoop(variables: List[String],
                       expressions: List[Expression],
                       block: Block)
    extends Statement

case class LabelDeclaration(name: String) extends Statement

case class Parameter(name: String) extends LuaAST {
  override def toString: String = name
}

case class ParameterList(parameters: List[Parameter], hasVarArgs: Boolean)
    extends LuaAST {
  def paramsWithVarArg: List[Parameter] = {
    if (hasVarArgs) parameters :+ Parameter("...")
    else parameters
  }
}

case class FunctionName(name: List[String], withSelf: Boolean) extends LuaAST {
  override def toString: String = {
    if (withSelf) {
      name.dropRight(1).mkString(".") + ":" + name.last
    } else {
      name.mkString(".")
    }
  }
}

case class FunctionDefinition(name: FunctionName,
                              body: FunctionBody,
                              local: Boolean)
    extends Statement

case class FunctionBody(parameters: ParameterList, body: Block) extends LuaAST

case class FunctionExpression(functionBody: FunctionBody) extends Expression

sealed trait PrefixPart
case class NamePrefixPart(name: String) extends PrefixPart
case class IndexPrefixPart(prefix: Expression) extends PrefixPart
case class FunctionCallPrefixPart(args: List[Expression]) extends PrefixPart
case class FunctionCallWithSelfPrefixPart(name: String, args: List[Expression])
    extends PrefixPart

sealed trait PrefixExpression extends Expression

sealed trait Variable extends PrefixExpression

case class ExpressionAsPrefix(expr: Expression) extends PrefixExpression

case class NamedVariable(name: String) extends Variable

case class PrefixedVariable(prefix: PrefixExpression, name: String)
    extends Variable

case class IndexedVariable(prefix: PrefixExpression, expression: Expression)
    extends Variable

case object LuaNil extends Expression

case object LuaFalse extends Expression

case object LuaTrue extends Expression

case class LuaNumber(value: Double, raw: String) extends Expression

case class LuaString(value: String, raw: String) extends Expression

case object VarArgs extends Expression

sealed trait Field extends LuaAST {
  def value: Expression
}

case class FieldWithoutIndex(value: Expression) extends Field

case class FieldByIndex(key: Expression, value: Expression) extends Field

case class FieldByName(key: String, value: Expression) extends Field

case class TableConstructor(fields: List[Field]) extends Expression

case class Plus(left: Expression, right: Expression) extends BinaryOperator {
  val token = PLUS
  override val precedence = 6
}

case class Minus(left: Expression, right: Expression) extends BinaryOperator {
  val token = MINUS
  override val precedence = 6
}

case class Multiplication(left: Expression, right: Expression)
    extends BinaryOperator {
  val token = MULTIPLY
  override val precedence = 7
}

case class Division(left: Expression, right: Expression)
    extends BinaryOperator {
  val token = DIVIDED_BY
  override val precedence = 7
}

case class Modulus(left: Expression, right: Expression) extends BinaryOperator {
  val token = MODULUS
  override val precedence = 7
}

case class Exponentiation(left: Expression, right: Expression)
    extends BinaryOperator {
  val token = POW
  override val precedence = 9
}

case class Concatenation(left: Expression, right: Expression)
    extends BinaryOperator {
  val token = CONCATENATION
  override val precedence = 5
}

case class LessThan(left: Expression, right: Expression)
    extends BinaryOperator {
  val token = LESS_THAN
  override val precedence = 4
}

case class LessEquals(left: Expression, right: Expression)
    extends BinaryOperator {
  val token = LESS_EQUALS
  override val precedence = 4
}

case class GreaterThan(left: Expression, right: Expression)
    extends BinaryOperator {
  val token = GREATER_THAN
  override val precedence = 4
}

case class GreaterEquals(left: Expression, right: Expression)
    extends BinaryOperator {
  val token = GREATER_EQUALS
  override val precedence = 4
}

case class Equals(left: Expression, right: Expression) extends BinaryOperator {
  val token = EQUALS
  override val precedence = 4
}

case class NotEquals(left: Expression, right: Expression)
    extends BinaryOperator {
  val token = NOT_EQUALS
  override val precedence = 4
}

case class And(left: Expression, right: Expression) extends BinaryOperator {
  val token = LOGICAL_AND
  override val precedence = 3
}

case class Or(left: Expression, right: Expression) extends BinaryOperator {
  val token = LOGICAL_OR
  override val precedence = 2
}

case class UnaryMinus(subExpression: Expression) extends UnaryOperator {
  val token = MINUS
}

case class UnaryNot(subExpression: Expression) extends UnaryOperator {
  val token = LOGICAL_NOT
}

case class UnaryCount(subExpression: Expression) extends UnaryOperator {
  val token = NUMBER_SIGN
}
