package com.scalup.visitors
import com.scalup._
import com.scalup.visitors.LocalVariableSubVisitor.LocalVarSubData

import scala.collection.mutable


// Walks the AST and replaces local variable names with ones from the supplied iterator. Used for minification or obfuscation

object LocalVariableSubVisitor {
	case class LocalVarSubData(var variableNames: Iterator[String], nameSubs: mutable.Map[String, String]) {
		override def clone: LocalVarSubData = {
			variableNames.duplicate match {
				case (i1, i2) =>
					this.variableNames = i1 // Reassign this objects variables names because we destroyed the og iterator by duplicating it
					LocalVarSubData(i2, nameSubs.clone())
			}
		}

		def subVarName(originalName: String): String = {
			if(originalName == "self")
				"self"
			else {
				if(nameSubs.contains(originalName)) {
					nameSubs(originalName)
				} else {
					val newName = variableNames.next()
					nameSubs(originalName) = newName
					newName
				}
			}
		}

		// Returns the variables substituted name or the originally passed name if there is no existing substitution
		def getSubNameOrOrig(varName: String): String =
			nameSubs.getOrElse(varName, varName)

	}
}

class LocalVariableSubVisitor(subTrueFalseLiterals: Boolean) extends TreeBuildVisitor[LocalVarSubData] {

	val (luaTrueVal, luaFalseVal): (Expression, Expression) = {
		if(subTrueFalseLiterals) {
			(UnaryNot(UnaryNot(LuaNumber(1, "1"))), UnaryNot(LuaNumber(1, "1")))
		} else
			(LuaTrue, LuaFalse)
	}

	override def visitVariable(variable: Variable, passthrough: LocalVarSubData, global: Boolean = false): Variable =
		if(!global) {
			variable match {
				case NamedVariable(name) => NamedVariable(passthrough.getSubNameOrOrig(name))
				case PrefixedVariable(prefix, name) => PrefixedVariable(visitPrefixExpression(prefix, passthrough), name)
				case IndexedVariable(prefix, expression) =>
					IndexedVariable(visitPrefixExpression(prefix, passthrough), visitExpression(expression, passthrough))
			}
		} else super.visitVariable(variable, passthrough)

	override def visitFunctionCall(call: FunctionCall, passthrough: LocalVarSubData): FunctionCall =
		call match {
			case f: FunctionCallWithoutSelf => super.visitFunctionCall(f, passthrough)

			case FunctionCallWithSelf(prefix, name, arguments) =>
				FunctionCallWithSelf(
					visitPrefixExpression(prefix, passthrough),
					passthrough.getSubNameOrOrig(name),
					arguments.map(visitExpression(_, passthrough))
				)
		}

	override def visitExpression(expression: Expression, passthrough: LocalVarSubData): Expression = {
		expression match {

			case FunctionExpression(body) =>
				FunctionExpression(
					FunctionBody(
						body.parameters.copy(parameters = body.parameters.parameters.map(p => Parameter(passthrough.subVarName(p.name)))),
						visitBlock(body.body, passthrough)
					)
				)

			case LuaFalse => luaFalseVal
			case LuaTrue => luaTrueVal

			case _ => super.visitExpression(expression, passthrough)
		}
	}

	override def visitStatement(statement: Statement, passthrough: LocalVarSubData): Statement = {

		statement match {

			case LocalDeclaration(names, expressionList) =>
				val passthroughCopy = passthrough.clone
				LocalDeclaration(names.map(passthrough.subVarName), expressionList.map(visitExpression(_, passthroughCopy)))

			case ForLoop(variableName, initialValue, upperBound, stepValue, block) =>
				ForLoop(
					passthrough.subVarName(variableName),
					visitExpression(initialValue, passthrough),
					visitExpression(upperBound, passthrough),
					stepValue.map(visitExpression(_, passthrough)),
					visitBlock(block, passthrough)
				)

			case ForEachLoop(variables, expressions, block) =>
				ForEachLoop(
					variables.map(passthrough.subVarName),
					expressions.map(visitExpression(_, passthrough)),
					visitBlock(block, passthrough)
				)

			case FunctionDefinition(name, body, local) =>
				FunctionDefinition(
					if(local)
						FunctionName(name.name.splitAt(1) match { case (head, rest) => passthrough.subVarName(head.head) +: rest }, name.withSelf)
					else name,
					FunctionBody(
						body.parameters.copy(parameters = body.parameters.parameters.map(p => Parameter(passthrough.subVarName(p.name)))),
						visitBlock(body.body, passthrough)
					),
					local
				)

			case _ => super.visitStatement(statement, passthrough)
		}
	}

	override def visitStatements(statements: List[Statement], passthrough: LocalVarSubData): List[Statement] =
		statements.map(s => visitStatement(s, passthrough))

	override def visitBlock(block: Block, passthrough: LocalVarSubData): Block =
		Block(visitStatements(block.statements, passthrough.clone))


}
