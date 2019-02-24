package com.scalup
import Compactor.{CompactorConfig, intToVarName}
import com.scalup.visitors.LocalVariableSubVisitor.LocalVarSubData
import com.scalup.visitors.{LocalVariableSubVisitor, ReducerVisitor}

import scala.annotation.tailrec
import scala.collection.mutable

object Compactor {
  case class CompactorConfig(
    shortenTrueFalseLiterals: Boolean = true // Shortens true to !!1 and false to !1
  )

  val reservedNames =
    Set("if", "while", "do", "end", "repeat", "until", "client", "server", "require", "util", "math", "debug", "G", "util", "math", "MsgN", "print", "break")

  val variableCharPool = ('a' to 'z') ++ ('A' to 'Z')

  def intToVarName(index: Int): String = {
    @tailrec
    def genVar(index: Int, currentName: String = ""): String = {
      if(index > 51)
        genVar((index / 52) - 1, currentName + variableCharPool(index % 52))
      else
        variableCharPool(index) + currentName
    }
    genVar(index)
  }
}

class Compactor(config: CompactorConfig = CompactorConfig()) {

  def compactBlock(block: Block): Block = {
    val globalVarWalker = new ReducerVisitor[Null, List[String]]((L1, L2) => L1 ++ L2, List[String]()) {
      override def visitVariable(variable: Variable, passthrough: Null, global: Boolean): List[String] = {
        if(global)
          variable match {
            case NamedVariable(name) => List(name)
            case PrefixedVariable(prefix, name) => visitExpression(prefix, passthrough)
            case IndexedVariable(prefix, expression) =>
              visitExpression(prefix, passthrough) ++ visitExpression(expression, passthrough)
          }
        else List[String]()
      }
    }

    val globalVariables: Set[String] = globalVarWalker.visitBlock(block, null).toSet
    
    val localVariableReplacementNames =
      Iterator.from(0)
        .map(intToVarName)
        .filter(n => !Compactor.reservedNames.contains(n) && !globalVariables.contains(n))

    val localVariableSubVisitor = new LocalVariableSubVisitor(config.shortenTrueFalseLiterals)
    localVariableSubVisitor.visitBlock(block, LocalVarSubData(localVariableReplacementNames, mutable.Map()))
  }

}