package com.scalup
import Compactor.{CompactorConfig, intToVarName}

import scala.annotation.tailrec

object Compactor {
  case class CompactorConfig(
    replaceLocalVars: Boolean = true
  )

  val reservedNames = Set("if", "while", "do", "end", "repeat", "until", "client", "server", "require", "util", "math", "debug")

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

//  class GlobalVarWalker extends ASTWalker[Null, List[String]]((L1, L2) => L1 ++ L2, List[String]()) {
//    override
//    def walkVariable(variable: Variable, passthrough: Null, global: Boolean): List[String] = {
//
//    }
//  }


  def compactBlock(block: Block): List[String] = {
    val globalVarWalker = new ASTWalker[Null, List[String]]((L1, L2) => L1 ++ L2, List[String]()) {
      override def walkVariable(variable: Variable, passthrough: Null, global: Boolean): List[String] = {
        if(global)
          variable match {
            case NamedVariable(name) => List(name)
            case PrefixedVariable(prefix, name) => walkExpression(prefix, passthrough)
            case IndexedVariable(prefix, expression) =>
              walkExpression(prefix, passthrough) ++ walkExpression(expression, passthrough)
          }
        else List[String]()
      }
    }

    val globalVariables = globalVarWalker.walkBlock(block, null)
    println(globalVariables)
    globalVariables
  }

}