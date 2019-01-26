package com.scalup

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}


class LuaTokenReader(tokens: Seq[LuaToken]) extends Reader[LuaToken] {
  override def first: LuaToken = tokens.head

  override def rest: Reader[LuaToken] = new LuaTokenReader(tokens.tail)

  override def pos: Position = NoPosition

  override def atEnd: Boolean = tokens.isEmpty
}
