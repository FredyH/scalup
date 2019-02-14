package com.scalup
import java.util.regex.Pattern

import enumeratum._

import scala.collection.immutable
import scala.util.matching.Regex

sealed trait LuaToken extends EnumEntry

object LuaToken extends Enum[LuaToken] {
  sealed class KeywordOrOperator(val keywords: String*) extends LuaToken {
    private[scalup] def regex: Regex = keywords.map(s => "(" + Pattern.quote(s) + ")").mkString("|").r

    override def toString: String = keywords.head
  }


  case class IDENTIFIER(name: String) extends LuaToken
  case class STRING(content: String, raw: String) extends LuaToken
  case class NUMBER(value: Double, raw: String) extends LuaToken
  case object MODULUS extends KeywordOrOperator("%")
  case object LESS_THAN extends KeywordOrOperator("<")
  case object LESS_EQUALS extends KeywordOrOperator("<=")
  case object GREATER_THAN extends KeywordOrOperator(">")
  case object GREATER_EQUALS extends KeywordOrOperator(">=")
  case object EQUALS extends KeywordOrOperator("==")
  case object ASSIGNMENT extends KeywordOrOperator("=")
  case object NOT_EQUALS extends KeywordOrOperator("!=", "~=")
  case object LOGICAL_OR extends KeywordOrOperator("||", "or")
  case object LOGICAL_AND extends KeywordOrOperator("&&", "and")
  case object LOGICAL_NOT extends KeywordOrOperator("!", "not")
  case object REPEAT extends KeywordOrOperator("repeat")
  case object UNTIL extends KeywordOrOperator("until")
  case object WHILE extends KeywordOrOperator("while")
  case object DO extends KeywordOrOperator("do")
  case object FOR extends KeywordOrOperator("for")
  case object IN extends KeywordOrOperator("in")
  case object IF extends KeywordOrOperator("if")
  case object THEN extends KeywordOrOperator("then")
  case object ELSEIF extends KeywordOrOperator("elseif")
  case object ELSE extends KeywordOrOperator("else")
  case object FUNCTION extends KeywordOrOperator("function")
  case object END extends KeywordOrOperator("end")
  case object RETURN extends KeywordOrOperator("return")
  case object NIL extends KeywordOrOperator("nil")
  case object BREAK extends KeywordOrOperator("break")
  case object CONTINUE extends KeywordOrOperator("continue")
  case object LOCAL extends KeywordOrOperator("local")
  case object FALSE extends KeywordOrOperator("false")
  case object TRUE extends KeywordOrOperator("true")
  case object TABLE_OPEN extends KeywordOrOperator("{")
  case object TABLE_CLOSE extends KeywordOrOperator("}")
  case object INDEX_OPEN extends KeywordOrOperator("[")
  case object INDEX_CLOSE extends KeywordOrOperator("]")
  case object PLUS extends KeywordOrOperator("+")
  case object MINUS extends KeywordOrOperator("-")
  case object MULTIPLY extends KeywordOrOperator("*")
  case object DIVIDED_BY extends KeywordOrOperator("/")
  case object POW extends KeywordOrOperator("^")
  case object NUMBER_SIGN extends KeywordOrOperator("#")
  case object CONCATENATION extends KeywordOrOperator("..")
  case object DOT extends KeywordOrOperator(".")
  case object COLON extends KeywordOrOperator(":")
  case object COMMA extends KeywordOrOperator(",")
  case object SEMI_COLON extends KeywordOrOperator(";")
  case object TRIPLE_DOT extends KeywordOrOperator("...")
  case object PAR_OPEN extends KeywordOrOperator("(")
  case object PAR_CLOSE extends KeywordOrOperator(")")

  override def values: immutable.IndexedSeq[LuaToken] = findValues

  def keyWordMap: Map[String, KeywordOrOperator] = values.collect { case e : KeywordOrOperator => e }
    .flatMap(token => token.keywords.map(keyword => keyword -> token)).toMap

  //This needs to be in order of descending length to try and match longest keywords first
  def keyWords: List[String] = keyWordMap.keys.toList.sortBy(_.length).reverse

}