package com.scalup

import java.io.Reader
import java.util.regex.Pattern

import com.scalup.LuaToken._
import org.apache.commons.text.StringEscapeUtils

import scala.util.{Failure => TryFailure, Success => TrySuccess, Try}
import scala.util.parsing.combinator.RegexParsers

object LuaLexer extends RegexParsers {
  def identifier: Parser[IDENTIFIER] = "[A-Za-z][A-Za-z0-9_]*".r ^^ { str => IDENTIFIER(str) }

  def number: Parser[NUMBER] = """[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?""".r ^^ { str => NUMBER(str.toDouble) }

  def doubleQuotedString: Parser[STRING] = """"[^"\\]*(\\.[^"\\]*)*"""".r ^^ { str  =>
    STRING(StringEscapeUtils.unescapeJava(str), false)
  }

  def singleQuotedString: Parser[STRING] = """'[^'\\]*(\\.[^'\\]*)*'""".r ^^ { str  =>
    STRING(StringEscapeUtils.unescapeJava(str), false)
  }

  def multilineString: Parser[STRING] = """\[((=*)\[(.|\n|\r)*?)\]\2\]""".r ^^ { str  => STRING(str, true) }

  private val keyWordRegex = keyWords.map(s => "(" + Pattern.quote(s) + ")").mkString("|")
  def keyWordOrOperator: Parser[KeywordOrOperator] = keyWordRegex.r ^^ { str => LuaToken.keyWordMap(str) }

  def tokenizeAll: Parser[List[LuaToken]] =
    (multilineString |  doubleQuotedString | singleQuotedString  | number | keyWordOrOperator | identifier).*

  case class ParsingFailedException(reason: String) extends RuntimeException(reason)

  //TODO: Also allow Reader/etc.
  def tokenizeLuaFile(input: String): Try[List[LuaToken]] = {
    val result = parseAll(tokenizeAll, input)
    result match {
      case Success(res, _) => TrySuccess(res)
      case NoSuccess(reason, _) => TryFailure(ParsingFailedException(reason))
    }
  }

  def tokenizeLuaFile(input: Reader): Try[List[LuaToken]] = {
    val result = parseAll(tokenizeAll, input)
    result match {
      case Success(res, _) => TrySuccess(res)
      case NoSuccess(reason, _) => TryFailure(ParsingFailedException(reason))
    }
  }

  def main(args: Array[String]): Unit = {
    println(tokenizeLuaFile(
      """local test = 5 function hello() return "FUCKING RETARD\" MATE" .. [[ayy
        rofl]] + 5.31234e123 end"""))
  }
}
