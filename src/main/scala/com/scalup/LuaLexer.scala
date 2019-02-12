package com.scalup

import java.util.regex.Pattern

import com.scalup.LuaToken._
import org.apache.commons.text.StringEscapeUtils
import scala.util.parsing.combinator.RegexParsers

trait LuaLexer extends RegexParsers {
  def identifier: Parser[IDENTIFIER] = "[A-Za-z][A-Za-z0-9_]*".r.filter(n => !keyWords.contains(n)) ^^ { str => IDENTIFIER(str) }

  def number: Parser[NUMBER] = """[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?""".r ^^ { str => NUMBER(str.toDouble) }

  def doubleQuotedString: Parser[STRING] =
    """"[^"\\]*(\\.[^"\\]*)*"""".r ^^ { str =>
      val content = str.slice(1, str.length - 1)
      STRING(StringEscapeUtils.unescapeJava(content), str)
    }

  def singleQuotedString: Parser[STRING] =
    """'[^'\\]*(\\.[^'\\]*)*'""".r ^^ { str =>
      val content = str.slice(1, str.length - 1)
      STRING(StringEscapeUtils.unescapeJava(content), str)
    }

  def multilineString: Parser[STRING] = """\[((=*)\[(.|\n|\r)*?)\]\2\]""".r ^^ { str =>
    val content = str.slice(2, str.length - 2)
    STRING(content, str)
  }

  def string: Parser[STRING] = doubleQuotedString | singleQuotedString | multilineString

  def keyword[T <: KeywordOrOperator](p: T): Parser[T] = p.regex ^^ { _ => p }
}
