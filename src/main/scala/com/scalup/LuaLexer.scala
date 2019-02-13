package com.scalup

import com.scalup.LuaToken.{LOGICAL_OR, NOT_EQUALS, _}
import org.apache.commons.text.StringEscapeUtils
import fastparse._
import NoWhitespace._


trait LuaLexer {
  def identifier[_: P]: P[IDENTIFIER] =
    P(CharIn("A-Za-z_")~CharIn("A-Za-z_0-9").rep).!
      .filter(n => !keyWords.contains(n))
      .map(IDENTIFIER)

  def number[_: P] = {
    def digits(min: Int = 0) = P(CharIn("0-9").rep(min))
    def plusOrMinus = CharIn("\\-+")
    def decimalPlaces = "." ~ digits(1) ~ exponentialPart.?
    def hexDigits(min: Int = 0) = P(CharIn("0-9A-F").rep(min))
    def exponentialPart = P(CharIn("eE") ~/ CharIn("\\-+").? ~ digits(1))
    def hexNumber = P(StringIn("0x") ~/ hexDigits(1)).!.map(str => Integer.parseInt(str.substring(2), 16).toDouble)
    def floatWithoutLeading = P(plusOrMinus.? ~ decimalPlaces.?).!.filter(_.nonEmpty).map(_.toDouble)
    def float = P(plusOrMinus.? ~ digits(1) ~ decimalPlaces.?).!.filter(_.nonEmpty).map(_.toDouble)
    P(hexNumber | floatWithoutLeading | float)
  }

  def digits[_: P](min: Int = 1): P[Unit] =
    P(CharIn("0-9").rep(1))

  def `=`[_: P]: P[Unit] = P("=")
  def `%`[_: P]: P[KeywordOrOperator] = P("%").map(_ => MODULUS)
  def `<`[_: P]: P[KeywordOrOperator] = P("<").map(_ => LESS_THAN)
  def `<=`[_: P]: P[KeywordOrOperator] = P("<=").map(_ => LESS_EQUALS)
  def `>`[_: P]: P[KeywordOrOperator] = P(">").map(_ => GREATER_THAN)
  def `>=`[_: P]: P[KeywordOrOperator] = P(">=").map(_ => GREATER_EQUALS)
  def `==`[_: P]: P[KeywordOrOperator] = P("==").map(_ => EQUALS)
  def `!=`[_: P]: P[KeywordOrOperator] = P(StringIn("!=", "~=")).map(_ => NOT_EQUALS)
  def `||`[_: P]: P[KeywordOrOperator] = P(StringIn("||", "or")).map(_ => LOGICAL_OR)
  def `&&`[_: P]: P[KeywordOrOperator] = P(StringIn("&&", "and")).map(_ => LOGICAL_AND)
  def `!`[_: P]: P[KeywordOrOperator] = P(StringIn("!", "not")).map(_ => LOGICAL_NOT)
  def `repeat`[_: P]: P[Unit] = P("repeat")
  def `until`[_: P]: P[Unit] = P("until")
  def `while`[_: P]: P[Unit] = P("while")
  def `do`[_: P]: P[Unit] = P("do")
  def `for`[_: P]: P[Unit] = P("for")
  def `in`[_: P]: P[Unit] = P("in")
  def `if`[_: P]: P[Unit] = P("if")
  def `then`[_: P]: P[Unit] = P("then")
  def `elseif`[_: P]: P[Unit] = P("elseif")
  def `else`[_: P]: P[Unit] = P("else")
  def `function`[_: P]: P[Unit] = P("function")
  def `end`[_: P]: P[Unit] = P("end")
  def `return`[_: P]: P[Unit] = P("return")
  def `nil`[_: P]: P[Unit] = P("nil")
  def `break`[_: P]: P[Unit] = P("break")
  def `continue`[_: P]: P[Unit] = P("continue")
  def `local`[_: P]: P[Unit] = P("local")
  def `false`[_: P]: P[Unit] = P("false")
  def `true`[_: P]: P[Unit] = P("true")
  def `{`[_: P]: P[Unit] = P("{")
  def `}`[_: P]: P[Unit] = P("}")
  def `[`[_: P]: P[Unit] = P("[")
  def `]`[_: P]: P[Unit] = P("]")
  def `+`[_: P]: P[KeywordOrOperator] = P("+").map(_ => PLUS)
  def `-`[_: P]: P[KeywordOrOperator] = P("-").map(_ => MINUS)
  def `#`[_: P]: P[KeywordOrOperator] = P("#").map(_ => NUMBER_SIGN)
  def `*`[_: P]: P[KeywordOrOperator] = P("*").map(_ => MULTIPLY)
  def `/`[_: P]: P[KeywordOrOperator] = P("/").map(_ => DIVIDED_BY)
  def `^`[_: P]: P[Unit] = P("^")
  def `..`[_: P]: P[KeywordOrOperator] = P("..").map(_ => CONCATENATION)
  def `.`[_: P]: P[Unit] = P(".")
  def `:`[_: P]: P[Unit] = P(":")
  def `,`[_: P]: P[Unit] = P(",")
  def `;`[_: P]: P[Unit] = P(";")
  def `...`[_: P]: P[Unit] = P("...")
  def `(`[_: P]: P[Unit] = P("(")
  def `)`[_: P]: P[Unit] = P(")")

  //TODO: Strings don't handle things like \\" correctly, which should close the string
  def doubleQuotedString[_: P]: P[STRING] =
    P("\"" ~ ("\\\"" | CharPred(c => c != '"' && c != '\n' && c != '\r')).rep ~ "\"").!
      .map{ str =>
        val content = StringEscapeUtils.unescapeJava(str.slice(1, str.length - 1))
        STRING(StringEscapeUtils.unescapeJava(content), str)
      }

  def singleQuotedString[_: P]: P[STRING] =
    P("'" ~ ("\\\"" | CharPred(c => c != ''' && c != '\n' && c != '\r')).rep ~ "'").!
      .map{ str =>
        val content = StringEscapeUtils.unescapeJava(str.slice(1, str.length - 1))
        STRING(StringEscapeUtils.unescapeJava(content), str)
      }

  def multilineString[_: P]: P[STRING] =
    P("[[" ~ ((!"]]") ~ AnyChar).rep ~ "]]").!.map{ str =>
      val content = str.slice(2, str.length - 2)
      STRING(content, str)
    }

  def string[_: P]: P[STRING] =
    P(singleQuotedString | doubleQuotedString | multilineString)


}