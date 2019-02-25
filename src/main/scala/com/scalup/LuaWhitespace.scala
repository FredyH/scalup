package com.scalup

import fastparse.ParsingRun
import fastparse.internal.Util

import scala.annotation.{switch, tailrec}

//Mostly copied from JavaWhitespace
object LuaWhitespace {
  implicit val whitespace = { implicit ctx: ParsingRun[_] =>
    val input = ctx.input
    val startIndex = ctx.index

    @tailrec def rec(current: Int, state: Int, blockEquals: Int = 0, remainingBlockEquals: Int = 0): ParsingRun[Unit] = {
      if (!input.isReachable(current)) {
        if (state == 0 || state == 1 || state == 5) ctx.freshSuccessUnit(current)
        else if (state == 2) ctx.freshSuccessUnit(current - 1)
        else {
          ctx.cut = true
          val res = ctx.freshFailure(current)
          if (ctx.verboseFailures) ctx.setMsg(startIndex, () => Util.literalize("*/"))
          res
        }
      } else {
        val currentChar = input(current)
        (state: @switch) match {
          case 0 =>
            (currentChar: @switch) match {
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state)
              case '/' => rec(current + 1, state = 2)
              case '-' => rec(current + 1, state = 2)
              case _ => ctx.freshSuccessUnit(current)
            }
          case 1 => rec(current + 1, state = if (currentChar == '\n') 0 else state)
          case 2 =>
            (currentChar: @switch) match {
              case '/' => rec(current + 1, state = 1)
              case '-' => rec(current + 1, state = 5)
              case '*' => rec(current + 1, state = 3)
              case _ => ctx.freshSuccessUnit(current - 1)
            }
          case 3 => rec(current + 1, state = if (currentChar == '*') 4 else state)
          case 4 =>
            (currentChar: @switch) match {
              case '/' => rec(current + 1, state = 0)
              case '*' => rec(current + 1, state = 4)
              case _ => rec(current + 1, state = 3)
            }
          case 5 =>
            (currentChar: @switch) match {
              case '[' => rec(current + 1, state = 6)
              case '\n' => rec(current + 1, state = 0)
              case _ => rec(current + 1, state = 1)
            }
          case 6 =>
            (currentChar: @switch) match {
              case '=' => rec(current + 1, state = 6, blockEquals = blockEquals + 1)
              case '[' => rec(current + 1, state = 7, blockEquals)
              case '\n' => rec(current + 1, state = 0)
              case _ => rec(current + 1, state = 1)
            }
          case 7 =>
            (currentChar: @switch) match {
              case ']' => rec(current + 1, state = 8, blockEquals, blockEquals)
              case _ => rec(current + 1, state = 7, blockEquals)
            }
          case 8 =>
            (currentChar: @switch) match {
              case '=' => rec(current + 1, state = 8, blockEquals, remainingBlockEquals - 1)
              case ']' =>
                if (remainingBlockEquals == 0) {
                  rec(current + 1, state = 0)
                } else {
                  rec(current + 1, state = 7, blockEquals)
                }
              case _ => rec(current + 1, state = 7, blockEquals)
            }
        }
      }
    }

    rec(current = ctx.index, state = 0)
  }
}