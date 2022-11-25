/*
 * Copyright 2022 ThatScalaGuy
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.thatscalaguy.circe.jq

import cats.parse.Parser
import cats.parse.Rfc5234.{alpha, dquote, digit}
import cats.parse.Numbers.nonNegativeIntString

package object parser {

  val combinedParser: Parser[Output] =
    (FilterParser.parser.backtrack | ArrayParser.parser.backtrack | ObjectParser.parser)

  private[parser] lazy val rec = Parser.string("..")
  private[parser] lazy val dot = Parser.char('.')
  private[parser] lazy val space = Parser.char(' ')
  private[parser] lazy val comma = Parser.char(',')
  private[parser] lazy val pipe = Parser.char('|')
  private[parser] lazy val colon = Parser.char(':')
  private[parser] lazy val lbracket = Parser.char('[')
  private[parser] lazy val rbracket = Parser.char(']')
  private[parser] lazy val lrbracket = Parser.char('(')
  private[parser] lazy val rrbracket = Parser.char(')')
  private[parser] lazy val lcbracket = Parser.char('{')
  private[parser] lazy val rcbracket = Parser.char('}')
  private[parser] lazy val line = Parser.charIn('_')
  private[parser] lazy val string =
    (alpha | digit | line).surroundedBy(dquote.?).repAs[String]
  private[parser] lazy val num =
    (Parser
      .char('-')
      .?
      .with1 ~ nonNegativeIntString ~ (dot ~ nonNegativeIntString).?).string
      .map(_.toDouble)

  private[parser] lazy val field =
    ((space.?.with1 *> dot) *> (alpha | line).repAs[String])

  private[parser] lazy val optional = Parser.char('?').?
}
