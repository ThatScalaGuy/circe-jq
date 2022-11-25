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

package de.thatscalaguy.circe.jq.parser

import cats.parse.Rfc5234.{alpha}
import cats.parse.Parser
import de.thatscalaguy.circe.jq.Pair
import de.thatscalaguy.circe.jq
import scala.annotation.meta.companionObject

object ObjectParser {

  private val name: Parser[String] =
    space.?.with1 *> (alpha | line)
      .repAs[String]

  private val pair: Parser[Pair] =
    ((FilterParser.expression.backtrack | name) ~ colon.surroundedBy(
      space.?
    ) ~ (FilterParser.parser.backtrack | ArrayParser.parser))
      .map { case ((k, _), v) =>
        Pair(k, v)
      }

  val parser: Parser[jq.Object] =
    pair
      .repSep(comma)
      .between(lcbracket, rcbracket)
      .map(jq.Object.apply)

}
