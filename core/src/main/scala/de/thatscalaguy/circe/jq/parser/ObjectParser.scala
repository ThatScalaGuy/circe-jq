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
import de.thatscalaguy.circe.jq.{ListFieldTerm, FieldTerm, ListTerm}
import cats.data.NonEmptyList

object ObjectParser {

  private val name: Parser[String] =
    (alpha | line).repAs[String]

  private lazy val value: Parser[jq.Output] =
    Parser.defer(
      FilterParser.singleExpr.backtrack |
        ArrayParser.parser.backtrack |
        parser.backtrack
    )

  private lazy val pairWithColon: Parser[Pair] =
    (space.?.with1 *> (FilterParser.expression.backtrack | name) ~ colon
      .surroundedBy(space.?) ~ value)
      .map { case ((k, _), v) => Pair(k, v) }

  private lazy val pairShorthand: Parser[Pair] =
    (space.?.with1 *> name).map { n =>
      // `{foo}` is shorthand for `{foo: .foo}`
      val term = ListFieldTerm(NonEmptyList.one(FieldTerm(n, optional = false)))
      val filter = jq.Filter(NonEmptyList.one(ListTerm(NonEmptyList.one(term))))
      Pair(n, filter)
    }

  private lazy val pair: Parser[Pair] = pairWithColon.backtrack | pairShorthand

  val parser: Parser[jq.Object] =
    pair
      .repSep(comma.surroundedBy(space.?))
      .between(lcbracket.surroundedBy(space.?), rcbracket.surroundedBy(space.?))
      .map(jq.Object.apply)

}
