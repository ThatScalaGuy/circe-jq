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
import cats.data.NonEmptyList

object exceptions {
  final class InvalidExpression private (message: String)
      extends Throwable(message) {
    def this(query: String, error: Parser.Error) = {
      this(message = s"can't parse expression: ${query} ${error.toString()}")
    }
  }

  final class InvalidFilter(terms: NonEmptyList[Term])
      extends Throwable(s"unable to handle expression $terms")

  final class InvalidTermType(term: Term)
      extends Throwable(s"invalid term type $term")

}
