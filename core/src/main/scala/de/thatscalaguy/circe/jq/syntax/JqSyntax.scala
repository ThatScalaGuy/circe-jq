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

package de.thatscalaguy.circe.jq.syntax

import de.thatscalaguy.circe.jq.parser.combinedParser
import de.thatscalaguy.circe.jq.exceptions._
import io.circe.Json
import cats.data.NonEmptyList
import de.thatscalaguy.circe.jq._
import de.thatscalaguy.circe.jq.{Runtime => R}

trait JqSyntax {
  implicit def jqOps[F[_], A](wrapped: Json): JqOps =
    new JqOps(wrapped)
}

private final case class JqFunction(fn: Json => Json) {
  def apply(in: Json): Json = fn(in)
}

final class JqOps private[syntax] (wrapped: Json) {

  private def runFilter(terms: NonEmptyList[ListTerm], data: Json): Json =
    terms match {
      case NonEmptyList(head, tail) if tail.isEmpty => runTerm(head.terms, data)
      case NonEmptyList(head, tail) =>
        runFilter(NonEmptyList.fromListUnsafe(tail), runTerm(head.terms, data))
    }

  private def runTerm(terms: NonEmptyList[Term], data: Json): Json =
    terms match {
      case NonEmptyList(head, tail) if tail.isEmpty =>
        R.term(head)(data)
      case nel => R.termsToJsArray(nel, data)
    }

  private def runObject(obj: Object, data: Json): Json = {
    obj.nodes
      .map { pair =>
        val name = pair.name match {
          case f: Filter => runFilter(f.terms, data).asString.get
          case s: String => s
          case e => throw new Exception(s"unable to handle expression $e")
        }

        pair.value match {
          case Array(filters) =>
            Json.obj(name -> runFilter(filters.terms, data))
          case Filter(terms) => Json.obj(name -> runFilter(terms, data))
          case obj: Object   => Json.obj(name -> runObject(obj, data))
        }
      }
      .foldLeft(Json.obj())((a, b) => a.deepMerge(b))
  }

  private def run(exp: Output, data: Json): Json = {
    exp match {
      case Filter(terms)  => runFilter(terms, data)
      case Array(filters) => runFilter(filters.terms, data)
      case obj: Object    => runObject(obj, data)
    }
  }

  def jq(query: String): Json = {
    combinedParser.parseAll(query) match {
      case Left(value)  => throw new InvalidExpression(query, value)
      case Right(value) => run(value, wrapped)
    }
  }
}
