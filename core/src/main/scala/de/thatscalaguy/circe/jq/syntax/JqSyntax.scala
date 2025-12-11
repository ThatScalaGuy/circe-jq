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
import de.thatscalaguy.circe.jq._
import de.thatscalaguy.circe.jq.{Runtime => R}
import io.circe.JsonObject

trait JqSyntax {
  implicit def jqOps[F[_], A](wrapped: Json): JqOps =
    new JqOps(wrapped)
}

private final case class JqFunction(fn: Json => Json) {
  def apply(in: Json): Json = fn(in)
}

final class JqOps private[syntax] (wrapped: Json) {

  private def runStage(stage: ListTerm, data: Json): Vector[Json] =
    // Comma semantics: evaluate each atom on the same input and concatenate.
    stage.terms.toList.toVector.flatMap(t => R.eval(t, data))

  private def runFilterAll(filter: Filter, data: Json): Vector[Json] =
    // Pipe semantics: feed each output of the left stage into the next.
    filter.terms.toList.foldLeft(Vector(data)) { (stream, stage) =>
      stream.flatMap(in => runStage(stage, in))
    }

  private def runObjectAll(obj: Object, data: Json): Vector[Json] = {
    val out: Vector[JsonObject] =
      obj.nodes.toList.foldLeft(Vector(JsonObject.empty)) { (objects, pair) =>
        val keys: Vector[String] = pair.name match {
          case s: String => Vector(s)
          case f: Filter =>
            runFilterAll(f, data).map {
              case j if j.isString => j.asString.get
              case other =>
                throw new IllegalArgumentException(
                  s"object key must be a string, got $other"
                )
            }
          case other =>
            throw new Exception(s"unable to handle expression $other")
        }

        val values: Vector[Json] = pair.value match {
          case f: Filter => runFilterAll(f, data)
          case Array(filters) =>
            Vector(Json.arr(runFilterAll(filters, data): _*))
          case o: Object => runObjectAll(o, data)
        }

        for {
          obj0 <- objects
          k <- keys
          v <- values
        } yield obj0.add(k, v)
      }

    out.map(Json.fromJsonObject)
  }

  private def runAll(exp: Output, data: Json): Vector[Json] = exp match {
    case f: Filter      => runFilterAll(f, data)
    case Array(filters) => Vector(Json.arr(runFilterAll(filters, data): _*))
    case o: Object      => runObjectAll(o, data)
  }

  /** Returns all jq outputs (jq is stream-oriented).
    */
  def jqAll(query: String): Vector[Json] =
    combinedParser.parseAll(query) match {
      case Left(value)  => throw new InvalidExpression(query, value)
      case Right(value) => runAll(value, wrapped)
    }

  def jq(query: String): Json = {
    val out = jqAll(query)
    out match {
      case Vector()    => Json.Null
      case Vector(one) => one
      case many        => Json.arr(many: _*)
    }
  }
}
