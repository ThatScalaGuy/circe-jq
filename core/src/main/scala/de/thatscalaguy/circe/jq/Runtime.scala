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

import io.circe.Json
import cats.data.NonEmptyList
import de.thatscalaguy.circe.jq.exceptions._

object Runtime {

  /** Evaluate a term as a jq value stream. jq is stream-oriented: each
    * expression can output 0..N values.
    */
  def eval(term: Term, input: Json): Vector[Json] = term match {
    case IdentityTerm   => Vector(input)
    case NullTerm       => Vector(Json.Null)
    case StringTerm(v)  => Vector(Json.fromString(v))
    case NumberTerm(v)  => Vector(Json.fromDoubleOrNull(v))
    case BooleanTerm(v) => Vector(Json.fromBoolean(v))

    // Recursive descent: outputs every value, including the input.
    case RecTerm => recursiveDescent(input)

    // Sequential chaining, e.g. `.foo[0].bar`.
    // Comma semantics are handled at the filter stage level (see JqSyntax).
    case ListTerm(terms) =>
      terms.toList.foldLeft(Vector(input)) { (stream, next) =>
        stream.flatMap(v => eval(next, v))
      }

    case ListFieldTerm(fields) =>
      // Sequential application of identifier indices, e.g. `.foo.bar`
      fields.toList.foldLeft(Vector(input)) { (stream, field) =>
        stream.flatMap(v => evalField(field.name, field.optional, v))
      }

    case IndexTerm(target, indexExp, optional) =>
      eval(target, input).flatMap { v =>
        // In jq, the index expression is evaluated with `.` being the indexed value.
        // This repo historically evaluated it against the current value.
        val indexValues = eval(indexExp, v)
        if (indexValues.isEmpty) Vector(Json.Null)
        else indexValues.flatMap(idx => evalIndex(idx, optional, v))
      }

    case IteratorTerm(target, optional) =>
      eval(target, input).flatMap(v => evalIterator(v, optional))

    case SliceTerm(target, startExp, endExp, optional) =>
      eval(target, input).flatMap { v =>
        val startIdxOpt = startExp.flatMap(t => evalAsInt(t, v))
        val endIdxOpt = endExp.flatMap(t => evalAsInt(t, v))
        evalSlice(v, startIdxOpt, endIdxOpt, optional)
      }

    case other => throw new InvalidTermType(other)
  }

  /** Backwards-compatible single-value evaluation used by legacy call sites. If
    * multiple outputs exist, they are collected into an array.
    */
  def term(term: Term): Json => Json = { input =>
    val out = eval(term, input)
    out match {
      case Vector()    => Json.Null
      case Vector(one) => one
      case many        => Json.arr(many: _*)
    }
  }

  def termsToJsArray(terms: NonEmptyList[Term], data: Json): Json = {
    Json.arr(terms.toList.map(t => term(t)(data)): _*)
  }

  private def evalAsInt(term: Term, input: Json): Option[Int] =
    eval(term, input).headOption.flatMap(_.asNumber.flatMap(_.toInt))

  private def evalField(
      name: String,
      optional: Boolean,
      input: Json
  ): Vector[Json] = {
    input.asObject match {
      case Some(obj) => Vector(obj(name).getOrElse(Json.Null))
      case None      =>
        // jq: .foo on null yields null; .foo? on wrong type yields empty
        if (input.isNull) Vector(Json.Null)
        else if (optional) Vector.empty // jq-conform: try semantics -> empty
        else
          throw new IllegalArgumentException(
            s"input $input not supported, .$name"
          )
    }
  }

  private def evalIndex(
      index: Json,
      optional: Boolean,
      input: Json
  ): Vector[Json] = {
    index.asNumber.flatMap(_.toInt) match {
      case Some(i) =>
        input.asArray match {
          case Some(arr) =>
            val idx = if (i < 0) arr.length + i else i
            if (idx >= 0 && idx < arr.length) Vector(arr(idx))
            else Vector(Json.Null)
          case None =>
            // jq-conform: .[i]? on wrong type -> empty
            if (optional) Vector.empty
            else
              throw new IllegalArgumentException(
                s"input $input not supported, .[$i]"
              )
        }

      case None =>
        index.asString match {
          case Some(key) =>
            input.asObject match {
              case Some(obj) => Vector(obj(key).getOrElse(Json.Null))
              case None      =>
                // jq-conform: .["key"]? on wrong type -> empty
                if (optional) Vector.empty
                else
                  throw new IllegalArgumentException(
                    s"input $input not supported, .[\"$key\"]"
                  )
            }
          case None =>
            if (optional) Vector.empty
            else
              throw new IllegalArgumentException(s"index $index not supported")
        }
    }
  }

  private def evalIterator(input: Json, optional: Boolean): Vector[Json] = {
    input.asArray match {
      case Some(arr) => arr.toVector
      case None =>
        input.asObject match {
          case Some(obj) => obj.values.toVector
          case None =>
            if (optional) Vector.empty
            else
              throw new IllegalArgumentException(
                s"input $input not supported, .[]"
              )
        }
    }
  }

  private def clampSliceIndex(i: Int, length: Int): Int = {
    val resolved = if (i < 0) length + i else i
    math.max(0, math.min(length, resolved))
  }

  private def evalSlice(
      input: Json,
      startIdx: Option[Int],
      endIdx: Option[Int],
      optional: Boolean
  ): Vector[Json] = {
    input.asArray match {
      case Some(arr) =>
        val start = clampSliceIndex(startIdx.getOrElse(0), arr.length)
        val end = clampSliceIndex(endIdx.getOrElse(arr.length), arr.length)
        Vector(Json.arr(arr.slice(start, end): _*))

      case None =>
        input.asString match {
          case Some(str) =>
            val start = clampSliceIndex(startIdx.getOrElse(0), str.length)
            val end = clampSliceIndex(endIdx.getOrElse(str.length), str.length)
            Vector(Json.fromString(str.substring(start, end)))
          case None =>
            // jq-conform: slice? on wrong type -> empty
            if (optional) Vector.empty
            else
              throw new IllegalArgumentException(
                s"input $input not supported, slice"
              )
        }
    }
  }

  private def recursiveDescent(input: Json): Vector[Json] = {
    val children: Vector[Json] =
      input.asArray
        .map(_.toVector)
        .orElse(input.asObject.map(_.values.toVector))
        .getOrElse(Vector.empty)

    Vector(input) ++ children.flatMap(recursiveDescent)
  }
}
