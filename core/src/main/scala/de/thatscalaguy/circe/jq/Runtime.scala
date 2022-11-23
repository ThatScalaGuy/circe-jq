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
  final case class RuntimeF(fn: Json => Json) {
    def apply(in: Json): Json = fn(in)
  }

  def term(term: Term): RuntimeF = term match {
    case IdentityTerm => identity
    case ListFieldTerm(fields) =>
      composeList(
        fields.map(f => Runtime.field(Json.fromString(f.name), f.optional))
      )

    case ListTerm(terms)    => Runtime.list(terms)
    case NumberTerm(value)  => Runtime.number(value)
    case BooleanTerm(value) => Runtime.boolean(value)
    case RecTerm            => Runtime.rec
    case SliceTerm(term, start, end, optional) =>
      Runtime.slice(term, start, end, optional)
    case IndexTerm(term, exp, optional) => Runtime.index(term, exp, optional)
    case NullTerm                       => Runtime.`null`
    case StringTerm(value)              => Runtime.string(value)
    case term                           => throw new InvalidTermType(term)
  }

  def termsToJsArray(terms: NonEmptyList[Term], data: Json): Json = {

    val headTerm: Json = Runtime.term(terms.head)(data)
    if (terms.tail.isEmpty) {
      Json.arr(headTerm)
    } else {
      Json.arr(
        headTerm,
        termsToJsArray(NonEmptyList.fromListUnsafe(terms.tail), data)
      )
    }
  }

  private def rec: RuntimeF = RuntimeF {
    case value if value.isArray =>
      rec(value.asArray.get.head)
    case value => value
  }

  private val identity = RuntimeF { data => data }

  private val `null` = RuntimeF { _ => Json.Null }

  private def string(value: String) = RuntimeF { _ => Json.fromString(value) }

  private def number(value: Double) = RuntimeF { _ =>
    Json.fromDoubleOrNull(value)
  }
  private def boolean(value: Boolean) = RuntimeF { _ =>
    Json.fromBoolean(value)
  }

  private def list(terms: NonEmptyList[Term]): RuntimeF = RuntimeF { data =>
    if (terms.tail.isEmpty) {
      Runtime.term(terms.head)(data)
    } else {
      Runtime.list(NonEmptyList.fromListUnsafe(terms.tail))(
        Runtime.term(terms.head)(data)
      )
    }
  }

  private def field(input: Json, optional: Boolean): RuntimeF = RuntimeF {
    data =>
      input match {
        case _ if input.isNumber =>
          val n = input.asNumber.get.toInt
            .getOrElse(throw new Exception("invalid numeric input"))
          data.asArray match {
            case Some(value) if n >= 0 && n < value.length => value(n.toInt)
            case Some(value) if n < 0 =>
              val reverseIndex = value.length + n.toInt
              if (reverseIndex >= 0) {
                value(reverseIndex)
              } else {
                Json.Null
              }
            case _ => optionalResult(optional, input, data)
          }

        case _ if input.isString =>
          (data \\ input.asString.get).headOption match {
            case Some(value) => value
            case None =>
              data.asArray match {
                case Some(value) =>
                  Json.arr(
                    value.map(node =>
                      (node \\ input.asString.get).headOption
                        .getOrElse(optionalResult(optional, input, data))
                    ): _*
                  )
                case None => optionalResult(optional, input, data)
              }

          }
        case _ if input.isNull => data
        case e                 => throw new Exception(s"field $e not supported")
      }
  }

  private def index(term: Term, index: Term, opt: Boolean) = RuntimeF { input =>
    val termFunction = Runtime.term(term)
    val indexFunction = Runtime.term(index)
    val trm = termFunction(input)
    val idx = indexFunction(trm)

    Runtime.field(idx, opt)(trm)
  }

  private def slice(
      trm: Term,
      startExp: Term,
      endExp: Term,
      opt: Boolean
  ) = RuntimeF { input =>
    val termFunction = Runtime.term(trm)
    val startFunction = Runtime.term(startExp)
    val endFunction = Runtime.term(endExp)
    val term: Json = termFunction(input)

    val startIdx = startFunction(term).asNumber
      .flatMap(_.toInt)
      .getOrElse(throw new Exception("Int expected"))
    val endIdx = endFunction(term).asNumber
      .flatMap(_.toInt)
      .getOrElse(throw new Exception("Int expected"))

    term match {
      case _ if term.isArray =>
        val indices = startIdx until endIdx
        val functions: Seq[RuntimeF] =
          indices.map(idx => Runtime.index(trm, NumberTerm(idx.toDouble), opt))
        Json.arr(functions.map(f => f(input)): _*)

      case _ if term.isString =>
        Json.fromString(term.asString.get.substring(startIdx, endIdx))
      case e =>
        throw new Exception(s"input $e not supported")
    }
  }

  private def optionalResult(
      optional: Boolean,
      field: Json,
      input: Json
  ): Json = {
    if (optional) {
      Json.Null
    } else {
      throw new IllegalArgumentException(
        s"input $input not supported, $field"
      )
    }
  }

  private def composeList(lst: NonEmptyList[RuntimeF]): RuntimeF =
    lst.foldLeft(identity) { (acc, next) =>
      RuntimeF { input =>
        val previousResult = acc(input)
        next(previousResult)
      }
    }
}
