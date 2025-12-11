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

import cats.parse.Parser
import de.thatscalaguy.circe.jq._
import cats.data.NonEmptyList

object FilterParser {
  private val recursiveTerm: Parser[Term] = rec.map(_ => RecTerm)
  private val stringTerm: Parser[Term] = string.map(StringTerm.apply)
  private val numberTerm: Parser[Term] = num.map(NumberTerm.apply)

  // Helper: optional number term (can be absent)
  private val optNumTerm: cats.parse.Parser0[Option[Term]] =
    numberTerm.?.surroundedBy(space.?)

  // `.[]` / `.[]?`
  private val iterator: Parser[Model] =
    (lbracket ~ rbracket).as(IteratorModel(optional = false))

  // `.[start:end]` with open-ended bounds - must try BEFORE index
  // Handles: `.[:]`, `.[5:]`, `.[:5]`, `.[2:5]`
  private val slice: Parser[Model] =
    (lbracket *> (optNumTerm <* colon) ~ optNumTerm <* rbracket)
      .map { case (startOpt, endOpt) =>
        SliceModel(startOpt, endOpt, optional = false)
      }

  // `.[<number>]` / `.[<string>]` - try AFTER slice
  private val index: Parser[Model] =
    (numberTerm | stringTerm)
      .surroundedBy(space.?)
      .between(lbracket, rbracket)
      .map(i => IndexModel(i, optional = false))

  // Order matters: try slice first (which has colon), then index, then iterator
  private val sliceOrIndex: Parser[(Model, Option[Unit])] =
    (slice.backtrack | index.backtrack | iterator) ~ optional

  private val sliceOrIndexModel: cats.parse.Parser0[Option[Model]] =
    sliceOrIndex.map {
      case (IteratorModel(_), opt) => IteratorModel(opt.isDefined)
      case (SliceModel(start, end, _), opt) =>
        SliceModel(start, end, opt.isDefined)
      case (IndexModel(idx, _), opt) =>
        IndexModel(idx, opt.isDefined)
      case (m, _) => m
    }.?

  private val sliceOrIndexTerm: Parser[Term] =
    (dot ~ sliceOrIndexModel).map { case (_, model) =>
      wrapTerm(IdentityTerm, model)
    }

  private val fieldWithSlice: Parser[NonEmptyList[
    (NonEmptyList[(String, Option[Unit])], Option[Model])
  ]] = ((field ~ optional <* space.?).rep ~ sliceOrIndexModel).rep

  private val fieldTerm: Parser[Term] = fieldWithSlice.map { nelField =>
    ListTerm(nelField.map { t =>
      wrapTerm(
        ListFieldTerm(t._1.map { case (name, opt) =>
          FieldTerm(name, opt.isDefined)
        }),
        t._2
      )
    })
  }

  private def wrapTerm(term: Term, model: Option[Model]): Term = model match {
    case None => term
    case Some(IteratorModel(optional)) =>
      IteratorTerm(term, optional)
    case Some(SliceModel(start, end, optional)) =>
      SliceTerm(term, start, end, optional)
    case Some(IndexModel(idx, optional)) =>
      IndexTerm(term, idx, optional)
  }

  private val singleTerm: Parser[Term] =
    (recursiveTerm.backtrack | fieldTerm.backtrack | sliceOrIndexTerm)
      .surroundedBy(space.?)

  // Single piped expression (no comma at top level) - used as value in object pairs
  private val singlePipedTerms: Parser[NonEmptyList[Term]] =
    singleTerm.repSep(pipe)

  // For object values: a single filter expression without comma separation
  val singleExpr: Parser[Filter] =
    singlePipedTerms.map(terms => Filter(NonEmptyList.one(ListTerm(terms))))

  private val seqTermParser: Parser[NonEmptyList[NonEmptyList[Term]]] = {
    singleTerm
      .repSep(comma)
      .repSep(pipe)
  }
  val parser: Parser[Filter] =
    seqTermParser.map(t => Filter(t.map(ListTerm.apply)))

  val expression: Parser[Filter] =
    parser.surroundedBy(space.?).between(lrbracket, rrbracket)
}
