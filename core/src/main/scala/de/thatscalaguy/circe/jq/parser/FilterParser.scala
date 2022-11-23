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

  private val sliceOrIndex
      : Parser[((Either[Term, (Term, Term)]), Option[Unit])] = {
    val index: Parser[Either[Term, (Term, Term)]] =
      (numberTerm | stringTerm).between(lbracket, rbracket).map(i => Left(i))

    val slice: Parser[Either[Term, (Term, Term)]] =
      (numberTerm ~ colon ~ numberTerm)
        .map(lr => (lr._1._1, lr._2))
        .between(lbracket, rbracket)
        .map(i => Right(i))

    (index.backtrack | slice) ~ optional
  }

  private val sliceOrIndexModel = sliceOrIndex.map {
    case (Left(t), opt) => SliceOrIndexModel(t, None, opt.isDefined)
    case (Right((t1, t2)), opt) =>
      SliceOrIndexModel(t1, Some(t2), opt.isDefined)
    case _ => SliceOrIndexModel(NullTerm, None, false)
  }.?

  private val sliceOrIndexTerm =
    (dot ~ sliceOrIndexModel).map(t => wrapTerm(IdentityTerm, t._2))

  private val fieldWithSlice: cats.parse.Parser[cats.data.NonEmptyList[
    (
        cats.data.NonEmptyList[(String, Option[Unit])],
        Option[de.thatscalaguy.circe.jq.SliceOrIndexModel]
    )
  ]] = ((field ~ optional <* space.?).rep ~ sliceOrIndexModel).rep

  private val fieldTerm = fieldWithSlice.map { nelFiled =>
    ListTerm(nelFiled.map { t =>
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
    case Some(SliceOrIndexModel(start, Some(end), optional)) =>
      SliceTerm(term, start, end, optional)
    case Some(SliceOrIndexModel(start, None, optional)) =>
      IndexTerm(term, start, optional)
  }

  private val seqTermParser: Parser[NonEmptyList[NonEmptyList[Term]]] = {
    (recursiveTerm.backtrack | fieldTerm.backtrack | sliceOrIndexTerm)
      .surroundedBy(space.?)
      .repSep(comma)
      .repSep(pipe)
  }
  val parser: Parser[Filter] =
    seqTermParser.map(t => Filter(t.map(ListTerm.apply)))

  val expression: Parser[Filter] =
    parser.surroundedBy(space.?).between(lrbracket, rrbracket)
}
