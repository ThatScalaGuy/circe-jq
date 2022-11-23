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

import cats.data.NonEmptyList

sealed trait Term
case object NullTerm extends Term
case object RecTerm extends Term
case object IdentityTerm extends Term
case class ListTerm(terms: NonEmptyList[Term]) extends Term
case class FieldTerm(name: String, optional: Boolean) extends Term
case class ListFieldTerm(fields: NonEmptyList[FieldTerm]) extends Term
case class StringTerm(value: String) extends Term
case class NumberTerm(value: Double) extends Term
case class BooleanTerm(value: Boolean) extends Term
case class SliceTerm(
    term: Term,
    start: Term,
    end: Term,
    optional: Boolean
) extends Term
case class IndexTerm(term: Term, exp: Term, optional: Boolean) extends Term
