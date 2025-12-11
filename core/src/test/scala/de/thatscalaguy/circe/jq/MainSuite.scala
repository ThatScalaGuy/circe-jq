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
import de.thatscalaguy.circe.jq.syntax.all._
import de.thatscalaguy.circe.jq.exceptions.InvalidExpression

class MainSuite extends munit.FunSuite {

  test("`jq` should throw a InvalidExpression on invalid input") {
    intercept[InvalidExpression] {
      Json.Null.jq("invalid query")
    }
  }

  test("Identity") {
    val data = Json.obj("name" -> Json.fromString("Sven"))

    assertEquals(data.jq("."), data)
    assertEquals(data.jq(" ."), data)
    assertEquals(data.jq(". "), data)
    assertEquals(data.jq(" . "), data)

  }

  test("Object Identifier-Index") {
    val data = Json.obj(
      "name" -> Json.fromString("Sven"),
      "age" -> Json.fromInt(35)
    )
    val res = Json.fromString("Sven")

    assertEquals(data.jq(".name"), res)
    assertEquals(data.jq(" .name"), res)
    assertEquals(data.jq(".name "), res)
    assertEquals(data.jq(" .name "), res)

  }

  test("Object Identifier-Index Opt") {
    val data = Json.obj(
      "name" -> Json.fromString("Sven"),
      "age" -> Json.fromInt(35)
    )
    val res = Json.fromString("Sven")

    assertEquals(data.jq(".name?"), res)
    assertEquals(data.jq(".[name]?"), res)
    assertEquals(data.jq(".[name2]?"), Json.Null)
  }

  test("piped Object Identifier-Index") {
    val data = Json.obj(
      "name" -> Json.fromString("Sven"),
      "attr" -> Json.obj("age" -> Json.fromInt(35))
    )
    val res = Json.fromInt(35)

    assertEquals(data.jq(""".["attr"] | .age"""), res)
    assertEquals(data.jq(".attr|.age"), res)
    assertEquals(data.jq(" .attr|.age"), res)
    assertEquals(data.jq(" .attr|.age "), res)
    assertEquals(data.jq(".attr| .age"), res)
  }

  test("substring") {
    val data = Json.obj(
      "full_name" -> Json.fromString("Sven Herrmann")
    )
    val res = Json.fromString("Sven")

    assertEquals(data.jq(".full_name[0:4]"), res)
    assertEquals(data.jq(".full_name | .[0:4]"), res)
  }

  test("array index") {
    val data = Json.arr(
      Json.fromString("Sven"),
      Json.fromString("Max"),
      Json.fromString("Phineas")
    )
    assertEquals(data.jq(".[0]"), Json.fromString("Sven"))
    assertEquals(data.jq(".[1]"), Json.fromString("Max"))
    assertEquals(data.jq(".[2]"), Json.fromString("Phineas"))
  }

  test("create simple objects") {
    val data = Json.obj(
      "name" -> Json.fromString("Sven"),
      "attr" -> Json.obj("age" -> Json.fromInt(35))
    )

    val res = Json.obj("test" -> Json.fromString("Sven"))

    assertEquals(data.jq("{test:.name}"), res)
    assertEquals(data.jq("{ test:.name }"), res)
    assertEquals(data.jq("{ test : .name }"), res)
    assertEquals(data.jq("{ test:.name}"), res)
    assertEquals(data.jq("{ test: .name}"), res)
    assertEquals(data.jq("{test:.name }"), res)
  }

  test("Array/Object value iterator .[] and jqAll") {
    val arr = Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
    assertEquals(
      arr.jq(".[]"),
      Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
    )
    assertEquals(
      arr.jqAll(".[]"),
      Vector(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
    )

    val obj = Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))
    assertEquals(obj.jq(".[]"), Json.arr(Json.fromInt(1), Json.fromInt(2)))
  }

  test("Comma concatenates outputs") {
    val data = Json.obj(
      "name" -> Json.fromString("Sven"),
      "age" -> Json.fromInt(35)
    )

    assertEquals(
      data.jq(".name, .age"),
      Json.arr(Json.fromString("Sven"), Json.fromInt(35))
    )
  }

  test("Pipe maps over iterator outputs") {
    val data = Json.arr(
      Json.obj("name" -> Json.fromString("Sven")),
      Json.obj("name" -> Json.fromString("Max"))
    )

    assertEquals(
      data.jq(".[] | .name"),
      Json.arr(Json.fromString("Sven"), Json.fromString("Max"))
    )
  }

  test("Open-ended and negative slices") {
    val data = Json.obj(
      "full_name" -> Json.fromString("Sven Herrmann")
    )

    assertEquals(data.jq(".full_name[:4]"), Json.fromString("Sven"))
    assertEquals(data.jq(".full_name[5:]"), Json.fromString("Herrmann"))
    assertEquals(data.jq(".full_name[-8:]"), Json.fromString("Herrmann"))
  }

  test("Recursive descent ..") {
    val data = Json.obj(
      "a" -> Json.obj("a" -> Json.fromInt(1)),
      "b" -> Json.arr(Json.obj("a" -> Json.fromInt(2)))
    )

    val out = data.jqAll("..")
    assert(out.contains(Json.fromInt(1)))
    assert(out.contains(Json.fromInt(2)))
    assert(out.contains(Json.obj("a" -> Json.fromInt(1))))
  }

  // ==================== jq-conform optional (?) semantics ====================
  // In jq, `EXP?` is shorthand for `try EXP`, which suppresses errors by yielding empty output.
  // Wrong type + optional -> empty stream (not null).
  // Missing key on correct type -> null (not empty).

  test("Optional .foo? on wrong type yields empty stream (jq-conform)") {
    // jq: `1 | .foo?` outputs nothing
    assertEquals(Json.fromInt(1).jqAll(".name?"), Vector.empty)
    // jq: `.foo?` on null yields null (not error)
    assertEquals(Json.Null.jq(".name?"), Json.Null)
    // jq: `.foo?` on object with missing key yields null
    assertEquals(Json.obj("a" -> Json.fromInt(1)).jq(".b?"), Json.Null)
  }

  test("Optional .[i]? on wrong type yields empty stream (jq-conform)") {
    // jq: `1 | .[0]?` outputs nothing
    assertEquals(Json.fromInt(1).jqAll(".[0]?"), Vector.empty)
    // jq: `\"abc\" | .[0]?` outputs nothing (strings don't support integer index in jq)
    assertEquals(Json.fromString("abc").jqAll(".[0]?"), Vector.empty)
  }

  test(
    "Optional .[start:end]? on wrong type yields empty stream (jq-conform)"
  ) {
    // jq: `1 | .[0:1]?` outputs nothing
    assertEquals(Json.fromInt(1).jqAll(".[0:1]?"), Vector.empty)
    assertEquals(Json.fromBoolean(true).jqAll(".[0:1]?"), Vector.empty)
  }

  test("Optional .[]? on wrong type yields empty stream (jq-conform)") {
    // jq: `1 | .[]?` outputs nothing
    assertEquals(Json.fromInt(1).jqAll(".[]?"), Vector.empty)
    assertEquals(Json.fromString("abc").jqAll(".[]?"), Vector.empty)
    assertEquals(Json.Null.jqAll(".[]?"), Vector.empty)
  }

  // ==================== Negative array indices ====================

  test("Negative array index") {
    val arr = Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
    // jq: `[-1]` is the last element
    assertEquals(arr.jq(".[-1]"), Json.fromInt(3))
    assertEquals(arr.jq(".[-2]"), Json.fromInt(2))
    assertEquals(arr.jq(".[-3]"), Json.fromInt(1))
    // Out of bounds yields null
    assertEquals(arr.jq(".[-4]"), Json.Null)
  }

  // ==================== Array slicing ====================

  test("Array slicing with open bounds") {
    val arr = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4)
    )
    // jq: `.[2:]` from index 2 to end
    assertEquals(
      arr.jq(".[2:]"),
      Json.arr(Json.fromInt(2), Json.fromInt(3), Json.fromInt(4))
    )
    // jq: `.[:3]` from start to index 3 (exclusive)
    assertEquals(
      arr.jq(".[:3]"),
      Json.arr(Json.fromInt(0), Json.fromInt(1), Json.fromInt(2))
    )
    // jq: `.[1:4]` from 1 to 4
    assertEquals(
      arr.jq(".[1:4]"),
      Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))
    )
    // jq: `.[:]` entire array
    assertEquals(arr.jq(".[:]"), arr)
  }

  test("Array slicing with negative indices") {
    val arr = Json.arr(
      Json.fromInt(0),
      Json.fromInt(1),
      Json.fromInt(2),
      Json.fromInt(3),
      Json.fromInt(4)
    )
    // jq: `.[-2:]` last 2 elements
    assertEquals(arr.jq(".[-2:]"), Json.arr(Json.fromInt(3), Json.fromInt(4)))
    // jq: `.[:-1]` all but last
    assertEquals(
      arr.jq(".[:-1]"),
      Json.arr(
        Json.fromInt(0),
        Json.fromInt(1),
        Json.fromInt(2),
        Json.fromInt(3)
      )
    )
  }

  // ==================== Object construction ====================

  test("Object construction with shorthand {foo}") {
    val data = Json.obj("foo" -> Json.fromInt(1), "bar" -> Json.fromInt(2))
    assertEquals(data.jq("{foo}"), Json.obj("foo" -> Json.fromInt(1)))
  }

  test("Object construction with multiple keys") {
    val data = Json.obj(
      "a" -> Json.fromInt(1),
      "b" -> Json.fromInt(2),
      "c" -> Json.fromInt(3)
    )
    assertEquals(
      data.jq("{x:.a, y:.b}"),
      Json.obj("x" -> Json.fromInt(1), "y" -> Json.fromInt(2))
    )
  }

  // ==================== Complex pipes and iteration ====================

  test("Nested array access with pipe") {
    val data = Json.obj(
      "items" -> Json.arr(
        Json.obj("id" -> Json.fromInt(1)),
        Json.obj("id" -> Json.fromInt(2))
      )
    )
    assertEquals(
      data.jq(".items | .[] | .id"),
      Json.arr(Json.fromInt(1), Json.fromInt(2))
    )
  }

  test("Array construction with iterator") {
    val data = Json.obj("a" -> Json.fromInt(1), "b" -> Json.fromInt(2))
    // jq: `[.a, .b]` collects values into array
    assertEquals(
      data.jq("[.a, .b]"),
      Json.arr(Json.fromInt(1), Json.fromInt(2))
    )
  }

  test("Null propagation through field access") {
    // jq: `.foo.bar` on object without .foo yields null (not error)
    val data = Json.obj("x" -> Json.fromInt(1))
    assertEquals(data.jq(".foo"), Json.Null)
    assertEquals(data.jq(".foo.bar"), Json.Null)
  }

  // ==================== Errors on non-optional wrong type ====================

  test("Non-optional .foo on wrong type throws") {
    intercept[IllegalArgumentException] {
      Json.fromInt(1).jq(".foo")
    }
  }

  test("Non-optional .[i] on wrong type throws") {
    intercept[IllegalArgumentException] {
      Json.fromString("abc").jq(".[0]")
    }
  }

  test("Non-optional .[] on wrong type throws") {
    intercept[IllegalArgumentException] {
      Json.fromInt(1).jq(".[]")
    }
  }
}
