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

  test("`jq` should thorw a InvalidExpression on invalid input") {
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
}
