package no.mehl.argonaut

import org.scalatest.FunSuite

class Json4SchemaTest extends FunSuite {

  import argonaut._
  import Argonaut._

  // http://json-schema.org/examples.html
  val spec = """{
               |    "title": "Person",
               |    "type": "object",
               |    "properties": {
               |        "firstName": {
               |            "type": "string"
               |        },
               |        "lastName": {
               |            "type": "string"
               |        },
               |        "age": {
               |            "description": "Age in years",
               |            "type": "integer",
               |            "minimum": 0
               |        }
               |    },
               |    "required": ["firstName", "lastName"]
               |}""".stripMargin

  test("Conform to spec") {
    case class MyJsonObject(firstName: String, lastName: String, age: Int)

    val model = Model[MyJsonObject]("Person",
      new SchemaEncoder[MyJsonObject](
        Field[MyJsonObject, String]("firstName", _.firstName, "string", true),
        Field[MyJsonObject, String]("lastName", _.lastName, "string", true),
        Field[MyJsonObject, Int]("age", _.age, "integer", false, Some("Age in years"), Some(0))
      ),
      c => for {
        firstName <- (c --\ "firstName").as[String]
        lastName  <- (c --\ "lastName").as[String]
        age       <- (c --\ "age").as[Int]
      } yield MyJsonObject(firstName, lastName, age)
    )

    implicit val codec = model.codec

    val jsoned = MyJsonObject("bas", "bar", 10).asJson

    println(jsoned)
    assert(model.jsonSchema == Parse.parse(spec).right.get)
    println(model.jsonSchema)

    assert (Parse.parse(jsoned.toString).right.get.toString == "{\"firstName\":\"bas\",\"lastName\":\"bar\",\"age\":10}")
  }

}
