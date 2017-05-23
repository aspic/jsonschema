package no.mehl.argonaut

import org.scalatest.FunSuite

class JsonEnrich extends FunSuite {

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
      c => for {
        firstName <- (c --\ "firstName").as[String]
        lastName <- (c --\ "lastName").as[String]
        age <- (c --\ "age").as[Int]
      } yield MyJsonObject(firstName, lastName, age),
      EncodedField("firstName", (s, o) => s := o.firstName, "string", true),
      EncodedField("lastName", (s, o) => s := o.lastName, "string", true),
      EncodedField("age", (s, o) => s := o.age, "integer", false, Some("Age in years"), Some(0))
    )

    implicit val codec = model.asCodec

    val jsoned = MyJsonObject("bas", "bar", 10).asJson

    println(jsoned)
    assert(model.jsonSchema == Parse.parse(spec).right.get)
    println(model.jsonSchema)

    val o = Parse.parse(jsoned.toString).toOption
    println(o)

  }

}
