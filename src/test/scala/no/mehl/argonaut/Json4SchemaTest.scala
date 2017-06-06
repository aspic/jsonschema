package no.mehl.argonaut

import argonaut.Argonaut
import argonaut.Json.JsonAssoc
import org.scalatest.FunSuite

class Json4SchemaTest extends FunSuite {

  import argonaut._
  import Argonaut._

  // http://json-schema.org/examples.html
  val spec = """{
               |    "$schema":"http://json-schema.org/draft-04/schema#",
               |    "title": "Person",
               |    "description": "Describes a Person",
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

  implicit class JsonAssocOps(a: JsonAssoc) {

  }

  case class Schema(j: Json, a: JsonAssoc, title: String, description: Option[String] = None, minimum: Option[Int] = None) {
    def schema(): (JsonField, Json) = {
      title := jEmptyObject
        .->?:(description.map("description" := _))
        .->:("type" := a._2.name)
        .->?:(minimum.map("minimum" := _))
    }
  }

  test("Conform to spec") {
    case class PersonObject(firstName: String, lastName: String, age: Int, tags: List[String])

    val model: Model[PersonObject] = Model("Person", Some("Describes a Person"),
      SchemaEncoder(
        Field[PersonObject, String]("firstName", _.firstName, "string", true),
        Field[PersonObject, String]("lastName", _.lastName, "string", true),
        Field[PersonObject, Int]("age", _.age, "integer", false, Some("Age in years"), Some(0))
      ),
      c => for {
        firstName <- (c --\ "firstName").as[String]
        lastName  <- (c --\ "lastName").as[String]
        age       <- (c --\ "age").as[Int]
      } yield PersonObject(firstName, lastName, age, List.empty)
    )

    implicit val codec = model.codec

    val jsoned = PersonObject("bas", "bar", 10, List("foo", "bar")).asJson

    assert(model.jsonSchema.pretty(PrettyParams.spaces2) == Parse.parse(spec).right.get.pretty(PrettyParams.spaces2))
    assert (Parse.parse(jsoned.toString).right.get.toString == "{\"firstName\":\"bas\",\"lastName\":\"bar\",\"age\":10}")
  }

}
