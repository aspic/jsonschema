package no.mehl.argonaut

import java.time.LocalDate

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



  test("Conform to spec, and decode instance") {
    case class PersonObject(firstName: String, lastName: String, age: Option[Int])

    val example = PersonObject("John", "Doe", Some(42))
    val personModel: Model[PersonObject] = Model("Person", Some("Describes a Person"), example,
      o => {
        SchemaEncoder(
          Field("firstName", o.firstName),
          Field("lastName", o.lastName),
          Field("age", o.age, Some("Age in years"), Some(0))
        )
      },
      c => for {
        firstName <- (c --\ "firstName").as[String]
        lastName  <- (c --\ "lastName").as[String]
        age       <- (c --\ "age").as[Option[Int]]
      } yield PersonObject(firstName, lastName, age)
    )

    implicit val codec = personModel.codec

    val asJson = example.asJson

    assert(personModel.jsonSchema.pretty(PrettyParams.spaces2) == Parse.parse(spec).right.get.pretty(PrettyParams.spaces2))
    assert (Parse.parse(asJson.toString).right.get.toString == "{\"firstName\":\"John\",\"lastName\":\"Doe\",\"age\":42}")
  }

  test("Composed object") {
    case class Person(name: String, surname: String, birthday: LocalDate, address: Address)
    case class Address(street: String, city: String, state: String, country: String)

    val exampleAddress = Address("3200 Mount Vernon Memorial Highway", "Mount Vernon", "Virginia", "United States")
    val examplePerson = Person("John", "Doe", LocalDate.now(), exampleAddress)

    implicit val localdateCodec: CodecJson[LocalDate] = CodecJson(
      d => jString(d.toString),
      c => for {
        date <- c.as[String]
      } yield LocalDate.parse(date)
    )

    val addressModel: Model[Address] = Model("Address", Some("Describes an address"), exampleAddress,
      o => {
        SchemaEncoder(
          Field("street", o.street),
          Field("city", o.city),
          Field("state", o.state),
          Field("country", o.state)
        )
      },
      c => for {
        street <- (c --\ "street").as[String]
        city <- (c --\ "city").as[String]
        state <- (c --\ "state").as[String]
        country <- (c --\ "country").as[String]
    } yield Address(street, city, state, country))

    implicit val addressCodec = addressModel.codec

    val personModel: Model[Person] = Model("Person", Some("Describes a Person"), examplePerson,
      o => {
        SchemaEncoder(
          Field("first_name", o.name),
          Field("last_name", o.surname),
          Field("birthday", o.birthday),
          Field("address", o.address)
        )
      },
      c => for {
        firstName <- (c --\ "firstName").as[String]
        lastName  <- (c --\ "lastName").as[String]
        birthday       <- (c --\ "age").as[LocalDate]
        address       <- (c --\ "address").as[Address]
      } yield Person(firstName, lastName, birthday, address)
    )

    println(personModel.jsonSchema.toString)

  }

}
