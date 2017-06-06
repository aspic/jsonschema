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
    import implicits._

    val personModel: Model[PersonObject] = Model(
      "Person",
      Some("Describes a Person"),
      Some(example),
      SchemaEncoder(
        Field("firstName", _.firstName),
        Field("lastName", _.lastName),
        Field("age", _.age, Some("Age in years"))
      ),
      c =>
        for {
          firstName <- (c --\ "firstName").as[String]
          lastName  <- (c --\ "lastName").as[String]
          age       <- (c --\ "age").as[Option[Int]]
        } yield PersonObject(firstName, lastName, age)
    )

    implicit val codec = personModel.codec

    val asJson = example.asJson

    println(personModel.jsonSchema)

    assert(
      personModel.jsonSchema.pretty(PrettyParams.spaces2) == Parse.parse(spec).right.get.pretty(PrettyParams.spaces2))
    assert(
      Parse.parse(asJson.toString).right.get.toString == "{\"firstName\":\"John\",\"lastName\":\"Doe\",\"age\":42}")
  }

  test("Composed object") {
    case class Person(name: String, surname: String, birthday: LocalDate, address: Address)
    case class Address(street: String, city: String, state: String, country: String)

    val exampleAddress = Address("3200 Mount Vernon Memorial Highway", "Mount Vernon", "Virginia", "United States")
    val examplePerson  = Person("John", "Doe", LocalDate.now(), exampleAddress)

    import implicits._

    implicit val localdateCodec: CodecJson[LocalDate] = CodecJson(
      d => jString(d.toString),
      c =>
        for {
          date <- c.as[String]
        } yield LocalDate.parse(date)
    )

    implicit val addressModel: Model[Address] = Model(
      "Address",
      Some("Describes an address"),
      Some(exampleAddress),
      SchemaEncoder(
        Field("street", _.street),
        Field("city", _.city),
        Field("state", _.state),
        Field("country", _.state)
      ),
      c =>
        for {
          street  <- (c --\ "street").as[String]
          city    <- (c --\ "city").as[String]
          state   <- (c --\ "state").as[String]
          country <- (c --\ "country").as[String]
        } yield Address(street, city, state, country)
    )

    implicit val addressCodec = addressModel.codec

    val personModel: Model[Person] = Model(
      "Person",
      Some("Describes a Person"),
      Some(examplePerson),
      SchemaEncoder(
        Field("first_name", _.name),
        Field("last_name", _.surname),
        Field("birthday", _.birthday),
        Field("address", _.address)
      ),
      c =>
        for {
          firstName <- (c --\ "firstName").as[String]
          lastName  <- (c --\ "lastName").as[String]
          birthday  <- (c --\ "age").as[LocalDate]
          address   <- (c --\ "address").as[Address]
        } yield Person(firstName, lastName, birthday, address)
    )

    implicit val personCodec = personModel.codec
    println(personModel.encoder.encode(Person("foo", "bar", LocalDate.now(), exampleAddress)).toString)

    println(personModel.jsonSchema)
    println(examplePerson.asJson)


  }

}
