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

  /**
  test("Conform to spec, and decode instance") {
    case class PersonObject(firstName: String, lastName: String, age: Option[Int])

    val example = PersonObject("John", "Doe", Some(42))

    import schemaImplicits._

    implicit val intSchemaDef = new IntSchemaDef with MinimumDef {
      override val props = List() ++ minProps
      override val minimum: Int = 0
    }

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
    println(example.asJson)

    assert(
      personModel.jsonSchema.pretty(PrettyParams.spaces2) == Parse.parse(spec).right.get.pretty(PrettyParams.spaces2))
    assert(
      Parse.parse(asJson.toString).right.get.toString == "{\"firstName\":\"John\",\"lastName\":\"Doe\",\"age\":42}")
  }
    */
  test("Composed object") {
    case class Person(name: String, surname: String, birthday: LocalDate, address: Address)
    case class Address(street: String, city: String, state: String, country: String)

    val exampleAddress = Address("3200 Mount Vernon Memorial Highway", "Mount Vernon", "Virginia", "United States")
    val examplePerson  = Person("John", "Doe", LocalDate.now(), exampleAddress)

    import schemaImplicits._

    implicit val localdateCodec: CodecJson[LocalDate] = CodecJson(
      d => jString(d.toString),
      c =>
        for {
          date <- c.as[String]
        } yield LocalDate.parse(date)
    )

    /**
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
      */
    implicit val localDateSchemaDef = new StringSchemaDef[LocalDate] {}
    implicit val foo                = new StringSchemaDef[Address]   {}
    implicit val codec: CodecJson[Address] = CodecJson(
      e => jEmptyObject,
      c => DecodeResult.ok(Address("fpp", "bar", "bas", "barbar"))
    )

    val personModel: Model[Person] = Model(
      "Person",
      Some("Describes a Person"),
      Some(examplePerson),
      SchemaEncoder(
        Field("first_name", _.name),
        Field("last_name", _.surname),
        Field("birthday", _.birthday),
        Field("address", _.address)
      ), {
        val firstName = FieldTo[Option[String]]("first_name")
        val lastName  = FieldTo[String]("last_name")
        val birthDay  = FieldTo[LocalDate]("birthday")
        val address   = FieldTo[Address]("address")
        SchemaDecoder(
          List(firstName, lastName, birthDay, address),
          c =>
            for {
              firstName <- firstName(c)
              lastName  <- lastName(c)
              birthday  <- birthDay(c)
              address   <- address(c)
            } yield Person(firstName.get, lastName, birthday, address)
        )
      }
    )

    implicit val personCodec = personModel.codec
    println(personModel.encoder.encode(Person("foo", "bar", LocalDate.now(), exampleAddress)).toString)

    println(personModel.jsonSchema)
    println(examplePerson.asJson)
    val json = examplePerson.asJson
    println(json)

    println(personModel.codec.decodeJson(json).toOption.get)
    println(personModel.jsonSchema)

  }

}
