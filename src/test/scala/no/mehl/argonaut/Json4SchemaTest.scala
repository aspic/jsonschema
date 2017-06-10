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

    import schemaImplicits._

    implicit val intSchemaDef = new IntSchemaDef with MinimumDef {
      override val props        = List() ++ minProps
      override val minimum: Int = 0
    }

    val firstName = Field[String]("firstName")
    val lastName  = Field[String]("lastName")
    val age       = Field[Option[Int]]("age", Some("Age in years"))

    val personModel: Model[PersonObject] = Model(
      "Person",
      Some("Describes a Person"),
      Some(example),
      o =>
        Json.obj(
          firstName(o.firstName),
          lastName(o.lastName),
          age(o.age)
      ), {
        SchemaDecoder(
          c =>
            for {
              first <- firstName(c)
              last  <- lastName(c)
              age   <- age(c)
            } yield PersonObject(first, last, age),
          firstName,
          lastName,
          age
        )
      }
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

    implicit val addressModel: Model[Address] = Model(
      "Address",
      Some("Describes an address"),
      Some(exampleAddress),
      o =>
        Json.obj(
          "street" := o.street,
          "city" := o.city,
          "state" := o.state,
          "country" := o.country
      ), {
        val street  = Field[String]("street")
        val city    = Field[String]("city")
        val state   = Field[String]("state")
        val country = Field[String]("country")
        SchemaDecoder(c =>
                        for {
                          street  <- street(c)
                          city    <- city(c)
                          state   <- state(c)
                          country <- country(c)
                        } yield Address(street, city, state, country),
                      street,
                      city,
                      state,
                      country)
      }
    )

    implicit val addressCodec       = addressModel.codec
    implicit val localDateSchemaDef = new StringSchemaDef[LocalDate] {}

    implicit val personModel: Model[Person] = Model(
      "Person",
      Some("Describes a Person"),
      Some(examplePerson),
      o =>
        Json.obj(
          "first_name" := o.name,
          "last_name" := o.surname,
          "birthday" := o.birthday,
          "address" := o.address
      ), {
        val firstName = Field[String]("first_name")
        val lastName  = Field[String]("last_name")
        val birthDay  = Field[LocalDate]("birthday")
        val address   = Field[Address]("address")
        SchemaDecoder(
          c =>
            for {
              firstName <- firstName(c)
              lastName  <- lastName(c)
              birthday  <- birthDay(c)
              address   <- address(c)
            } yield Person(firstName, lastName, birthday, address),
          firstName,
          lastName,
          birthDay,
          address
        )
      }
    )

    implicit val personCodec = personModel.codec
    println(personModel.encoder(Person("foo", "bar", LocalDate.now(), exampleAddress)).toString)

    println(personModel.jsonSchema)
    println(examplePerson.asJson)
    val json = examplePerson.asJson
    println(json)

    println(personModel.codec.decodeJson(json).toOption.get)
    println(personModel.jsonSchema)

    case class Foo(p: Person)

    val foo = new Model[Foo]("Foo", None, None, e => Json.obj("person" := e.p), {
      val f = Field[Person]("person")
      SchemaDecoder(c =>
                      for {
                        p <- f(c)
                      } yield Foo(p),
                    f)
    })

    implicit val fooCodec = foo.codec

    println(foo.jsonSchema)
    println(Foo(examplePerson).asJson)

  }

  test("oneOf") {
    import schemaImplicits._

    case class Person(name: String, gender: String)

    val personModel = Model[Person](
      "Person",
      Some("Some person"),
      None,
      p => casecodec2(Person.apply, Person.unapply)("name", "gender").Encoder(p), {
        val name   = Field[String]("name")
        val gender = Field[String]("gender", None, Set("male", "female"))
        SchemaDecoder(c =>
                        for {
                          name   <- name(c)
                          gender <- gender(c)
                        } yield Person(name, gender),
                      name,
                      gender)
      }
    )

    assert(personModel.jsonSchema.toString === "{\"description\":\"Some person\",\"properties\":{\"name\":{\"type\":\"string\"},\"gender\":{\"type\":\"string\",\"enum\":[\"male\",\"female\"]}},\"title\":\"Person\",\"type\":\"object\",\"required\":[\"name\",\"gender\"],\"$schema\":\"http://json-schema.org/draft-04/schema#\"}")
  }

}
