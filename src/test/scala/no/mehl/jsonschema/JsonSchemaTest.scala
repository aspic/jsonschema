package no.mehl.jsonschema

import java.time.LocalDate

import org.scalatest.FunSuite

class JsonSchemaTest extends FunSuite {

  import argonaut._
  import Argonaut._

  def resourceToJson(path: String): Json = {
    val is   = scala.io.Source.fromInputStream(getClass.getResourceAsStream(path), "UTF-8")
    val text = is.mkString
    is.close()
    Parse.parse(text).right.get
  }

  def assertEncodeDecode[T](example: T)(implicit encode: EncodeJson[T], decode: DecodeJson[T]) = {
    val exampleJson = example.asJson(encode).toString
    assert(exampleJson.decodeOption(decode).get == example)
  }

  test("Conform to basic spec, encodes and decodes") {
    import schemaImplicits._

    case class PersonObject(firstName: String, lastName: String, age: Option[Int])

    implicit val intSchemaDef = minimumDef(0)

    implicit val personCodec = new FieldCodec[PersonObject] {

      val firstName = field("firstName", _.firstName)
      val lastName  = field("lastName", _.lastName)
      val age       = field("age", _.age, Some("Age in years"))

      override def encode(t: PersonObject): Json =
        jEmptyObject
          .->:(firstName(t))
          .->:(lastName(t))
          .->:(age(t))

      override def decode(c: HCursor): DecodeResult[PersonObject] =
        for {
          f <- c --\ firstName
          l <- c --\ lastName
          a <- c --\ age
        } yield PersonObject(f, l, a)

      val fields = List(firstName, lastName, age)
    }

    val personSpec = Schema(Some("Person"), None, personCodec)

    assert(
      personSpec.toSchema.pretty(PrettyParams.spaces2) == resourceToJson("/basic.schema.json").pretty(
        PrettyParams.spaces2))

    val example = PersonObject("John", "Doe", Some(42))
    assertEncodeDecode(example)
  }

  test("Complex object should flatten definitions") {
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

    implicit val addressCodec = new FieldCodec[Address] {
      val street  = field("street", _.street)
      val city    = field("city", _.city)
      val state   = field("state", _.state)
      val country = field("country", _.country)

      override def encode(t: Address): Json = Json.obj(
        street(t),
        city(t),
        state(t),
        country(t)
      )

      override def decode(c: HCursor): DecodeResult[Address] =
        for {
          street  <- c --\ street
          city    <- c --\ city
          state   <- c --\ state
          country <- c --\ country
        } yield Address(street, city, state, country)

      override val fields: List[JsonDef[Address, _]] = List(street, city, state, country)
    }

    implicit val addressSchema = Schema(Some("Address"), Some("Describes an address"), addressCodec)

    implicit val localDateSchemaDef = new StringSchemaDef[LocalDate] {}

    implicit val personCodec = new FieldCodec[Person] {
      val firstName = field("first_name", _.name)
      val lastName  = field("last_name", _.surname)
      val birthDay  = field("birthday", _.birthday)
      val address   = field("address", _.address)

      override def decode(c: HCursor): DecodeResult[Person] =
        for {
          f <- c --\ firstName
          l <- c --\ lastName
          b <- c --\ birthDay
          a <- c --\ address
        } yield Person(f, l, b, a)

      override val fields: List[JsonDef[Person, _]] = List(firstName, lastName, birthDay, address)
    }

    implicit val personModel = Schema(Some("Person"), Some("Describes a Person"), personCodec)

    case class PersonWrapper(p: Person)
    implicit val personWrapperCodec = new FieldCodec[PersonWrapper] {
      val person = field("person", _.p)

      override def decode(c: HCursor): DecodeResult[PersonWrapper] =
        for {
          p <- c --\ person
        } yield PersonWrapper(p)

      override val fields: List[JsonDef[PersonWrapper, _]] = List(person)
    }

    val personWrapperSchema = Schema(Some("PersonWrapper"), Some("Wraps a person"), personWrapperCodec)

    assert(personWrapperSchema.toSchema == resourceToJson("/complex-object.schema.json"))
    val example = PersonWrapper(Person("John", "Doe", LocalDate.now(), Address("foo", "bar", "bas", "barr")))
    assertEncodeDecode(example)
  }

  test("Object with enum") {
    import schemaImplicits._

    case class Person(name: String, gender: String)

    val personCodec = new FieldCodec[Person] {
      val name   = field("name", _.name)
      val gender = field("gender", _.gender, None, Set("male", "female"))

      override def decode(c: HCursor): DecodeResult[Person] = for {
        n <- c --\ name
        g <- c --\ gender
      } yield Person(n, g)

      override val fields: List[JsonDef[Person, _]] = List(name, gender)
    }

    val personModel = Schema(Some("Person"), Some("Some person"), personCodec)
    assert(personModel.toSchema === resourceToJson("/enum.schema.json"))
  }

}
