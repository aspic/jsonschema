package no.mehl.argonaut

import java.time.LocalDate

import argonaut.Argonaut.JsonField
import argonaut.{Argonaut, CodecJson, DecodeResult, HCursor, Json, _}
import Argonaut._
import no.mehl.argonaut.TypeOps.typed

trait Schematic[T] {
  def asSchema(name: String, description: Option[String]): (JsonField, Json) = {
    name := fields(name, description)
  }

  def fields(name: String, description: Option[String]) = {
    val json = jEmptyObject
      .->?:(description.map("description" := _))
      .->:("type" := schemaType)
    props.foldLeft(json)((f, json) => {
      f.->:(json)
    })
  }
  val isDefinition = false
  val schemaType: String

  val props: List[(JsonField, Json)] = List()
}

object implicits {

  implicit def modelSchematic[F](implicit ev: Model[F]) = new Schematic[F] {
    override val isDefinition: Boolean                                   = true
    override def fields(name: String, description: Option[String]): Json = ev.jsonSchema

    override val schemaType: String = "object"
  }

  implicit val stringSchematic = new Schematic[String] {
    override val schemaType: String = "string"
  }

  implicit val intSchematic = new Schematic[Int] {
    override val props = List(
      "minimum" := 0
    )
    override val schemaType: String = "integer"
  }

  implicit def optionSchematic[F](implicit ev: Schematic[F]) = new Schematic[Option[F]] {
    override val props              = ev.props
    override val schemaType: String = ev.schemaType
  }

  implicit val localDate = new Schematic[LocalDate] {
    override val schemaType: String = "string"
  }

}

case class Field[F, M: EncodeJson](name: String, value: F => M, description: Option[String] = None)(
    implicit enc: Schematic[M]) {

  def asField(foo: F): (JsonField, Json) = name := value(foo)

  def asSchema: (JsonField, Json) = enc.asSchema(name, description)

  val isDefinition = enc.isDefinition

  val isRequired = value match {
    case _: Option[_] => false
    case _            => true
  }

}

/**
case class ObjectEncoder(fields: List[Field[_]] = List()) {
  def ->: (field: Field[_]) = ObjectEncoder(fields :+ field)
}
  */
case class SchemaEncoder[T](fields: Field[T, _]*) {
  def encode(a: T): Json = Json.obj(
    fields.map(f => f.asField(a)): _*
  )
}

trait Json4Schema {
  def jsonSchema: Json
}

case class Model[T](title: String,
                    description: Option[String] = None,
                    example: Option[T] = None,
                    encoder: SchemaEncoder[T],
                    decoder: HCursor => DecodeResult[T])
    extends Json4Schema {

  val codec: CodecJson[T] = CodecJson(
    encoder.encode,
    decoder
  )

  def toDefinition(schemaEncoder: SchemaEncoder[T]): Option[Json] = {
    val definitions = schemaEncoder.fields.filter(_.isDefinition)
    if (definitions.isEmpty) None
    else Some(Json.obj(definitions.map(_.asSchema): _*))
  }

  def jsonSchema =
    jEmptyObject
      .->:("$schema" := "http://json-schema.org/draft-04/schema#")
      .->:("title" := title)
      .->?:(description.map("description" := _))
      .->:("type" := "object")
      .->?:(toDefinition(encoder).map("definitions" := _))
      .->:("properties" := Json.obj(encoder.fields.map(f => {
        if (f.isDefinition) f.name := jEmptyObject.->:("$ref" := s"#/definitions/${f.name}")
        else f.asSchema
      }): _*))
      .->:("required" := Json.array(encoder.fields.filter(_.isRequired).map(s => jString(s.name)): _*))

  def jsonExample: Option[Json] = example.map(encoder.encode)
}

object TypeOps {

  // array, boolean, integer, null, number, object, string

  // TODO: Smarter
  def typed[T](t: T): Option[String] = t match {
    case _: String => Some("string")
    case _: Int    => Some("integer")
    case Some(o)   => typed(o)
    case _         => Some("string") // TODO: Ok fallback?
  }

}
