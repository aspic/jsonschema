package no.mehl.argonaut

import java.time.LocalDate

import argonaut.Argonaut.JsonField
import argonaut.{Argonaut, CodecJson, DecodeResult, HCursor, Json, _}
import Argonaut._
import no.mehl.argonaut.TypeOps.typed

trait Schematic[T] {
  def asSchema(name: String, value: T, description: Option[String]): (JsonField, Json) = {
    name := fields(name, value, description)
  }

  def fields(name: String, value: T, description: Option[String]) = {
      val json = jEmptyObject
        .->?:(description.map("description" := _))
        .->:("type" := typed(value))
      props.foldLeft(json)((f, json) => {
        f.->:(json)
      })
  }

  val props: List[(JsonField, Json)] = List()
}

object implicits {

  implicit def modelSchematic[F](implicit ev: Model[F]) = new Schematic[F] {
    override def fields(name: String, value: F, description: Option[String]): Json = ev.jsonSchema
  }

  implicit val stringSchematic = new Schematic[String]{}

  implicit val intSchematic = new Schematic[Int] {
    override val props = List(
      "minimum" := 0
    )
  }

  implicit def optionSchematic[F](implicit ev: Schematic[F]) = new Schematic[Option[F]] {
    override val props  = ev.props
  }

  implicit val localDate = new Schematic[LocalDate] {}
}

case class Field[M : EncodeJson](name: String, value: M, description: Option[String] = None)(implicit enc: Schematic[M]) {

  def asField: (JsonField, Json) = name := value

  def asSchema: (JsonField, Json) = enc.asSchema(name, value, description)

  val isRequired = value match {
    case _: Option[_] => false
    case _ => true
  }

}

case class SchemaEncoder[T](fields: Field[_]*) extends EncodeJson[T] {
  override def encode(a: T): Json = Json.obj(
    fields.map(f => f.asField) : _*
  )
}

trait Json4Schema {
  def jsonSchema: Json
}

case class Model[T](title: String, description: Option[String] = None, example: T, encoder: T => SchemaEncoder[T], decoder: HCursor => DecodeResult[T]) extends Json4Schema {

  val jsonEncoder = encoder(example)

  val codec: CodecJson[T] = CodecJson(
    jsonEncoder.encode,
    decoder
  )

  def jsonSchema = jEmptyObject
      .->:("$schema" := "http://json-schema.org/draft-04/schema#")
      .->:("title" := title)
      .->?:(description.map("description" := _))
      .->:("type" := "object")
      .->:("properties" := Json.obj(jsonEncoder.fields.map(_.asSchema): _*))
      .->:("required" := Json.array(jsonEncoder.fields.filter(_.isRequired).map(s => jString(s.name)) : _*))

  def jsonExample = codec.Encoder(example)
}

object TypeOps {

  // TODO: Smarter
  def typed[T](t: T): Option[String] = t match {
    case _: String => Some("string")
    case _: Int => Some("integer")
    case Some(o) => typed(o)
    case _ => Some("string") // TODO: Ok fallback?
  }


}
