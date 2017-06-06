package no.mehl.argonaut

import argonaut.Argonaut.JsonField
import argonaut.{CodecJson, DecodeResult, HCursor, Json}
import argonaut._
import Argonaut._
import no.mehl.argonaut.TypeOps.typed

case class Field[M : EncodeJson](name: String, value: M, description: Option[String] = None, minimum: Option[Int] = None) {

  def asField: (JsonField, Json) = name := value

  def asSchema: (JsonField, Json) = {
    name := jEmptyObject
      .->?:(description.map("description" := _))
      .->:("type" := typed(value))
      .->?:(minimum.map("minimum" := _))
  }

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
      .->:("properties" := Json.obj(jsonEncoder.fields.map(f => {
        f.asSchema
      }): _*))
      .->:("required" := Json.array(jsonEncoder.fields.filter(_.isRequired).map(s => jString(s.name)) : _*))

  def jsonExample = codec.Encoder(example)
}

object TypeOps {

  // TODO: Smarter
  def typed[T](t: T): Option[String] = t match {
    case _: String => Some("string")
    case _: Int => Some("integer")
    case Some(o) => typed(o)
    case _ => Some("not implemented")
  }


}
