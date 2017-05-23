package no.mehl.argonaut

import argonaut.Argonaut.JsonField
import argonaut.{CodecJson, DecodeResult, HCursor, Json}
import argonaut._
import Argonaut._

case class Field[T, M : EncodeJson](name: String, value: T => M, jsonType: String, required: Boolean = false, description: Option[String] = None, minimum: Option[Int] = None) {
  def asField: T => (JsonField, Json) = t => name := value(t)

  def asSchema: (JsonField, Json) = {
    name := jEmptyObject
      .->?:(description.map("description" := _))
      .->:("type" := jsonType)
      .->?:(minimum.map("minimum" := _))
  }
}

case class SchemaEncoder[T](fields: Field[T, _]*) extends EncodeJson[T] {
  override def encode(a: T): Json = Json.obj(
    fields.map(f => f.asField(a)) : _*
  )
}

trait Json4Schema {
  def jsonSchema: Json
}

case class Model[T](title: String, description: String, encoder: SchemaEncoder[T], decoder: HCursor => DecodeResult[T]) extends Json4Schema {

  def codec: CodecJson[T] = CodecJson(
    encoder.encode,
    decoder
  )

  def jsonSchema = jEmptyObject
      .->:("$schema" := "http://json-schema.org/draft-04/schema#")
      .->:("title" := title)
      .->:("description" := description)
      .->:("type" := "object")
      .->:("properties" := Json.obj(encoder.fields.map(_.asSchema): _*))
      .->:("required" := Json.array(encoder.fields.filter(_.required).map(s => jString(s.name)) : _*))
}
