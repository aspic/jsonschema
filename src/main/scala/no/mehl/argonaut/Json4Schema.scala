package no.mehl.argonaut

import argonaut.Argonaut.JsonField
import argonaut.{CodecJson, DecodeResult, HCursor, Json}

import argonaut._
import Argonaut._

case class EncodedField[T](name: String, field: (String, T) => (JsonField, Json), jsonType: String, required: Boolean = false, description: Option[String] = None, minimum: Option[Int] = None) {
  def asField: (JsonField, Json) = name := jEmptyObject
    .->?:(description.map("description" := _))
    .->:("type" := jsonType)
    .->?:(minimum.map("minimum" := _))
}

case class Model[T](title: String, decoder: HCursor => DecodeResult[T], defs: EncodedField[T]*) {

  def encoder(o: T) = Json.obj(
    defs.map(f => f.field(f.name, o)) : _*
  )

  def asCodec: CodecJson[T] = CodecJson(
    encoder,
    decoder
  )

  def jsonSchema = Json.obj(
    "title" := title,
    "type" := "object",
    "properties" := Json.obj(defs.map(_.asField) : _*),
    "required" := Json.array(defs.filter(_.required).map(s => jString(s.name)) : _*)
  )
}
