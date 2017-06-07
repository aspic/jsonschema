package no.mehl.argonaut

import argonaut.Argonaut.JsonField
import argonaut.{Argonaut, CodecJson, DecodeResult, HCursor, Json, _}
import Argonaut._

trait SchemaDef[T] {
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
  val isRequired   = true

  val schemaType: String

  val props: List[(JsonField, Json)] = List()
}

object schemaImplicits {

  trait IntSchemaDef extends SchemaDef[Int] {
    override val schemaType: String = "integer"
  }

  trait StringSchemaDef[T] extends SchemaDef[T] {
    override val schemaType: String = "string"
  }

  implicit def modelSchemaDef[F](implicit ev: Model[F]) = new SchemaDef[F] {
    override val isDefinition: Boolean                                   = true
    override def fields(name: String, description: Option[String]): Json = ev.jsonSchema

    override val schemaType: String = "object"
  }

  trait MinimumDef {
    val minimum: Int
    val minProps = List("minimum" := minimum)
  }

  implicit val intSchemaDef    = new IntSchemaDef            {}
  implicit val stringSchemaDef = new StringSchemaDef[String] {}

  implicit def optionSchemaDef[F](implicit ev: SchemaDef[F]) = new SchemaDef[Option[F]] {
    override val props               = ev.props
    override val schemaType: String  = ev.schemaType
    override val isRequired: Boolean = false
  }
}

case class Field[M: DecodeJson](field: String, description: Option[String] = None)(implicit schemaDef: SchemaDef[M]) {

  val isDefinition = schemaDef.isDefinition

  val isRequired = schemaDef.isRequired

  def asSchema: (JsonField, Json) = schemaDef.asSchema(field, description)

  def apply(c: HCursor) = {
    (c --\ field).as[M]
  }
}

case class SchemaDecoder[T](decoder: HCursor => DecodeResult[T], fields: Field[_]*)

trait Json4Schema {
  def jsonSchema: Json
}

case class Model[T](title: String,
                    description: Option[String] = None,
                    example: Option[T] = None,
                    encoder: T => Json,
                    decoder: SchemaDecoder[T])
    extends Json4Schema {

  val codec: CodecJson[T] = CodecJson(
    encoder,
    decoder.decoder
  )

  def toDefinition(schemaEncoder: SchemaDecoder[T]): Option[Json] = {
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
      .->?:(toDefinition(decoder).map("definitions" := _))
      .->:("properties" := Json.obj(decoder.fields.map(f => {
        if (f.isDefinition) f.field := jEmptyObject.->:("$ref" := s"#/definitions/${f.field}")
        else f.asSchema
      }): _*))
      .->:("required" := Json.array(decoder.fields.filter(_.isRequired).map(s => jString(s.field)): _*))

  def jsonExample: Option[Json] = example.map(encoder)
}