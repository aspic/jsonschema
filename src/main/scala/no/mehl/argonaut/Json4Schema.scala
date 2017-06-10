package no.mehl.argonaut

import argonaut.Argonaut.JsonField
import argonaut.{Argonaut, CodecJson, DecodeResult, HCursor, Json, _}
import Argonaut._
import no.mehl.argonaut.schemaImplicits.ModelSchemaDef

trait SchemaDef[T] {
  def asSchema[M: EncodeJson](name: String, description: Option[String], enum: Set[M]): (JsonField, Json) = {
    name := fields(name, description, enum)
  }

  def fields[M: EncodeJson](name: String, description: Option[String], enum: Set[M]) = {
    props.foldLeft(
      jEmptyObject
        .->?:(description.map("description" := _))
        .->:("type" := schemaType)) { case (json, jsonField) => json.->:(jsonField) }
          .->?:(Some(enum).filter(_.nonEmpty).map(e => "enum" := Json.array(e.toList.map(_.asJson) : _*)))
  }
  val isRequired = true

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

  trait ModelSchemaDef[T] extends SchemaDef[T] {
    override val schemaType: String = "object"
    val decoder: SchemaDecoder[T]
  }

  implicit def modelSchemaDef[F](implicit ev: Model[F]) = new ModelSchemaDef[F] {
    override def fields[F : EncodeJson](name: String, description: Option[String], enum: Set[F]) = ev.internalSchema
    val decoder                                                          = ev.decoder
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

case class Field[M: DecodeJson : EncodeJson](field: String, description: Option[String] = None, enum: Set[M] = Set[M]())(implicit schemaDef: SchemaDef[M]) {

  val isRequired = schemaDef.isRequired

  val withDef = schemaDef

  /** Returns this field as a part of the schema */
  def toSchema: (JsonField, Json) = schemaDef.asSchema[M](field, description, enum)

  /** Returns this field as a schema property or definition reference */
  def toProperty: (JsonField, Json) = schemaDef match {
    case _: ModelSchemaDef[_] => field := jEmptyObject.->:("$ref" := s"#/definitions/$field")
    case _                    => schemaDef.asSchema(field, description, enum)
  }

  def apply(c: ACursor): DecodeResult[M] = c.as[M].flatMap(f => {
    if (enum.contains(f)) DecodeResult.ok(f)
    else DecodeResult.fail(s"$f not found in $enum", CursorHistory.empty)
  })
  def apply(c: HCursor): DecodeResult[M] = apply(c --\ field)

  def apply[T: EncodeJson](t: T) = field := t
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

  private def getFields(schemaDecoder: SchemaDecoder[_]): List[Field[_]] = {
    schemaDecoder.fields
      .map(f => (f, f.withDef))
      .collect {
        case (f: Field[_], m: ModelSchemaDef[_]) => getFields(m.decoder) :+ f
      }
      .flatten
      .toList
  }

  private def required: Json = Json.array(decoder.fields.filter(_.isRequired).map(_.field.asJson): _*)

  private[argonaut] def internalSchema = {
    jEmptyObject
      .->:("$schema" := "http://json-schema.org/draft-04/schema#")
      .->:("title" := title)
      .->?:(description.map("description" := _))
      .->:("type" := "object")
      .->:("properties" := decoder.fields.foldLeft(jEmptyObject) { case (o, f) => o.->:(f.toProperty) })
      .->:("required" := required)
  }

  def jsonSchema =
    internalSchema.->?:(
      Some(getFields(decoder))
        .filter(_.nonEmpty)
        .map(defs => "definitions" := defs.foldLeft(jEmptyObject) { case (o, f) => o.->:(f.toSchema) })
    )

  def jsonExample: Option[Json] = example.map(encoder)
}
