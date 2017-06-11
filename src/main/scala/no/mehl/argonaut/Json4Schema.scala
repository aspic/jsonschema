package no.mehl.argonaut

import argonaut.Argonaut.JsonField
import argonaut.{Argonaut, CodecJson, DecodeResult, HCursor, Json, _}
import Argonaut._
import no.mehl.argonaut.schemaImplicits.ModelSchemaDef

import scala.reflect.{ClassTag, classTag}

trait SchemaDef[T] {
  def asSchema[M: EncodeJson](name: String, description: Option[String], enum: Set[M]): (JsonField, Json) = {
    name := fields(name, description, enum)
  }

  def fields[M: EncodeJson](name: String, description: Option[String], enum: Set[M]) = {
    props
      .foldLeft(
        jEmptyObject
          .->?:(description.map("description" := _))) { case (json, jsonField) => json.->:(jsonField) }
      .->?:(Some(enum).filter(_.nonEmpty).map(e => "enum" := Json.array(e.toList.map(_.asJson): _*)))
  }
  val isRequired = true

  val schemaType: String

  def props: List[(JsonField, Json)] = List("type" := schemaType)
}

object schemaImplicits {

  implicit class HCursorOps(c: HCursor) {

    /** Move the cursor to the given sibling field in a JSON object (alias for `field`). */
    def --\[T: DecodeJson](f: JsonDef[_, T]): DecodeResult[T] = f.decode(c)
  }

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
    override def fields[F: EncodeJson](name: String, description: Option[String], enum: Set[F]) = ev.internalSchema
    val decoder                                                                                 = ev.decoder
  }

  trait MinimumDef {
    val minimum: Int

  }

  implicit val intSchemaDef    = new IntSchemaDef            {}
  implicit val stringSchemaDef = new StringSchemaDef[String] {}

  def minimumDef(min: Int) = new IntSchemaDef {
    override val props = super.props :+ ("minimum" := min)
  }

  implicit def optionSchemaDef[F](implicit ev: SchemaDef[F]) = new SchemaDef[Option[F]] {
    override val props               = ev.props
    override val schemaType: String  = ev.schemaType
    override val isRequired: Boolean = false
  }
}

case class JsonDef[Model, Prop: EncodeJson: DecodeJson](field: String, f: Model => Prop, description: Option[String])(
    implicit schemaDef: SchemaDef[Prop]) {

  def isRequired = schemaDef.isRequired

  def toJson(foo: Model): (JsonField, Json) = field := f(foo)

  def decode(c: HCursor): DecodeResult[Prop] = decode(c --\ field)
  def decode(c: ACursor): DecodeResult[Prop] = c.as[Prop]
  def toSchema: (JsonField, Json) =
    field :=
      schemaDef.props.foldLeft(
        jEmptyObject
          .->?:(description.map("description" := _))
          .->:("type" := schemaDef.schemaType)) {
        case (o, p) => o.->:(p)
      }
}

trait JsonSpec[T] {
  val fields: List[JsonDef[T, _]]
  def title: Option[String]       = None
  def description: Option[String] = None

  def encode(t: T): Json = Json.obj(fields.map(p => p.toJson(t)): _*)
  def decode(c: HCursor): DecodeResult[T]

  def codec: CodecJson[T] = CodecJson(
    encode,
    decode
  )

  def asType = {
    "object"
  }

  lazy val maybeFields = Some(fields).filterNot(_.isEmpty)

  lazy val properties =
    maybeFields.map(fields => "properties" := fields.foldLeft(jEmptyObject) { case (o, f) => o.->:(f.toSchema) })

  lazy val required =
    maybeFields.map(fields => "required" := Json.array(fields.filter(_.isRequired).map(_.field.asJson): _*))

  lazy val toSchema: Json = jEmptyObject
    .->:("$schema" := "http://json-schema.org/draft-04/schema#")
    .->?:(title.map("title" := _))
    .->:("type" := asType)
    .->?:(description.map("description" := _))
    .->?:(properties)
    .->?:(required)

  def field[Prop: EncodeJson: DecodeJson](field: String, f: T => Prop, description: Option[String] = None)(
      implicit schemaDef: SchemaDef[Prop]) = new JsonDef[T, Prop](field, f, description)
}

case class ModelDef[T](title: String, t: JsonSpec[T]) {}

case class Field[M: DecodeJson: EncodeJson](field: String,
                                            description: Option[String] = None,
                                            enum: Set[M] = Set[M]())(implicit schemaDef: SchemaDef[M]) {

  val isRequired = schemaDef.isRequired

  val withDef = schemaDef

  /** Returns this field as a part of the schema */
  def toSchema: (JsonField, Json) = schemaDef.asSchema[M](field, description, enum)

  /** Returns this field as a schema property or definition reference */
  def toProperty: (JsonField, Json) = schemaDef match {
    case _: ModelSchemaDef[_] => field := jEmptyObject.->:("$ref" := s"#/definitions/$field")
    case _                    => schemaDef.asSchema(field, description, enum)
  }

  def apply(c: ACursor): DecodeResult[M] =
    c.as[M]
      .flatMap(f => {
        if (enum.contains(f) || enum.isEmpty) DecodeResult.ok(f)
        else DecodeResult.fail(s"$f not found in $enum", CursorHistory.empty)
      })
  def apply(c: HCursor): DecodeResult[M] = apply(c --\ field)

  def apply[T: EncodeJson](t: T) = field := t
}

case class SchemaDecoder[T](decoder: HCursor => DecodeResult[T], fields: Field[_]*)

trait Json4Schema {
  def jsonSchema: Json
}

import scala.reflect.{ClassTag, classTag}

case class Model[T: ClassTag](title: String,
                              description: Option[String] = None,
                              example: Option[T] = None,
                              encoder: T => Json,
                              decoder: SchemaDecoder[T],
                              anyOf: Set[_ <: SchemaDef[_]] = Set())
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

  val maybeFields = Some(decoder.fields).filterNot(_.isEmpty)

  val required =
    maybeFields.map(fields => "required" := Json.array(fields.filter(_.isRequired).map(_.field.asJson): _*))
  val props =
    maybeFields.map(fields => "properties" := fields.foldLeft(jEmptyObject) { case (o, f) => o.->:(f.toProperty) })
  val anyOfProp = Some(anyOf)
    .filterNot(_.isEmpty)
    .map(any =>
      "anyOf" := Json.array(any.toList.flatMap(t => t.props).foldLeft(jEmptyObject) {
        case (o, p) => o.->:(p)
      }))

  private[argonaut] def internalSchema = {
    jEmptyObject
      .->:("$schema" := "http://json-schema.org/draft-04/schema#")
      .->:("title" := title)
      .->:("type" := getType)
      .->?:(description.map("description" := _))
      .->?:(props)
      .->?:(required)
      .->?:(anyOfProp)
  }

  def getType: String = {
    this match {
      case f if classTag[T] == classTag[String] => "string"
      case f if classTag[T] == classTag[Int]    => "integer"
      case _                                    => "object"
    }
  }

  def jsonSchema =
    internalSchema.->?:(
      Some(getFields(decoder))
        .filter(_.nonEmpty)
        .map(defs => "definitions" := defs.foldLeft(jEmptyObject) { case (o, f) => o.->:(f.toSchema) })
    )

  def jsonExample: Option[Json] = example.map(encoder)
}
