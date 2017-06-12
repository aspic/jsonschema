package no.mehl.jsonschema

import argonaut.Argonaut.JsonField
import argonaut.{Argonaut, CodecJson, DecodeResult, HCursor, Json, _}
import Argonaut._
import no.mehl.jsonschema.schemaImplicits.ModelSchemaDef

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

  def jsonProps: Json = props.foldLeft(jEmptyObject) {
    case (o, p) => o.->:(p)
  }

  def props: List[(JsonField, Json)] = List("type" := schemaType)
}

case class JsonDef[Model, Prop: EncodeJson: DecodeJson](field: String,
                                                        f: Model => Prop,
                                                        description: Option[String],
                                                        enum: Set[Prop],
                                                        allOf: Set[SchemaDef[_]])(implicit schemaDef: SchemaDef[Prop]) {

  def isRequired = schemaDef.isRequired

  def toJson(foo: Model): (JsonField, Json) = field := f(foo)

  def apply(model: Model): (JsonField, Json) = field := f(model)

  def decode(c: HCursor): DecodeResult[Prop] = decode(c --\ field)
  def decode(c: ACursor): DecodeResult[Prop] = c.as[Prop]

  /** Returns this field as a schema property or definition reference */
  def toProperty: (JsonField, Json) = schemaDef match {
    case m: ModelSchemaDef[_] => field := jEmptyObject
      .->:("$ref" := s"#/definitions/${m.name}")
        .->?:(Some(allOf.toList).filterNot(_.isEmpty).map(all => "allOf" := Json.array(all.map(f => f.asSchema(field, description, enum)._2): _*)))
    case _                    => toSchema
  }

  def toSchema: (JsonField, Json) = schemaDef match {
    case m: ModelSchemaDef[_] => schemaDef.asSchema[Prop](m.name, description, enum)
    case _                    => schemaDef.asSchema[Prop](field, description, enum)
  }

  val withDef = schemaDef
}

trait FieldCodec[Model] extends EncodeJson[Model] with DecodeJson[Model] {

  /** An ad-hoc encoder for a json object, override for more specialization */
  def encode(t: Model): Json = fields.foldLeft(jEmptyObject) {
    case (o, p) => o.->:(p.toJson(t))
  }

  def decode(c: HCursor): DecodeResult[Model]

  val fields: List[JsonDef[Model, _]]

  def field[Prop: EncodeJson: DecodeJson](field: String,
                                          f: Model => Prop,
                                          description: Option[String] = None,
                                          enum: Set[Prop] = Set[Prop](),
                                          allOf: Set[SchemaDef[_]] = Set())(implicit schemaDef: SchemaDef[Prop]) =
    new JsonDef[Model, Prop](field, f, description, enum, allOf)
}

case class Schema[T: ClassTag](title: Option[String], description: Option[String], fieldCodec: FieldCodec[T]) {

  def codec: CodecJson[T] = CodecJson(fieldCodec.encode, fieldCodec.decode)

  lazy val fields = Some(fieldCodec.fields).filterNot(_.isEmpty)

  lazy val properties =
    fields.map(fields => "properties" := fields.foldLeft(jEmptyObject) { case (o, f) => o.->:(f.toProperty) })

  lazy val required =
    fields.map(fields => "required" := Json.array(fields.filter(_.isRequired).map(_.field.asJson): _*))

  lazy val internalSchema: Json = jEmptyObject
    .->:("$schema" := "http://json-schema.org/draft-04/schema#")
    .->?:(required)
    .->?:(title.map("title" := _))
    .->:("type" := asType)
    .->?:(description.map("description" := _))
    .->?:(properties)

  lazy val toSchema: Json = internalSchema
    .->?:(
      Some(getFields(fieldCodec))
        .filter(_.nonEmpty)
        .map(defs => "definitions" := defs.foldLeft(jEmptyObject) { case (o, f) => o.->:(f.toSchema) }))

  def asType = this match {
    case _ if classTag[T] == classTag[String] => "string"
    case _ if classTag[T] == classTag[Int]    => "integer"
    case _                                    => "object"
  }

  /** Gets all complex fields which in turn are made into references */
  private def getFields(schemaDecoder: FieldCodec[T]): List[JsonDef[T, _]] = {
    schemaDecoder.fields
      .map(f => (f, f.withDef))
      .collect {
        case (f: JsonDef[T, _], m: ModelSchemaDef[T]) => getFields(m.fieldCodec) :+ f
      }
      .flatten
  }
}

object Schema {
  def apply[T : ClassTag](f: FieldCodec[T]): Schema[T] = Schema(None, None, f)
}

object schemaImplicits {

  implicit class HCursorOps(c: HCursor) {
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
    val name: String
    val fieldCodec: FieldCodec[T]
  }

  implicit def modelSchemaDef[F](implicit ev: Schema[F]) = new ModelSchemaDef[F] {
    override def fields[F: EncodeJson](name: String, description: Option[String], enum: Set[F]) = ev.internalSchema
    val fieldCodec                                                                              = ev.fieldCodec
    override val name: String                                                                   = ev.title.map(_.toLowerCase).getOrElse("unnamed")
  }

  implicit def listSchemaDef[F](implicit ev: SchemaDef[F]) = new SchemaDef[List[F]] {
    override val schemaType: String = "array"

    override val props = super.props :+ ("items" := ev.jsonProps)
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
