package scynamo.generic

import cats.data.EitherNec
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.either._
import scynamo.ScynamoType.ScynamoString
import scynamo._
import scynamo.dsl.attributevalue._
import shapeless._
import shapeless.labelled._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait ShapelessScynamoDecoder[A] {
  def decodeMap(value: java.util.Map[String, AttributeValue]): EitherNec[ScynamoDecodeError, A]
}

object ShapelessScynamoDecoder extends DecoderHListInstances with DecoderCoproductInstances

trait DecoderHListInstances extends ScynamoDecoderFunctions {
  implicit def deriveHNil: ShapelessScynamoDecoder[HNil] = _ => Right(HNil)

  implicit def deriveHCons[K <: Symbol, V, T <: HList](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: Lazy[ShapelessScynamoDecoder[T]]
  ): ShapelessScynamoDecoder[FieldType[K, V] :: T] =
    value => {
      val decodedHead = for {
        fieldAttrValue <- Option(value.get(key.value.name))
          .map(Right(_))
          .getOrElse(Either.leftNec(MissingFieldInMap(key.value.name, value)))
        result <- sv.value.decode(fieldAttrValue)
      } yield result

      (decodedHead.map(field[K](_)), st.value.decodeMap(value)).mapN(_ :: _)
    }
}

trait DecoderCoproductInstances extends ScynamoDecoderFunctions {
  implicit def deriveCNil: ShapelessScynamoDecoder[CNil] = value => Either.leftNec(InvalidCase(value))

  implicit def deriveCCons[K <: Symbol, V, T <: Coproduct](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: Lazy[ShapelessScynamoDecoder[T]]
  ): ShapelessScynamoDecoder[FieldType[K, V] :+: T] =
    value => deriveCConsTagged.decodeMap(value).orElse(deriveCConsNested.decodeMap(value))

  def deriveCConsTagged[K <: Symbol, V, T <: Coproduct](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: Lazy[ShapelessScynamoDecoder[T]]
  ): ShapelessScynamoDecoder[FieldType[K, V] :+: T] =
    value =>
      for {
        typeTagAttrValue <- Option(value.get(ScynamoType.MAGIC_TYPE_ATTRIBUTE_NAME))
          .map(Right(_))
          .getOrElse(Either.leftNec(MissingFieldInMap(key.value.name, value)))
        typeTag <- accessOrTypeMismatch(typeTagAttrValue, ScynamoString)(_.sOpt)
        result <- if (key.value.name == typeTag) {
          sv.value.decode(AttributeValue.builder().m(value).build()).map(v => Inl(field[K](v)))
        } else {
          st.value.decodeMap(value).map(Inr(_))
        }
      } yield result

  def deriveCConsNested[K <: Symbol, V, T <: Coproduct](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: Lazy[ShapelessScynamoDecoder[T]]
  ): ShapelessScynamoDecoder[FieldType[K, V] :+: T] =
    value => {
      Option(value.get(key.value.name)) match {
        case Some(nestedValue) => sv.value.decode(nestedValue).map(v => Inl(field[K](v)))
        case _                 => st.value.decodeMap(value).map(Inr(_))
      }
    }

}
