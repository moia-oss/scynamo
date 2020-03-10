package scynamo.generic

import cats.data.EitherNec
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.either._
import scynamo.ScynamoDecodeError._
import scynamo._
import shapeless._
import shapeless.labelled._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait ShapelessScynamoDecoder[Base, A] {
  def decodeMap(value: java.util.Map[String, AttributeValue]): EitherNec[ScynamoDecodeError, A]
}

object ShapelessScynamoDecoder extends DecoderHListInstances with DecoderCoproductInstances

trait DecoderHListInstances extends ScynamoDecoderFunctions {
  implicit def deriveHNil[Base]: ShapelessScynamoDecoder[Base, HNil] = _ => Right(HNil)

  implicit def deriveHCons[Base, K <: Symbol, V, T <: HList](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: Lazy[ShapelessScynamoDecoder[Base, T]],
      opts: ScynamoDerivationOpts[Base] = ScynamoDerivationOpts.default[Base]
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :: T] =
    value => {
      val fieldName      = opts.transform(key.value.name)
      val fieldAttrValue = Option(value.get(fieldName))

      val decodedHead = (fieldAttrValue, sv.value.defaultValue) match {
        case (Some(field), _)      => sv.value.decode(field)
        case (None, Some(default)) => Right(default)
        case (None, None)          => Either.leftNec(MissingField(fieldName, value))
      }

      (decodedHead.map(field[K](_)), st.value.decodeMap(value)).mapN(_ :: _)
    }
}

trait DecoderCoproductInstances extends ScynamoDecoderFunctions {
  import scynamo.syntax.attributevalue._

  implicit def deriveCNil[Base]: ShapelessScynamoDecoder[Base, CNil] = value => Either.leftNec(InvalidCoproductCaseMap(value))

  implicit def deriveCCons[Base, K <: Symbol, V, T <: Coproduct](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: Lazy[ShapelessScynamoDecoder[Base, T]],
      opts: ScynamoSealedTraitOpts[Base] = ScynamoSealedTraitOpts.default[Base]
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :+: T] =
    value => deriveCConsTagged.decodeMap(value).orElse(deriveCConsNested.decodeMap(value))

  def deriveCConsTagged[Base, K <: Symbol, V, T <: Coproduct](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: Lazy[ShapelessScynamoDecoder[Base, T]],
      opts: ScynamoSealedTraitOpts[Base]
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :+: T] =
    value => {
      val name = opts.transform(key.value.name)
      for {
        typeTagAttrValue <- Option(value.get(opts.discriminator))
          .map(Right(_))
          .getOrElse(Either.leftNec(MissingField(name, value)))
        typeTag <- typeTagAttrValue.asEither(ScynamoType.String)
        result <- if (name == typeTag) {
          sv.value.decode(AttributeValue.builder().m(value).build()).map(v => Inl(field[K](v)))
        } else {
          st.value.decodeMap(value).map(Inr(_))
        }
      } yield result
    }

  def deriveCConsNested[Base, K <: Symbol, V, T <: Coproduct](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: Lazy[ShapelessScynamoDecoder[Base, T]],
      opts: ScynamoSealedTraitOpts[Base]
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :+: T] =
    value => {
      Option(value.get(opts.transform(key.value.name))) match {
        case Some(nestedValue) => sv.value.decode(nestedValue).map(v => Inl(field[K](v)))
        case _                 => st.value.decodeMap(value).map(Inr(_))
      }
    }

}
