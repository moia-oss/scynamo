package scynamo.generic

import cats.data.EitherNec
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.either._
import io.moia.dynamo.DynamoType.DynamoString
import io.moia.dynamo.attributevalue.dsl._
import io.moia.dynamo._
import scynamo.DynamoType.DynamoString
import scynamo.{DynamoDecodeError, DynamoDecoder, DynamoDecoderFunctions, DynamoType, InvalidCase, MissingFieldInMap}
import shapeless._
import shapeless.labelled._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait ShapelessDynamoDecoder[A] {
  def decodeMap(value: java.util.Map[String, AttributeValue]): EitherNec[DynamoDecodeError, A]
}

object ShapelessDynamoDecoder extends DecoderHListInstances with DecoderCoproductInstances

trait DecoderHListInstances extends DynamoDecoderFunctions {
  implicit def deriveHNil: ShapelessDynamoDecoder[HNil] = _ => Right(HNil)

  implicit def deriveHCons[K <: Symbol, V, T <: HList](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[DynamoDecoder[V]],
      st: Lazy[ShapelessDynamoDecoder[T]]
  ): ShapelessDynamoDecoder[FieldType[K, V] :: T] =
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

trait DecoderCoproductInstances extends DynamoDecoderFunctions {
  implicit def deriveCNil: ShapelessDynamoDecoder[CNil] = value => Either.leftNec(InvalidCase(value))

  implicit def deriveCCons[K <: Symbol, V, T <: Coproduct](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[DynamoDecoder[V]],
      st: Lazy[ShapelessDynamoDecoder[T]]
  ): ShapelessDynamoDecoder[FieldType[K, V] :+: T] =
    value => {
      for {
        typeTagAttrValue <- Option(value.get(DynamoType.MAGIC_TYPE_ATTRIBUTE_NAME))
          .map(Right(_))
          .getOrElse(Either.leftNec(MissingFieldInMap(key.value.name, value)))
        typeTag <- accessOrTypeMismatch(typeTagAttrValue, DynamoString)(_.sOpt)
        result <- if (key.value.name == typeTag) sv.value.decode(AttributeValue.builder().m(value).build()).map(v => Inl(field[K](v)))
        else st.value.decodeMap(value).map(Inr(_))
      } yield result
    }
}
