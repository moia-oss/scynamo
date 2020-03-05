package scynamo.generic

import cats.syntax.either._
import cats.data.EitherNec
import scynamo.{ScynamoDecodeError, ScynamoDecoder}
import shapeless._
import shapeless.labelled._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait GenericScynamoEnumDecoder[A] extends ScynamoDecoder[A]

object GenericScynamoEnumDecoder extends GenericScynamoEnumDecoderInstances

trait GenericScynamoEnumDecoderInstances {
  implicit def derivedEnumDecoderInstance[F, G](
      implicit gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[ShapelessScynamoEnumDecoder[G]]
  ): GenericScynamoEnumDecoder[F] = attributeValue => sg.value.decode(attributeValue).map(gen.from)
}

trait ShapelessScynamoEnumDecoder[A] {
  def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, A]
}

object ShapelessScynamoEnumDecoder extends EnumDecoderCoproductInstances

trait EnumDecoderCoproductInstances {
  import scynamo.syntax.attributevalue._
  implicit val deriveCNil: ShapelessScynamoEnumDecoder[CNil] = _ =>
    Either.leftNec(ScynamoDecodeError.GeneralError("Cannot decode CNil", None))

  implicit def deriveCCons[K <: Symbol, V, T <: Coproduct](
      implicit
      key: Witness.Aux[K],
      sv: LabelledGeneric.Aux[V, HNil],
      st: Lazy[ShapelessScynamoEnumDecoder[T]]
  ): ShapelessScynamoEnumDecoder[FieldType[K, V] :+: T] = attributeValue => {
    if (attributeValue.sOpt.contains(key.value.name)) {
      Right(Inl(field[K](sv.from(HNil))))
    } else {
      st.value.decode(attributeValue).map(Inr(_))
    }
  }
}
