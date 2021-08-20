package scynamo.generic

import cats.data.EitherNec
import cats.syntax.either._
import scynamo.StackFrame.Enum
import scynamo.{ScynamoDecodeError, ScynamoDecoder, ScynamoType}
import shapeless._
import shapeless.labelled._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait GenericScynamoEnumDecoder[A] extends ScynamoDecoder[A]

object GenericScynamoEnumDecoder extends GenericScynamoEnumDecoderInstances

trait GenericScynamoEnumDecoderInstances {
  implicit def derivedEnumDecoderInstance[F, G](implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: ShapelessScynamoEnumDecoder[G]
  ): GenericScynamoEnumDecoder[F] = attributeValue => sg.decode(attributeValue).map(gen.from)
}

trait ShapelessScynamoEnumDecoder[A] {
  def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, A]
}

object ShapelessScynamoEnumDecoder extends EnumDecoderCoproductInstances

trait EnumDecoderCoproductInstances {
  import scynamo.syntax.attributevalue._
  implicit val deriveCNil: ShapelessScynamoEnumDecoder[CNil] = value => Either.leftNec(ScynamoDecodeError.invalidCoproductCaseAttr(value))

  implicit def deriveCCons[K <: Symbol, V, T <: Coproduct](implicit
      key: Witness.Aux[K],
      sv: LabelledGeneric.Aux[V, HNil],
      st: ShapelessScynamoEnumDecoder[T]
  ): ShapelessScynamoEnumDecoder[FieldType[K, V] :+: T] =
    attributeValue =>
      if (attributeValue.asOption(ScynamoType.String).contains(key.value.name))
        Right(Inl(field[K](sv.from(HNil))))
      else
        st.decode(attributeValue).map(Inr(_)).leftMap(_.map(_.push(Enum(key.value.name))))
}
