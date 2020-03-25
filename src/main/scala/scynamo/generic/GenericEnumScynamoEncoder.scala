package scynamo.generic

import cats.data.EitherNec
import scynamo.{ScynamoEncodeError, ScynamoEncoder}
import shapeless._
import shapeless.labelled._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait GenericScynamoEnumEncoder[A] extends ScynamoEncoder[A]

object GenericScynamoEnumEncoder extends GenericScynamoEnumEncoderInstances

trait GenericScynamoEnumEncoderInstances {
  implicit def derivedEnumEncoderInstance[F, G](
      implicit gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[ShapelessScynamoEnumEncoder[G]]
  ): GenericScynamoEnumEncoder[F] = value => sg.value.encode(gen.to(value))
}

trait ShapelessScynamoEnumEncoder[A] {
  def encode(value: A): EitherNec[ScynamoEncodeError, AttributeValue]
}

object ShapelessScynamoEnumEncoder extends EnumEncoderCoproductInstances

trait EnumEncoderCoproductInstances {
  implicit val deriveCNil: ShapelessScynamoEnumEncoder[CNil] = _ => throw new NotImplementedError(s"Encoding CNil as Enum is not possible!")

  implicit def deriveCCons[K <: Symbol, V, T <: Coproduct](
      implicit
      key: Witness.Aux[K],
      st: Lazy[ShapelessScynamoEnumEncoder[T]]
  ): ShapelessScynamoEnumEncoder[FieldType[K, V] :+: T] = {
    case Inl(_)    => Right(AttributeValue.builder().s(key.value.name).build())
    case Inr(tail) => st.value.encode(tail)
  }
}
