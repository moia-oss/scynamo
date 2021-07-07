package scynamo.generic

import cats.data.EitherNec
import cats.syntax.either._
import cats.syntax.parallel._
import scynamo.StackFrame.{Attr, Case}
import scynamo.{ScynamoEncodeError, ScynamoEncoder, StackFrame}
import shapeless._
import shapeless.labelled._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait ShapelessScynamoEncoder[Base, A] {
  def encodeMap(value: A): EitherNec[ScynamoEncodeError, java.util.Map[String, AttributeValue]]
}

object ShapelessScynamoEncoder extends EncoderHListInstances with EncoderCoproductInstances

trait EncoderHListInstances {
  implicit def deriveHNil[Base]: ShapelessScynamoEncoder[Base, HNil] =
    _ => Right(new java.util.HashMap)

  implicit def deriveHCons[Base, K <: Symbol, V, T <: HList](implicit
      key: Witness.Aux[K],
      sv: ScynamoEncoder[FieldType[K, V]],
      st: ShapelessScynamoEncoder[Base, T],
      opts: ScynamoDerivationOpts[Base] = ScynamoDerivationOpts.default[Base]
  ): ShapelessScynamoEncoder[Base, FieldType[K, V] :: T] = { value =>
    val fieldName   = opts.transform(key.value.name)
    val encodedHead = StackFrame.push(sv.encode(value.head), Attr(fieldName))
    val encodedTail = st.encodeMap(value.tail)
    (encodedHead, encodedTail).parMapN { case (head, tail) =>
      if (!head.nul) tail.put(fieldName, head)
      tail
    }
  }
}

trait EncoderCoproductInstances {
  implicit def deriveCNil[Base]: ShapelessScynamoEncoder[Base, CNil] =
    _ => Either.leftNec(ScynamoEncodeError.generalError("Cannot encode CNil", None))

  implicit def deriveCCons[Base, K <: Symbol, V, T <: Coproduct](implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoEncoder[V]],
      st: ShapelessScynamoEncoder[Base, T],
      opts: ScynamoSealedTraitOpts[Base] = ScynamoSealedTraitOpts.default[Base]
  ): ShapelessScynamoEncoder[Base, FieldType[K, V] :+: T] = {
    case Inl(left) =>
      val name = opts.transform(key.value.name)
      StackFrame.push(sv.value.encode(left), Case(name)).map { encoded =>
        val attr = new java.util.HashMap[String, AttributeValue](encoded.m())
        attr.put(opts.discriminator, AttributeValue.builder.s(name).build())
        attr
      }
    case Inr(right) =>
      st.encodeMap(right)
  }
}
