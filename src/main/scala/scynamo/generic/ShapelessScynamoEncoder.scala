package scynamo.generic

import java.util
import java.util.Collections

import cats.data.EitherNec
import cats.syntax.either._
import cats.syntax.parallel._
import scynamo.StackFrame.{Attr, Case}
import scynamo.{ScynamoEncodeError, ScynamoEncoder}
import shapeless._
import shapeless.labelled._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait ShapelessScynamoEncoder[Base, A] {
  def encodeMap(value: A): EitherNec[ScynamoEncodeError, java.util.Map[String, AttributeValue]]
}

object ShapelessScynamoEncoder extends EncoderHListInstances with EncoderCoproductInstances

trait EncoderHListInstances {
  implicit def deriveHNil[Base]: ShapelessScynamoEncoder[Base, HNil] = _ => Right(Collections.emptyMap())

  implicit def deriveHCons[Base, K <: Symbol, V, T <: HList](implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoEncoder[V]],
      st: ShapelessScynamoEncoder[Base, T],
      opts: ScynamoDerivationOpts[Base] = ScynamoDerivationOpts.default[Base]
  ): ShapelessScynamoEncoder[Base, FieldType[K, V] :: T] =
    value => {
      val fieldName = opts.transform(key.value.name)

      val encodedHead = sv.value.encode(value.head).leftMap(x => x.map(_.push(Attr(fieldName))))
      val encodedTail = st.encodeMap(value.tail)

      (encodedHead, encodedTail).parMapN { case (head, tail) =>
        val hm = new util.HashMap[String, AttributeValue]()
        hm.putAll(tail)
        hm.put(fieldName, head)
        hm
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
    case Inl(l) =>
      val name = opts.transform(key.value.name)
      sv.value.encode(l).leftMap(x => x.map(_.push(Case(name)))).map(_.m()).map { vs =>
        val hm = new util.HashMap[String, AttributeValue]()
        hm.putAll(vs)
        hm.put(opts.discriminator, AttributeValue.builder().s(name).build())
        hm
      }
    case Inr(r) => st.encodeMap(r)
  }
}
