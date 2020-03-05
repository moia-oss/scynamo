package scynamo.generic

import java.util
import java.util.Collections

import scynamo.{ScynamoEncoder, ScynamoType}
import shapeless._
import shapeless.labelled._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait ShapelessScynamoEncoder[Base, A] {
  def encodeMap(value: A): java.util.Map[String, AttributeValue]
}

object ShapelessScynamoEncoder extends EncoderHListInstances with EncoderCoproductInstances

trait EncoderHListInstances {
  implicit def deriveHNil[Base]: ShapelessScynamoEncoder[Base, HNil] = _ => Collections.emptyMap()

  implicit def deriveHCons[Base, K <: Symbol, V, T <: HList](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoEncoder[V]],
      st: Lazy[ShapelessScynamoEncoder[Base, T]],
      opts: ScynamoDerivationOpts[Base] = ScynamoDerivationOpts.default[Base]
  ): ShapelessScynamoEncoder[Base, FieldType[K, V] :: T] =
    value => {
      val fieldName = opts.transform(key.value.name)

      val tail = st.value.encodeMap(value.tail)

      val hm = new util.HashMap[String, AttributeValue]()
      hm.putAll(tail)
      hm.put(fieldName, sv.value.encode(value.head))
      hm
    }
}

trait EncoderCoproductInstances {
  implicit def deriveCNil[Base]: ShapelessScynamoEncoder[Base, CNil] = _ => Collections.emptyMap()

  implicit def deriveCCons[Base, K <: Symbol, V, T <: Coproduct](
      implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoEncoder[V]],
      st: Lazy[ShapelessScynamoEncoder[Base, T]]
  ): ShapelessScynamoEncoder[Base, FieldType[K, V] :+: T] = {
    case Inl(l) =>
      val hm = new util.HashMap[String, AttributeValue]()
      hm.putAll(sv.value.encode(l).m())
      hm.put(ScynamoType.MAGIC_TYPE_ATTRIBUTE_NAME, AttributeValue.builder().s(key.value.name).build())
      hm
    case Inr(r) => st.value.encodeMap(r)
  }
}
