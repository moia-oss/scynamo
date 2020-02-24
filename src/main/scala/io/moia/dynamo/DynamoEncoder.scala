package io.moia.dynamo

import java.time.Instant
import java.util
import java.util.Collections

import shapeless._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait DynamoEncoder[A] { self =>
  def encode(value: A): AttributeValue

  def contramap[B](f: B => A): DynamoEncoder[B] = value => self.encode(f(value))
}

object DynamoEncoder extends LabelledTypeClassCompanion[DynamoEncoder] with DynamoEncoderInstances {
  val MAGIC_TYPE_ATTRIBUTE_NAME = "_MAGIC_TYPE_TAG_"

  object typeClass extends LabelledTypeClass[DynamoEncoder] {
    private[this] def newMap(): util.Map[String, AttributeValue] =
      new util.HashMap[String, AttributeValue]()

    def emptyProduct: DynamoEncoder[HNil] = _ => AttributeValue.builder().m(Collections.emptyMap()).build()

    def product[F, T <: HList](name: String, encoderHead: DynamoEncoder[F], encoderTail: DynamoEncoder[T]): DynamoEncoder[F :: T] =
      value => {
        val tail = encoderTail.encode(value.tail)

        val hm = newMap()
        hm.putAll(tail.m())
        hm.put(name, encoderHead.encode(value.head))
        AttributeValue.builder().m(hm).build()
      }

    def emptyCoproduct: DynamoEncoder[CNil] = _ => AttributeValue.builder().m(Collections.emptyMap()).build()

    def coproduct[L, R <: Coproduct](name: String, encodeL: => DynamoEncoder[L], encodeR: => DynamoEncoder[R]): DynamoEncoder[L :+: R] = {
      case Inl(l) =>
        val hm = newMap()
        hm.putAll(encodeL.encode(l).m())
        hm.put(MAGIC_TYPE_ATTRIBUTE_NAME, AttributeValue.builder().s(name).build())
        AttributeValue.builder().m(hm).build()
      case Inr(r) =>
        val hm = newMap()
        hm.putAll(encodeR.encode(r).m())
        AttributeValue.builder().m(hm).build()
    }

    def project[F, G](instance: => DynamoEncoder[G], to: F => G, from: G => F): DynamoEncoder[F] = value => instance.encode(to(value))
  }
}

trait DynamoEncoderInstances {
  implicit def stringEncoder: DynamoEncoder[String] = value => AttributeValue.builder().s(value).build()

  implicit def numericEncoder[A: Numeric]: DynamoEncoder[A] = value => AttributeValue.builder().n(value.toString).build()

  implicit def booleanEncoder: DynamoEncoder[Boolean] = value => AttributeValue.builder().bool(value).build()

  implicit def instantEncoder: DynamoEncoder[Instant] = value => AttributeValue.builder().n(value.toEpochMilli.toString).build()

  implicit def seqEncoder[A: DynamoEncoder]: DynamoEncoder[Seq[A]] =
    value => AttributeValue.builder().l(value.map(DynamoEncoder[A].encode): _*).build()
}
