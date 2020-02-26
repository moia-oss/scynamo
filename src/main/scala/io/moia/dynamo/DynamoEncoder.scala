package io.moia.dynamo

import java.time.Instant

import shapeless._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait DynamoEncoder[A] { self =>
  def encode(value: A): AttributeValue

  def contramap[B](f: B => A): DynamoEncoder[B] = value => self.encode(f(value))
}

object DynamoEncoder extends DynamoEncoderInstances {
  def apply[A](implicit instance: DynamoEncoder[A]): DynamoEncoder[A] = instance

  implicit def fromObject[A](implicit instance: Lazy[ObjectDynamoEncoder[A]]): DynamoEncoder[A] = instance.value
}

trait DynamoEncoderInstances {
  implicit def stringEncoder: DynamoEncoder[String] = value => AttributeValue.builder().s(value).build()

  implicit def numericEncoder[A: Numeric]: DynamoEncoder[A] = value => AttributeValue.builder().n(value.toString).build()

  implicit def booleanEncoder: DynamoEncoder[Boolean] = value => AttributeValue.builder().bool(value).build()

  implicit def instantEncoder: DynamoEncoder[Instant] = value => AttributeValue.builder().s(value.toString).build()

  implicit def seqEncoder[A: DynamoEncoder]: DynamoEncoder[Seq[A]] =
    value => AttributeValue.builder().l(value.map(DynamoEncoder[A].encode): _*).build()

  implicit def optionEncoder[A: DynamoEncoder]: DynamoEncoder[Option[A]] = {
    case Some(value) => DynamoEncoder[A].encode(value)
    case None        => AttributeValue.builder().nul(true).build()
  }
}

trait ObjectDynamoEncoder[A] extends DynamoEncoder[A] {
  def encodeMap(value: A): java.util.Map[String, AttributeValue]

  override def encode(value: A): AttributeValue = AttributeValue.builder().m(encodeMap(value)).build()
}

object ObjectDynamoEncoder {
  def apply[A](implicit instance: ObjectDynamoEncoder[A]): ObjectDynamoEncoder[A] = instance
}
