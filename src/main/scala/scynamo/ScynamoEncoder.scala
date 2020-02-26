package scynamo

import java.time.Instant

import shapeless._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait ScynamoEncoder[A] { self =>
  def encode(value: A): AttributeValue

  def contramap[B](f: B => A): ScynamoEncoder[B] = value => self.encode(f(value))
}

object ScynamoEncoder extends ScynamoEncoderInstances {
  def apply[A](implicit instance: ScynamoEncoder[A]): ScynamoEncoder[A] = instance

  implicit def fromObject[A](implicit instance: Lazy[ObjectScynamoEncoder[A]]): ScynamoEncoder[A] = instance.value
}

trait ScynamoEncoderInstances {
  implicit def stringEncoder: ScynamoEncoder[String] = value => AttributeValue.builder().s(value).build()

  implicit def numericEncoder[A: Numeric]: ScynamoEncoder[A] = value => AttributeValue.builder().n(value.toString).build()

  implicit def booleanEncoder: ScynamoEncoder[Boolean] = value => AttributeValue.builder().bool(value).build()

  implicit def instantEncoder: ScynamoEncoder[Instant] = value => AttributeValue.builder().n(value.toEpochMilli.toString).build()

  implicit def seqEncoder[A: ScynamoEncoder]: ScynamoEncoder[Seq[A]] =
    value => AttributeValue.builder().l(value.map(ScynamoEncoder[A].encode): _*).build()

  implicit def optionEncoder[A: ScynamoEncoder]: ScynamoEncoder[Option[A]] = {
    case Some(value) => ScynamoEncoder[A].encode(value)
    case None        => AttributeValue.builder().nul(true).build()
  }
}

trait ObjectScynamoEncoder[A] extends ScynamoEncoder[A] {
  def encodeMap(value: A): java.util.Map[String, AttributeValue]

  override def encode(value: A): AttributeValue = AttributeValue.builder().m(encodeMap(value)).build()
}

object ObjectScynamoEncoder {
  def apply[A](implicit instance: ObjectScynamoEncoder[A]): ObjectScynamoEncoder[A] = instance
}
