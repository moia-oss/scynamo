package scynamo

import java.time.Instant
import java.util.UUID

import shapeless._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.concurrent.duration.FiniteDuration

trait ScynamoEncoder[A] { self =>
  def encode(value: A): AttributeValue

  def contramap[B](f: B => A): ScynamoEncoder[B] = value => self.encode(f(value))
}

object ScynamoEncoder extends ScynamoEncoderInstances {
  def apply[A](implicit instance: ScynamoEncoder[A]): ScynamoEncoder[A] = instance

  implicit def fromObject[A](implicit instance: Lazy[ObjectScynamoEncoder[A]]): ScynamoEncoder[A] = instance.value
}

trait ScynamoEncoderInstances {
  implicit val stringEncoder: ScynamoEncoder[String] = value => AttributeValue.builder().s(value).build()

  implicit val intEncoder: ScynamoEncoder[Int] = value => AttributeValue.builder().n(value.toString).build()

  implicit val longEncoder: ScynamoEncoder[Long] = value => AttributeValue.builder().n(value.toString).build()

  implicit val bigIntEncoder: ScynamoEncoder[BigInt] = value => AttributeValue.builder().n(value.toString).build()

  implicit val floatEncoder: ScynamoEncoder[Float] = value => AttributeValue.builder().n(value.toString).build()

  implicit val doubleEncoder: ScynamoEncoder[Double] = value => AttributeValue.builder().n(value.toString).build()

  implicit val bigDecimalEncoder: ScynamoEncoder[BigDecimal] = value => AttributeValue.builder().n(value.toString).build()

  implicit val booleanEncoder: ScynamoEncoder[Boolean] = value => AttributeValue.builder().bool(value).build()

  implicit val instantEncoder: ScynamoEncoder[Instant] = value => AttributeValue.builder().n(value.toEpochMilli.toString).build()

  implicit val uuidEncoder: ScynamoEncoder[UUID] = value => AttributeValue.builder().s(value.toString).build()

  implicit def seqEncoder[A: ScynamoEncoder]: ScynamoEncoder[Seq[A]] =
    value => AttributeValue.builder().l(value.map(ScynamoEncoder[A].encode): _*).build()

  implicit def optionEncoder[A: ScynamoEncoder]: ScynamoEncoder[Option[A]] = {
    case Some(value) => ScynamoEncoder[A].encode(value)
    case None        => AttributeValue.builder().nul(true).build()
  }

  implicit val durationEncoder: ScynamoEncoder[FiniteDuration] = longEncoder.contramap(_.toNanos)
}

trait ObjectScynamoEncoder[A] extends ScynamoEncoder[A] {
  def encodeMap(value: A): java.util.Map[String, AttributeValue]

  override def encode(value: A): AttributeValue = AttributeValue.builder().m(encodeMap(value)).build()
}

object ObjectScynamoEncoder {
  def apply[A](implicit instance: ObjectScynamoEncoder[A]): ObjectScynamoEncoder[A] = instance
}
