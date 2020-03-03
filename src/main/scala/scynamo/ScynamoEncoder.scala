package scynamo

import java.time.Instant
import java.util.UUID

import scala.jdk.CollectionConverters._
import scynamo.generic.GenericScynamoEncoder
import scynamo.generic.auto.AutoDerivationUnlocked
import shapeless._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.concurrent.duration.{Duration, FiniteDuration}

trait ScynamoEncoder[A] { self =>
  def encode(value: A): AttributeValue

  def contramap[B](f: B => A): ScynamoEncoder[B] = value => self.encode(f(value))
}

object ScynamoEncoder extends DefaultScynamoEncoderInstances {
  def apply[A](implicit instance: ScynamoEncoder[A]): ScynamoEncoder[A] = instance
}

trait DefaultScynamoEncoderInstances extends ScynamoIterableEncoder {
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

  implicit def seqEncoder[A: ScynamoEncoder]: ScynamoEncoder[scala.collection.immutable.Seq[A]] =
    value => AttributeValue.builder().l(value.map(ScynamoEncoder[A].encode): _*).build()

  implicit def listEncoder[A: ScynamoEncoder]: ScynamoEncoder[List[A]] =
    value => AttributeValue.builder().l(value.map(ScynamoEncoder[A].encode): _*).build()

  implicit def vectorEncoder[A: ScynamoEncoder]: ScynamoEncoder[Vector[A]] =
    value => AttributeValue.builder().l(value.map(ScynamoEncoder[A].encode): _*).build()

  implicit def setEncoder[A: ScynamoEncoder]: ScynamoEncoder[Set[A]] =
    value => AttributeValue.builder().l(value.map(ScynamoEncoder[A].encode).toList: _*).build()

  implicit def optionEncoder[A: ScynamoEncoder]: ScynamoEncoder[Option[A]] = {
    case Some(value) => ScynamoEncoder[A].encode(value)
    case None        => AttributeValue.builder().nul(true).build()
  }

  implicit val finiteDurationEncoder: ScynamoEncoder[FiniteDuration] = longEncoder.contramap(_.toNanos)

  implicit val durationEncoder: ScynamoEncoder[Duration] = longEncoder.contramap(_.toNanos)

  implicit def mapEncoder[A](implicit valueEncoder: ScynamoEncoder[A]): ScynamoEncoder[Map[String, A]] =
    value => {
      val hm = value.toVector.map { case (k, v) => k -> valueEncoder.encode(v) }.foldLeft(new java.util.HashMap[String, AttributeValue]()) {
        case (acc, (k, v)) =>
          acc.put(k, v)
          acc
      }
      AttributeValue.builder().m(hm).build()
    }
}

trait ScynamoIterableEncoder extends LowestPrioAutoEncoder {
  def iterableEncoder[A: ScynamoEncoder]: ScynamoEncoder[Iterable[A]] =
    value => {
      val encodedValues = value.map(ScynamoEncoder[A].encode)
      AttributeValue.builder().l(encodedValues.toList.asJava).build()
    }
}

trait LowestPrioAutoEncoder {
  final implicit def autoDerivedScynamoEncoder[A: AutoDerivationUnlocked](
      implicit genericEncoder: Lazy[GenericScynamoEncoder[A]]
  ): ObjectScynamoEncoder[A] =
    scynamo.generic.semiauto.deriveScynamoEncoder[A]
}

trait ObjectScynamoEncoder[A] extends ScynamoEncoder[A] {
  def encodeMap(value: A): java.util.Map[String, AttributeValue]

  override def encode(value: A): AttributeValue = AttributeValue.builder().m(encodeMap(value)).build()
}

object ObjectScynamoEncoder {
  def apply[A](implicit instance: ObjectScynamoEncoder[A]): ObjectScynamoEncoder[A] = instance
}
