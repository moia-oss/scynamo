package scynamo

import java.time.Instant
import java.util.UUID

import cats.data.EitherNec
import cats.instances.either._
import cats.instances.list._
import cats.instances.vector._
import cats.syntax.either._
import cats.syntax.parallel._
import scynamo.generic.auto.AutoDerivationUnlocked
import scynamo.generic.{GenericScynamoEncoder, SemiautoDerivationEncoder}
import shapeless._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.jdk.CollectionConverters._

trait ScynamoEncoder[A] { self =>
  def encode(value: A): EitherNec[ScynamoEncodeError, AttributeValue]

  def contramap[B](f: B => A): ScynamoEncoder[B] = value => self.encode(f(value))
}

object ScynamoEncoder extends DefaultScynamoEncoderInstances {
  def apply[A](implicit instance: ScynamoEncoder[A]): ScynamoEncoder[A] = instance
}

trait DefaultScynamoEncoderInstances extends FailingScynamoEncoderInstances {
  implicit val intEncoder: ScynamoEncoder[Int] = value => Right(AttributeValue.builder().n(value.toString).build())

  implicit val longEncoder: ScynamoEncoder[Long] = value => Right(AttributeValue.builder().n(value.toString).build())

  implicit val bigIntEncoder: ScynamoEncoder[BigInt] = value => Right(AttributeValue.builder().n(value.toString).build())

  implicit val floatEncoder: ScynamoEncoder[Float] = value => Right(AttributeValue.builder().n(value.toString).build())

  implicit val doubleEncoder: ScynamoEncoder[Double] = value => Right(AttributeValue.builder().n(value.toString).build())

  implicit val bigDecimalEncoder: ScynamoEncoder[BigDecimal] = value => Right(AttributeValue.builder().n(value.toString).build())

  implicit val booleanEncoder: ScynamoEncoder[Boolean] = value => Right(AttributeValue.builder().bool(value).build())

  implicit val instantEncoder: ScynamoEncoder[Instant] = value => Right(AttributeValue.builder().n(value.toEpochMilli.toString).build())

  implicit val uuidEncoder: ScynamoEncoder[UUID] = value => Right(AttributeValue.builder().s(value.toString).build())

  implicit def seqEncoder[A: ScynamoEncoder]: ScynamoEncoder[scala.collection.immutable.Seq[A]] =
    value => value.toVector.parTraverse(ScynamoEncoder[A].encode).map(xs => AttributeValue.builder().l(xs: _*).build())

  implicit def listEncoder[A: ScynamoEncoder]: ScynamoEncoder[List[A]] =
    value => value.parTraverse(ScynamoEncoder[A].encode).map(xs => AttributeValue.builder().l(xs: _*).build())

  implicit def vectorEncoder[A: ScynamoEncoder]: ScynamoEncoder[Vector[A]] =
    value => value.parTraverse(ScynamoEncoder[A].encode).map(xs => AttributeValue.builder().l(xs: _*).build())

  implicit def setEncoder[A: ScynamoEncoder]: ScynamoEncoder[Set[A]] =
    value => value.toList.parTraverse(ScynamoEncoder[A].encode).map(xs => AttributeValue.builder().l(xs: _*).build())

  implicit def optionEncoder[A: ScynamoEncoder]: ScynamoEncoder[Option[A]] = {
    case Some(value) => ScynamoEncoder[A].encode(value)
    case None        => Right(AttributeValue.builder().nul(true).build())
  }

  implicit val finiteDurationEncoder: ScynamoEncoder[FiniteDuration] = longEncoder.contramap(_.toNanos)

  implicit val durationEncoder: ScynamoEncoder[Duration] = longEncoder.contramap(_.toNanos)

  implicit def mapEncoder[A, B](implicit keyEncoder: ScynamoKeyEncoder[A], valueEncoder: ScynamoEncoder[B]): ScynamoEncoder[Map[A, B]] =
    value => {
      value.toVector
        .parTraverse {
          case (k, v) => (keyEncoder.encode(k), valueEncoder.encode(v)).parMapN(_ -> _)
        }
        .map {
          _.foldLeft(new java.util.HashMap[String, AttributeValue]()) {
            case (acc, (k, v)) =>
              acc.put(k, v)
              acc
          }
        }
        .map(hm => AttributeValue.builder().m(hm).build())
    }

  implicit val attributeValueEncoder: ScynamoEncoder[AttributeValue] = value => Right(value)

  implicit def eitherScynamoErrorEncoder[A: ScynamoEncoder]: ScynamoEncoder[EitherNec[ScynamoEncodeError, A]] = {
    case Left(value)  => Left(value)
    case Right(value) => ScynamoEncoder[A].encode(value)
  }
}

/**
  * Instances that can fail during encoding due to DynamoDB limitations
  * @see https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html
  */
trait FailingScynamoEncoderInstances extends ScynamoIterableEncoder {
  implicit val stringEncoder: ScynamoEncoder[String] = value => {
    if (value.nonEmpty) {
      Right(AttributeValue.builder().s(value).build())
    } else {
      Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.String))
    }
  }
}

trait ScynamoIterableEncoder extends LowestPrioAutoEncoder {
  def iterableEncoder[A: ScynamoEncoder]: ScynamoEncoder[Iterable[A]] =
    value =>
      value.toList.parTraverse(ScynamoEncoder[A].encode).map(encodedValues => AttributeValue.builder().l(encodedValues.asJava).build())
}

trait LowestPrioAutoEncoder {
  final implicit def autoDerivedScynamoEncoder[A: AutoDerivationUnlocked](
      implicit genericEncoder: Lazy[GenericScynamoEncoder[A]]
  ): ObjectScynamoEncoder[A] =
    scynamo.generic.semiauto.deriveScynamoEncoder[A]
}

trait ObjectScynamoEncoder[A] extends ScynamoEncoder[A] {
  def encodeMap(value: A): EitherNec[ScynamoEncodeError, java.util.Map[String, AttributeValue]]

  override def encode(value: A): EitherNec[ScynamoEncodeError, AttributeValue] =
    encodeMap(value).map(AttributeValue.builder().m(_).build())
}

object ObjectScynamoEncoder extends SemiautoDerivationEncoder {
  def apply[A](implicit instance: ObjectScynamoEncoder[A]): ObjectScynamoEncoder[A] = instance

  implicit def mapEncoder[A](implicit valueEncoder: ScynamoEncoder[A]): ObjectScynamoEncoder[Map[String, A]] =
    value => {
      value.toList
        .parTraverse { case (k, v) => valueEncoder.encode(v).map(k -> _) }
        .map {
          _.foldLeft(new java.util.HashMap[String, AttributeValue]()) {
            case (acc, (k, v)) =>
              acc.put(k, v)
              acc
          }
        }
    }
}

trait ScynamoKeyEncoder[A] { self =>
  def encode(value: A): EitherNec[ScynamoEncodeError, String]

  def contramap[B](f: B => A): ScynamoKeyEncoder[B] = value => self.encode(f(value))
}

object ScynamoKeyEncoder {
  def apply[A](implicit encoder: ScynamoKeyEncoder[A]): ScynamoKeyEncoder[A] = encoder

  implicit val stringKeyEncoder: ScynamoKeyEncoder[String] = value =>
    if (value.nonEmpty) {
      Right(value)
    } else {
      Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.String))
    }

  implicit val uuidKeyEncoder: ScynamoKeyEncoder[UUID] = ScynamoKeyEncoder[String].contramap[UUID](_.toString)
}
