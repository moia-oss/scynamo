package scynamo

import cats.data.EitherNec
import cats.syntax.either._
import cats.syntax.parallel._
import scynamo.StackFrame.{Index, MapKey}
import scynamo.generic.auto.AutoDerivationUnlocked
import scynamo.generic.{GenericScynamoEncoder, SemiautoDerivationEncoder}
import shapeless._
import shapeless.tag.@@
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.jdk.CollectionConverters._

trait ScynamoEncoder[A] { self =>
  def encode(value: A): EitherNec[ScynamoEncodeError, AttributeValue]

  def contramap[B](f: B => A): ScynamoEncoder[B] = value => self.encode(f(value))
}

object ScynamoEncoder extends DefaultScynamoEncoderInstances {
  def apply[A](implicit instance: ScynamoEncoder[A]): ScynamoEncoder[A] = instance
}

trait DefaultScynamoEncoderInstances extends ScynamoIterableEncoder {
  implicit val stringEncoder: ScynamoEncoder[String] = value => Right(AttributeValue.builder().s(value).build())

  private[this] val numberStringEncoder: ScynamoEncoder[String] = value => Right(AttributeValue.builder().n(value).build())

  implicit val intEncoder: ScynamoEncoder[Int] = numberStringEncoder.contramap[Int](_.toString)

  implicit val longEncoder: ScynamoEncoder[Long] = numberStringEncoder.contramap[Long](_.toString)

  implicit val bigIntEncoder: ScynamoEncoder[BigInt] = numberStringEncoder.contramap[BigInt](_.toString)

  implicit val floatEncoder: ScynamoEncoder[Float] = numberStringEncoder.contramap[Float](_.toString)

  implicit val doubleEncoder: ScynamoEncoder[Double] = numberStringEncoder.contramap[Double](_.toString)

  implicit val bigDecimalEncoder: ScynamoEncoder[BigDecimal] = numberStringEncoder.contramap[BigDecimal](_.toString)

  implicit val booleanEncoder: ScynamoEncoder[Boolean] = value => Right(AttributeValue.builder().bool(value).build())

  implicit val instantEncoder: ScynamoEncoder[Instant] = numberStringEncoder.contramap[Instant](_.toEpochMilli.toString)

  implicit val instantTtlEncoder: ScynamoEncoder[Instant @@ TimeToLive] =
    numberStringEncoder.contramap[Instant @@ TimeToLive](_.getEpochSecond.toString)

  implicit val uuidEncoder: ScynamoEncoder[UUID] = stringEncoder.contramap[UUID](_.toString)

  implicit def seqEncoder[A: ScynamoEncoder]: ScynamoEncoder[scala.collection.immutable.Seq[A]] =
    value => value.toVector.parTraverse(ScynamoEncoder[A].encode).map(xs => AttributeValue.builder().l(xs: _*).build())

  implicit def listEncoder[A: ScynamoEncoder]: ScynamoEncoder[List[A]] =
    value =>
      value.zipWithIndex
        .parTraverse { case (x, i) =>
          ScynamoEncoder[A].encode(x).leftMap(_.map(_.push(Index(i))))
        }
        .map(xs => AttributeValue.builder().l(xs: _*).build())

  implicit def vectorEncoder[A: ScynamoEncoder]: ScynamoEncoder[Vector[A]] =
    value =>
      value.zipWithIndex
        .parTraverse { case (x, i) =>
          ScynamoEncoder[A].encode(x).leftMap(_.map(_.push(Index(i))))
        }
        .map(xs => AttributeValue.builder().l(xs: _*).build())

  implicit def setEncoder[A: ScynamoEncoder]: ScynamoEncoder[Set[A]] = listEncoder[A].contramap[Set[A]](x => x.toList)

  implicit def optionEncoder[A: ScynamoEncoder]: ScynamoEncoder[Option[A]] = {
    case Some(value) => ScynamoEncoder[A].encode(value)
    case None        => Right(AttributeValue.builder().nul(true).build())
  }

  implicit def someEncoder[A: ScynamoEncoder]: ScynamoEncoder[Some[A]] = x => ScynamoEncoder[A].encode(x.get)

  implicit val finiteDurationEncoder: ScynamoEncoder[FiniteDuration] = longEncoder.contramap(_.toNanos)

  implicit val durationEncoder: ScynamoEncoder[Duration] = longEncoder.contramap(_.toNanos)

  implicit def mapEncoder[A, B](implicit keyEncoder: ScynamoKeyEncoder[A], valueEncoder: ScynamoEncoder[B]): ScynamoEncoder[Map[A, B]] =
    value => {
      value.toVector
        .parTraverse { case (k, v) =>
          (keyEncoder.encode(k), valueEncoder.encode(v)).parMapN(_ -> _).leftMap(_.map(_.push(MapKey(k))))
        }
        .map {
          _.foldLeft(new java.util.HashMap[String, AttributeValue]()) { case (acc, (k, v)) =>
            acc.put(k, v)
            acc
          }
        }
        .map(hm => AttributeValue.builder().m(hm).build())
    }

  implicit val attributeValueEncoder: ScynamoEncoder[AttributeValue] = { value =>
    import scynamo.syntax.attributevalue._

    val nonEmptyStringSet = value.asOption(ScynamoType.StringSet).map(x => ScynamoType.StringSet -> (x.size() > 0))
    val nonEmptyNumberSet = value.asOption(ScynamoType.NumberSet).map(x => ScynamoType.NumberSet -> (x.size() > 0))
    val nonEmptyBinarySet = value.asOption(ScynamoType.BinarySet).map(x => ScynamoType.BinarySet -> (x.size() > 0))

    nonEmptyStringSet.orElse(nonEmptyNumberSet).orElse(nonEmptyBinarySet) match {
      case Some((typ, false))     => Either.leftNec(ScynamoEncodeError.invalidEmptyValue(typ))
      case Some((_, true)) | None => Right(value)
    }
  }

  implicit def eitherScynamoErrorEncoder[A: ScynamoEncoder]: ScynamoEncoder[EitherNec[ScynamoEncodeError, A]] = {
    case Left(value)  => Left(value)
    case Right(value) => ScynamoEncoder[A].encode(value)
  }
}

trait ScynamoIterableEncoder extends LowestPrioAutoEncoder {
  def iterableEncoder[A: ScynamoEncoder]: ScynamoEncoder[Iterable[A]] =
    value =>
      value.toList.parTraverse(ScynamoEncoder[A].encode).map(encodedValues => AttributeValue.builder().l(encodedValues.asJava).build())
}

trait LowestPrioAutoEncoder {
  final implicit def autoDerivedScynamoEncoder[A: AutoDerivationUnlocked](implicit
      genericEncoder: Lazy[GenericScynamoEncoder[A]]
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
          _.foldLeft(new java.util.HashMap[String, AttributeValue]()) { case (acc, (k, v)) =>
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
    if (value.nonEmpty)
      Right(value)
    else
      Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.String))

  implicit val uuidKeyEncoder: ScynamoKeyEncoder[UUID] = ScynamoKeyEncoder[String].contramap[UUID](_.toString)
}
