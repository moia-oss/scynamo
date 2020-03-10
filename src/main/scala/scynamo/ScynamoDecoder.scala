package scynamo

import java.time.Instant
import java.util.UUID
import java.util.concurrent.TimeUnit

import cats.data.{EitherNec, NonEmptyChain}
import cats.instances.either._
import cats.instances.vector._
import cats.kernel.Eq
import cats.syntax.either._
import cats.syntax.parallel._
import cats.{Functor, SemigroupK, Show}
import scynamo.ScynamoDecodeError._
import scynamo.generic.auto.AutoDerivationUnlocked
import scynamo.generic.{GenericScynamoDecoder, SemiautoDerivationDecoder}
import shapeless.Lazy
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.collection.compat._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

sealed abstract class ScynamoDecodeError extends Product with Serializable

object ScynamoDecodeError {
  case class MissingField(fieldName: String, hmap: java.util.Map[String, AttributeValue]) extends ScynamoDecodeError
  case class TypeMismatch(expected: ScynamoType, attributeValue: AttributeValue)          extends ScynamoDecodeError
  case class InvalidCoproductCase(hmap: java.util.Map[String, AttributeValue])            extends ScynamoDecodeError
  case class ConversionError(message: String, cause: Option[Throwable])                   extends ScynamoDecodeError
  case class GeneralError(message: String, cause: Option[Throwable])                      extends ScynamoDecodeError

  implicit val scynamoDecodeErrorEq: Eq[ScynamoDecodeError] = Eq.fromUniversalEquals[ScynamoDecodeError]

  implicit val scynamoDecodeErrorShow: Show[ScynamoDecodeError] = {
    case MissingField(fieldName, hmap)          => s"Could not find field '$fieldName' inside $hmap'"
    case TypeMismatch(expected, attributeValue) => s"Type mismatch, expected type $expected, given: $attributeValue"
    case InvalidCoproductCase(hmap)             => s"Could not decode into one of the sealed traits cases: $hmap"
    case ConversionError(message, cause)        => s"Error during conversion: $message${cause.fold("")(e => s" with cause: ${e.getMessage}")}"
    case GeneralError(message, cause)           => s"General decoder error: $message${cause.fold("")(e => s" with cause: ${e.getMessage}")}"
  }
}

trait ScynamoDecoder[A] extends ScynamoDecoderFunctions { self =>
  def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, A]

  def map[B](f: A => B): ScynamoDecoder[B] = value => self.decode(value).map(f)

  def orElse[AA >: A](other: ScynamoDecoder[A]): ScynamoDecoder[AA] =
    (attributeValue: AttributeValue) => self.decode(attributeValue).orElse(other.decode(attributeValue))

  def transform[B](f: EitherNec[ScynamoDecodeError, A] => EitherNec[ScynamoDecodeError, B]): ScynamoDecoder[B] =
    attributeValue => f(self.decode(attributeValue))

  def defaultValue: Option[A] = None
}

object ScynamoDecoder extends DefaultScynamoDecoderInstances {
  def apply[A](implicit instance: ScynamoDecoder[A]): ScynamoDecoder[A] = instance
}

trait DefaultScynamoDecoderInstances extends ScynamoDecoderFunctions with ScynamoIterableDecoder {
  import scynamo.syntax.attributevalue._
  implicit val catsInstances: Functor[ScynamoDecoder] with SemigroupK[ScynamoDecoder] =
    new Functor[ScynamoDecoder] with SemigroupK[ScynamoDecoder] {
      override def map[A, B](fa: ScynamoDecoder[A])(f: A => B): ScynamoDecoder[B] = fa.map(f)

      override def combineK[A](x: ScynamoDecoder[A], y: ScynamoDecoder[A]): ScynamoDecoder[A] = x.orElse(y)
    }

  implicit val stringDecoder: ScynamoDecoder[String] = attributeValue => attributeValue.asEither(ScynamoType.String)

  implicit val intDecoder: ScynamoDecoder[Int] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s)(_.toInt))

  implicit val longDecoder: ScynamoDecoder[Long] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s)(_.toLong))

  implicit val bigIntDecoder: ScynamoDecoder[BigInt] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s)(BigInt(_)))

  implicit val floatDecoder: ScynamoDecoder[Float] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s)(_.toFloat))

  implicit val doubleDecoder: ScynamoDecoder[Double] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s)(_.toDouble))

  implicit val bigDecimalDecoder: ScynamoDecoder[BigDecimal] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s)(BigDecimal(_)))

  implicit val booleanDecoder: ScynamoDecoder[Boolean] =
    attributeValue => attributeValue.asEither(ScynamoType.Bool)

  implicit val instantDecoder: ScynamoDecoder[Instant] =
    attributeValue =>
      for {
        nstring <- attributeValue.asEither(ScynamoType.Number)
        result  <- convert(nstring)(_.toLong)
      } yield Instant.ofEpochMilli(result)

  implicit def seqDecoder[A: ScynamoDecoder]: ScynamoDecoder[scala.collection.immutable.Seq[A]] = iterableDecoder

  implicit def listDecoder[A: ScynamoDecoder]: ScynamoDecoder[List[A]] = iterableDecoder

  implicit def vectorDecoder[A: ScynamoDecoder]: ScynamoDecoder[Vector[A]] = iterableDecoder

  implicit def setDecoder[A: ScynamoDecoder]: ScynamoDecoder[Set[A]] = iterableDecoder

  implicit def optionDecoder[A: ScynamoDecoder]: ScynamoDecoder[Option[A]] = new ScynamoDecoder[Option[A]] {
    override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, Option[A]] =
      if (attributeValue.nul()) Right(None) else ScynamoDecoder[A].decode(attributeValue).map(Some(_))

    override def defaultValue: Option[Option[A]] = Some(None)
  }

  implicit val finiteDurationDecoder: ScynamoDecoder[FiniteDuration] = longDecoder.map(Duration.fromNanos)

  implicit val durationDecoder: ScynamoDecoder[Duration] = longDecoder.map(n => Duration(n, TimeUnit.NANOSECONDS))

  implicit val uuidDecoder: ScynamoDecoder[UUID] = attributeValue =>
    attributeValue.asEither(ScynamoType.String).flatMap(s => convert(s)(UUID.fromString))

  implicit def mapDecoder[A, B](
      implicit keyDecoder: ScynamoKeyDecoder[A],
      valueDecoder: ScynamoDecoder[B]
  ): ScynamoDecoder[Map[A, B]] =
    attributeValue =>
      attributeValue.asEither(ScynamoType.Map).flatMap { javaMap =>
        javaMap.asScala.toVector
          .parTraverse {
            case (key, value) =>
              (keyDecoder.decode(key), valueDecoder.decode(value)).parMapN(_ -> _)
          }
          .map(_.toMap)
    }

  implicit val attributeValueDecoder: ScynamoDecoder[AttributeValue] = attributeValue => Right(attributeValue)
}

trait ScynamoIterableDecoder extends LowestPrioAutoDecoder {
  import scynamo.syntax.attributevalue._
  def iterableDecoder[A: ScynamoDecoder, C[_] <: Iterable[A], X](implicit factory: Factory[A, C[A]]): ScynamoDecoder[C[A]] =
    attributeValue =>
      attributeValue.asOption(ScynamoType.List) match {
        case Some(theList) =>
          val builder = factory.newBuilder
          var elems   = Either.rightNec[ScynamoDecodeError, builder.type](builder)

          theList.forEach { elem =>
            val decoded = ScynamoDecoder[A].decode(elem)
            elems = (elems, decoded).parMapN((builder, dec) => builder += dec)
          }

          elems.map(_.result())
        case None => Either.leftNec(TypeMismatch(ScynamoType.List, attributeValue))
    }
}

trait LowestPrioAutoDecoder {
  final implicit def autoDerivedScynamoDecoder[A: AutoDerivationUnlocked](
      implicit genericDecoder: Lazy[GenericScynamoDecoder[A]]
  ): ObjectScynamoDecoder[A] =
    scynamo.generic.semiauto.deriveScynamoDecoder[A]
}

object ScynamoDecoderFunctions extends ScynamoDecoderFunctions

trait ScynamoDecoderFunctions {
  def convert[A, B](s: A)(convertor: A => B): EitherNec[ScynamoDecodeError, B] =
    try {
      Right(convertor(s))
    } catch {
      case NonFatal(e) => Either.leftNec(ConversionError(s"Could not convert: ${e.getMessage}", Some(e)))
    }
}

trait ObjectScynamoDecoder[A] extends ScynamoDecoder[A] {
  import scynamo.syntax.attributevalue._
  override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, A] =
    attributeValue.asOption(ScynamoType.Map) match {
      case Some(value) => decodeMap(value)
      case None        => Either.leftNec(TypeMismatch(ScynamoType.Map, attributeValue))
    }

  def decodeMap(value: java.util.Map[String, AttributeValue]): EitherNec[ScynamoDecodeError, A]
}

object ObjectScynamoDecoder extends ScynamoDecoderFunctions with SemiautoDerivationDecoder {

  def apply[A](implicit instance: ObjectScynamoDecoder[A]): ObjectScynamoDecoder[A] = instance

  implicit def mapDecoder[A](implicit valueDecoder: ScynamoDecoder[A]): ObjectScynamoDecoder[Map[String, A]] =
    javaMap => javaMap.asScala.toVector.parTraverse { case (key, value) => valueDecoder.decode(value).map(key -> _) }.map(_.toMap)
}

trait ScynamoKeyDecoder[A] {
  def decode(value: String): EitherNec[ScynamoDecodeError, A]
}

object ScynamoKeyDecoder {
  def apply[A](implicit decoder: ScynamoKeyDecoder[A]): ScynamoKeyDecoder[A] = decoder

  implicit val stringKeyDecoder: ScynamoKeyDecoder[String] = s => Right(s)

  implicit val uuidKeyDecoder: ScynamoKeyDecoder[UUID] = s => {
    val result = Either.catchOnly[IllegalArgumentException](UUID.fromString(s))

    result.leftMap(e => NonEmptyChain.one(ScynamoDecodeError.ConversionError(s"Could not convert to UUID: $s", Some(e))))
  }
}
