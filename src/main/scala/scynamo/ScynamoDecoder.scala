package scynamo

import java.time.Instant
import java.util.UUID

import cats.data.{EitherNec, NonEmptyChain}
import cats.instances.either._
import cats.instances.vector._
import cats.kernel.Eq
import cats.syntax.either._
import cats.syntax.parallel._
import cats.{Functor, SemigroupK}
import scynamo.ScynamoType._
import scynamo.generic.GenericScynamoDecoder
import scynamo.generic.auto.AutoDerivationUnlocked
import shapeless.Lazy
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

abstract class ScynamoDecodeError                                                            extends Product with Serializable
case class MissingField(fieldName: String, attributeValue: AttributeValue)                   extends ScynamoDecodeError
case class MissingFieldInMap(fieldName: String, hmap: java.util.Map[String, AttributeValue]) extends ScynamoDecodeError
case class TypeMismatch(expected: ScynamoType, attributeValue: AttributeValue)               extends ScynamoDecodeError
case class InvalidCase(hmap: java.util.Map[String, AttributeValue])                          extends ScynamoDecodeError
case class ParseError(message: String, cause: Option[Throwable])                             extends ScynamoDecodeError
case class InvalidTypeTag(attributeValue: AttributeValue)                                    extends ScynamoDecodeError

object ScynamoDecodeError {
  implicit val scynamoDecodeErrorEq: Eq[ScynamoDecodeError] = Eq.fromUniversalEquals[ScynamoDecodeError]
}

trait ScynamoDecoder[A] extends ScynamoDecoderFunctions { self =>
  def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, A]

  def map[B](f: A => B): ScynamoDecoder[B] = value => self.decode(value).map(f)

  def orElse[AA >: A](other: ScynamoDecoder[A]): ScynamoDecoder[AA] =
    (attributeValue: AttributeValue) => self.decode(attributeValue).orElse(other.decode(attributeValue))
}

object ScynamoDecoder extends DefaultScynamoDecoderInstances0 {
  def apply[A](implicit instance: ScynamoDecoder[A]): ScynamoDecoder[A] = instance
}

trait DefaultScynamoDecoderInstances0 extends LowPrioAutoDecoder1 with ScynamoDecoderFunctions {
  implicit val catsInstances: Functor[ScynamoDecoder] with SemigroupK[ScynamoDecoder] =
    new Functor[ScynamoDecoder] with SemigroupK[ScynamoDecoder] {
      override def map[A, B](fa: ScynamoDecoder[A])(f: A => B): ScynamoDecoder[B] = fa.map(f)

      override def combineK[A](x: ScynamoDecoder[A], y: ScynamoDecoder[A]): ScynamoDecoder[A] = x.orElse(y)
    }

  import scynamo.attributevalue.dsl._

  implicit val stringDecoder: ScynamoDecoder[String] = attributeValue => accessOrTypeMismatch(attributeValue, ScynamoString)(_.sOpt)

  implicit val intDecoder: ScynamoDecoder[Int] =
    attributeValue => accessOrTypeMismatch(attributeValue, ScynamoNumber)(_.nOpt).flatMap(s => convert(s)(_.toInt))

  implicit val longDecoder: ScynamoDecoder[Long] =
    attributeValue => accessOrTypeMismatch(attributeValue, ScynamoNumber)(_.nOpt).flatMap(s => convert(s)(_.toLong))

  implicit val bigIntDecoder: ScynamoDecoder[BigInt] =
    attributeValue => accessOrTypeMismatch(attributeValue, ScynamoNumber)(_.nOpt).flatMap(s => convert(s)(BigInt(_)))

  implicit val floatDecoder: ScynamoDecoder[Float] =
    attributeValue => accessOrTypeMismatch(attributeValue, ScynamoNumber)(_.nOpt).flatMap(s => convert(s)(_.toFloat))

  implicit val doubleDecoder: ScynamoDecoder[Double] =
    attributeValue => accessOrTypeMismatch(attributeValue, ScynamoNumber)(_.nOpt).flatMap(s => convert(s)(_.toDouble))

  implicit val bigDecimalDecoder: ScynamoDecoder[BigDecimal] =
    attributeValue => accessOrTypeMismatch(attributeValue, ScynamoNumber)(_.nOpt).flatMap(s => convert(s)(BigDecimal(_)))

  implicit val booleanDecoder: ScynamoDecoder[Boolean] =
    attributeValue => accessOrTypeMismatch(attributeValue, ScynamoBool)(_.boolOpt)

  implicit val instantDecoder: ScynamoDecoder[Instant] =
    attributeValue =>
      for {
        nstring <- accessOrTypeMismatch(attributeValue, ScynamoNumber)(_.nOpt)
        result  <- convert(nstring)(_.toLong)
      } yield Instant.ofEpochMilli(result)

  implicit def seqDecoder[A: ScynamoDecoder]: ScynamoDecoder[Seq[A]] =
    attributeValue =>
      for {
        list   <- accessOrTypeMismatch(attributeValue, ScynamoList)(_.lOpt)
        result <- list.iterator.asScala.toVector.parTraverse(ScynamoDecoder[A].decode)
      } yield result

  implicit def optionDecoder[A: ScynamoDecoder]: ScynamoDecoder[Option[A]] =
    attributeValue => if (attributeValue.nul()) Right(None) else ScynamoDecoder[A].decode(attributeValue).map(Some(_))

  implicit val finiteDurationDecoder: ScynamoDecoder[FiniteDuration] = longDecoder.map(Duration.fromNanos)

  implicit val durationDecoder: ScynamoDecoder[Duration] = {
    import scala.concurrent.duration._
    longDecoder.map(_.nanos)
  }

  implicit val uuidDecoder: ScynamoDecoder[UUID] = attributeValue =>
    accessOrTypeMismatch(attributeValue, ScynamoString)(_.sOpt).flatMap(s => convert(s)(UUID.fromString))
}

trait LowPrioAutoDecoder1 {
  final implicit def autoDerivedScynamoDecoder[A: AutoDerivationUnlocked](
      implicit genericDecoder: Lazy[GenericScynamoDecoder[A]]
  ): ObjectScynamoDecoder[A] =
    scynamo.generic.semiauto.deriveScynamoDecoder[A]
}

object ScynamoDecoderFunctions extends ScynamoDecoderFunctions

trait ScynamoDecoderFunctions {
  def accessOrTypeMismatch[A](attributeValue: AttributeValue, typ: ScynamoType)(
      access: AttributeValue => Option[A]
  ): Either[NonEmptyChain[TypeMismatch], A] =
    access(attributeValue) match {
      case None        => Either.leftNec(TypeMismatch(typ, attributeValue))
      case Some(value) => Right(value)
    }

  def convert[A, B](s: A)(convertor: A => B): EitherNec[ScynamoDecodeError, B] =
    try {
      Right(convertor(s))
    } catch {
      case NonFatal(e) => Either.leftNec(ParseError(s"Could not convert: ${e.getMessage}", Some(e)))
    }
}

trait ObjectScynamoDecoder[A] extends ScynamoDecoder[A] {
  import scynamo.attributevalue.dsl._
  override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, A] = attributeValue.mOpt match {
    case Some(value) => decodeMap(value)
    case None        => Either.leftNec(TypeMismatch(ScynamoMap, attributeValue))
  }

  def decodeMap(value: java.util.Map[String, AttributeValue]): EitherNec[ScynamoDecodeError, A]
}

object ObjectScynamoDecoder {
  def apply[A](implicit instance: ObjectScynamoDecoder[A]): ObjectScynamoDecoder[A] = instance
}
