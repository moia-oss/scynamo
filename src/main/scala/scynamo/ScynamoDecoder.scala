package scynamo

import cats.data.{Chain, EitherNec, NonEmptyChain}
import cats.syntax.either._
import cats.{Monad, SemigroupK}
import scynamo.StackFrame.{Index, MapKey}
import scynamo.generic.auto.AutoDerivationUnlocked
import scynamo.generic.{GenericScynamoDecoder, SemiautoDerivationDecoder}
import scynamo.syntax.attributevalue._
import shapeless.labelled.{field, FieldType}
import shapeless.tag.@@
import shapeless.{tag, Lazy}
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import java.time.format.DateTimeFormatter
import java.time.{Instant, YearMonth}
import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.collection.compat._
import scala.collection.immutable.Seq
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.control.NonFatal

trait ScynamoDecoder[A] extends ScynamoDecoderFunctions { self =>
  def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, A]

  def map[B](f: A => B): ScynamoDecoder[B] =
    ScynamoDecoder.instance(decode(_).map(f))

  def flatMap[B](f: A => ScynamoDecoder[B]): ScynamoDecoder[B] =
    ScynamoDecoder.instance(value => decode(value).flatMap(f(_).decode(value)))

  def orElse[AA >: A](other: ScynamoDecoder[A]): ScynamoDecoder[AA] =
    ScynamoDecoder.instance(value => decode(value).orElse(other.decode(value)))

  def transform[B](f: EitherNec[ScynamoDecodeError, A] => EitherNec[ScynamoDecodeError, B]): ScynamoDecoder[B] =
    ScynamoDecoder.instance(value => f(decode(value)))

  def defaultValue: Option[A] = None

  def withDefault(value: A): ScynamoDecoder[A] = new ScynamoDecoder[A] {
    override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, A] = self.decode(attributeValue)
    override val defaultValue: Option[A]                                                  = Some(value)
  }
}

object ScynamoDecoder extends DefaultScynamoDecoderInstances {
  def apply[A](implicit instance: ScynamoDecoder[A]): ScynamoDecoder[A] = instance

  def const[A](value: A): ScynamoDecoder[A] =
    instance(_ => Right(value))

  // SAM syntax generates anonymous classes because of non-abstract methods like `defaultValue`.
  private[scynamo] def instance[A](f: AttributeValue => EitherNec[ScynamoDecodeError, A]): ScynamoDecoder[A] = f(_)
}

trait DefaultScynamoDecoderInstances extends ScynamoDecoderFunctions with ScynamoIterableDecoder {
  private val rightNone = Right(None)

  implicit val catsInstances: Monad[ScynamoDecoder] with SemigroupK[ScynamoDecoder] =
    new Monad[ScynamoDecoder] with SemigroupK[ScynamoDecoder] {
      override def map[A, B](fa: ScynamoDecoder[A])(f: A => B): ScynamoDecoder[B] =
        fa.map(f)

      override def pure[A](x: A): ScynamoDecoder[A] =
        ScynamoDecoder.const(x)

      override def flatMap[A, B](fa: ScynamoDecoder[A])(f: A => ScynamoDecoder[B]): ScynamoDecoder[B] =
        fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => ScynamoDecoder[Either[A, B]]): ScynamoDecoder[B] = {
        @tailrec def go(a: A, value: AttributeValue): EitherNec[ScynamoDecodeError, B] =
          f(a).decode(value) match {
            case Right(Left(a))  => go(a, value)
            case Right(Right(b)) => Right(b)
            case Left(errors)    => Left(errors)
          }

        ScynamoDecoder.instance(go(a, _))
      }

      override def combineK[A](x: ScynamoDecoder[A], y: ScynamoDecoder[A]): ScynamoDecoder[A] =
        x.orElse(y)
    }

  implicit val stringDecoder: ScynamoDecoder[String] =
    ScynamoDecoder.instance(_.asEither(ScynamoType.String))

  implicit val intDecoder: ScynamoDecoder[Int] =
    ScynamoDecoder.instance(_.asEither(ScynamoType.Number).flatMap(convert(_, "Int")(_.toInt)))

  implicit val longDecoder: ScynamoDecoder[Long] =
    ScynamoDecoder.instance(_.asEither(ScynamoType.Number).flatMap(convert(_, "Long")(_.toLong)))

  implicit val bigIntDecoder: ScynamoDecoder[BigInt] =
    ScynamoDecoder.instance(_.asEither(ScynamoType.Number).flatMap(convert(_, "BigInt")(BigInt.apply)))

  implicit val floatDecoder: ScynamoDecoder[Float] =
    ScynamoDecoder.instance(_.asEither(ScynamoType.Number).flatMap(convert(_, "Float")(_.toFloat)))

  implicit val doubleDecoder: ScynamoDecoder[Double] =
    ScynamoDecoder.instance(_.asEither(ScynamoType.Number).flatMap(convert(_, "Double")(_.toDouble)))

  implicit val bigDecimalDecoder: ScynamoDecoder[BigDecimal] =
    ScynamoDecoder.instance(_.asEither(ScynamoType.Number).flatMap(convert(_, "BigDecimal")(BigDecimal.apply)))

  implicit val booleanDecoder: ScynamoDecoder[Boolean] =
    ScynamoDecoder.instance(_.asEither(ScynamoType.Bool))

  implicit val instantDecoder: ScynamoDecoder[Instant] =
    ScynamoDecoder.instance { attr =>
      for {
        number <- attr.asEither(ScynamoType.Number)
        result <- convert(number, "Long")(_.toLong)
      } yield Instant.ofEpochMilli(result)
    }

  implicit val instantTtlDecoder: ScynamoDecoder[Instant @@ TimeToLive] =
    ScynamoDecoder.instance { attr =>
      for {
        number <- attr.asEither(ScynamoType.Number)
        result <- convert(number, "Long")(_.toLong)
      } yield tag[TimeToLive][Instant](Instant.ofEpochSecond(result))
    }

  implicit def seqDecoder[A: ScynamoDecoder]: ScynamoDecoder[Seq[A]]       = iterableDecoder
  implicit def listDecoder[A: ScynamoDecoder]: ScynamoDecoder[List[A]]     = iterableDecoder
  implicit def vectorDecoder[A: ScynamoDecoder]: ScynamoDecoder[Vector[A]] = iterableDecoder
  implicit def setDecoder[A: ScynamoDecoder]: ScynamoDecoder[Set[A]]       = iterableDecoder

  implicit def optionDecoder[A](implicit element: ScynamoDecoder[A]): ScynamoDecoder[Option[A]] =
    new ScynamoDecoder[Option[A]] {
      override val defaultValue: Option[Option[A]] = Some(None)
      override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, Option[A]] =
        if (attributeValue.nul) rightNone else element.decode(attributeValue).map(Some.apply)
    }

  implicit val finiteDurationDecoder: ScynamoDecoder[FiniteDuration] =
    longDecoder.map(Duration.fromNanos)

  implicit val durationDecoder: ScynamoDecoder[Duration] =
    longDecoder.map(Duration(_, TimeUnit.NANOSECONDS))

  /** Using a custom formatter because
    * "Years outside the range 0000 to 9999 must be prefixed by the plus or minus symbol."
    * but the plus symbol is not added by the default `.toString`.
    */
  implicit val yearMonthDecoder: ScynamoDecoder[YearMonth] =
    stringDecoder.map(YearMonth.parse(_, DateTimeFormatter.ofPattern("uuuu-MM")))

  implicit val uuidDecoder: ScynamoDecoder[UUID] =
    ScynamoDecoder.instance(_.asEither(ScynamoType.String).flatMap(convert(_, "UUID")(UUID.fromString)))

  implicit def mapDecoder[A, B](implicit key: ScynamoKeyDecoder[A], value: ScynamoDecoder[B]): ScynamoDecoder[Map[A, B]] =
    ScynamoDecoder.instance(_.asEither(ScynamoType.Map).flatMap { attributes =>
      var allErrors = Chain.empty[ScynamoDecodeError]
      val allValues = Map.newBuilder[A, B]

      attributes.forEach { (k, v) =>
        (key.decode(k), value.decode(v)) match {
          case (Right(k), Right(v)) =>
            allValues += k -> v
          case (Left(errors), Right(_)) =>
            allErrors ++= StackFrame.decoding(errors, MapKey(k)).toChain
          case (Right(_), Left(errors)) =>
            allErrors ++= StackFrame.decoding(errors, MapKey(k)).toChain
          case (Left(kErrors), Left(vErrors)) =>
            allErrors ++= StackFrame.decoding(kErrors ++ vErrors, MapKey(k)).toChain
        }
      }

      NonEmptyChain.fromChain(allErrors).toLeft(allValues.result())
    })

  implicit val attributeValueDecoder: ScynamoDecoder[AttributeValue] =
    ScynamoDecoder.instance(Right.apply)

  implicit def fieldDecoder[K, V](implicit V: Lazy[ScynamoDecoder[V]]): ScynamoDecoder[FieldType[K, V]] =
    new ScynamoDecoder[FieldType[K, V]] {
      override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, FieldType[K, V]] =
        V.value.decode(attributeValue).map(field[K][V])
      override lazy val defaultValue: Option[FieldType[K, V]] =
        V.value.defaultValue.map(field[K][V])
    }
}

trait ScynamoIterableDecoder extends LowestPrioAutoDecoder {
  import scynamo.syntax.attributevalue._

  import scala.language.higherKinds

  def iterableDecoder[A, C[_] <: Iterable[_]](implicit element: ScynamoDecoder[A], factory: Factory[A, C[A]]): ScynamoDecoder[C[A]] =
    ScynamoDecoder.instance(_.asEither(ScynamoType.List).flatMap { attributes =>
      var allErrors = Chain.empty[ScynamoDecodeError]
      val allValues = factory.newBuilder
      var i         = 0

      while (i < attributes.size()) {
        element.decode(attributes.get(i)) match {
          case Right(value) => allValues += value
          case Left(errors) => allErrors ++= StackFrame.decoding(errors, Index(i)).toChain
        }

        i += 1
      }

      NonEmptyChain.fromChain(allErrors).toLeft(allValues.result())
    })
}

trait LowestPrioAutoDecoder {
  final implicit def autoDerivedScynamoDecoder[A: AutoDerivationUnlocked](implicit
      genericDecoder: Lazy[GenericScynamoDecoder[A]]
  ): ObjectScynamoDecoder[A] =
    scynamo.generic.semiauto.deriveScynamoDecoder[A]
}

object ScynamoDecoderFunctions extends ScynamoDecoderFunctions

trait ScynamoDecoderFunctions {
  def convert[A, B](s: A, to: String = "(unknown)")(convertor: A => B): EitherNec[ScynamoDecodeError, B] =
    try Right(convertor(s))
    catch {
      case NonFatal(e) => Either.leftNec(ScynamoDecodeError.conversionError(s.toString, to, Some(e)))
    }
}

trait ObjectScynamoDecoder[A] extends ScynamoDecoder[A] {
  import scynamo.syntax.attributevalue._

  def decodeMap(value: java.util.Map[String, AttributeValue]): EitherNec[ScynamoDecodeError, A]

  override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, A] =
    attributeValue.asEither(ScynamoType.Map).flatMap(decodeMap)

  override def map[B](f: A => B): ObjectScynamoDecoder[B] =
    ObjectScynamoDecoder.instance(decodeMap(_).map(f))

  override def transform[B](f: EitherNec[ScynamoDecodeError, A] => EitherNec[ScynamoDecodeError, B]): ObjectScynamoDecoder[B] =
    ObjectScynamoDecoder.instance(attributes => f(decodeMap(attributes)))
}

object ObjectScynamoDecoder extends ScynamoDecoderFunctions with SemiautoDerivationDecoder {
  def apply[A](implicit instance: ObjectScynamoDecoder[A]): ObjectScynamoDecoder[A] = instance

  // SAM syntax generates anonymous classes because of non-abstract methods like `defaultValue`.
  private[scynamo] def instance[A](
      f: java.util.Map[String, AttributeValue] => EitherNec[ScynamoDecodeError, A]
  ): ObjectScynamoDecoder[A] = f(_)

  def const[A](value: A): ObjectScynamoDecoder[A] =
    instance(_ => Right(value))

  implicit val catsInstances: Monad[ObjectScynamoDecoder] with SemigroupK[ObjectScynamoDecoder] =
    new Monad[ObjectScynamoDecoder] with SemigroupK[ObjectScynamoDecoder] {
      override def map[A, B](fa: ObjectScynamoDecoder[A])(f: A => B): ObjectScynamoDecoder[B] =
        fa.map(f)

      override def pure[A](x: A): ObjectScynamoDecoder[A] =
        ObjectScynamoDecoder.const(x)

      override def flatMap[A, B](fa: ObjectScynamoDecoder[A])(f: A => ObjectScynamoDecoder[B]): ObjectScynamoDecoder[B] =
        instance(attributes => fa.decodeMap(attributes).flatMap(f(_).decodeMap(attributes)))

      override def tailRecM[A, B](a: A)(f: A => ObjectScynamoDecoder[Either[A, B]]): ObjectScynamoDecoder[B] = {
        @tailrec def go(a: A, attributes: java.util.Map[String, AttributeValue]): EitherNec[ScynamoDecodeError, B] =
          f(a).decodeMap(attributes) match {
            case Right(Left(a))  => go(a, attributes)
            case Right(Right(b)) => Right(b)
            case Left(errors)    => Left(errors)
          }

        instance(go(a, _))
      }

      override def combineK[A](x: ObjectScynamoDecoder[A], y: ObjectScynamoDecoder[A]): ObjectScynamoDecoder[A] =
        instance(attributes => x.decodeMap(attributes).orElse(y.decodeMap(attributes)))
    }

  implicit def mapDecoder[A](implicit value: ScynamoDecoder[A]): ObjectScynamoDecoder[Map[String, A]] =
    instance { attributes =>
      var allErrors = Chain.empty[ScynamoDecodeError]
      val allValues = Map.newBuilder[String, A]

      attributes.forEach { (k, v) =>
        value.decode(v) match {
          case Right(value) => allValues += k -> value
          case Left(errors) => allErrors ++= StackFrame.decoding(errors, MapKey(k)).toChain
        }
      }

      NonEmptyChain.fromChain(allErrors).toLeft(allValues.result())
    }
}

trait ScynamoKeyDecoder[A] {
  def decode(value: String): EitherNec[ScynamoDecodeError, A]
}

object ScynamoKeyDecoder {
  def apply[A](implicit decoder: ScynamoKeyDecoder[A]): ScynamoKeyDecoder[A] = decoder

  // SAM syntax generates anonymous classes because of non-abstract methods like `defaultValue`.
  private[scynamo] def instance[A](f: String => EitherNec[ScynamoDecodeError, A]): ScynamoKeyDecoder[A] = f(_)

  implicit val stringKeyDecoder: ScynamoKeyDecoder[String] =
    instance(Right.apply)

  implicit val uuidKeyDecoder: ScynamoKeyDecoder[UUID] =
    instance(ScynamoDecoder.convert(_, "UUID")(UUID.fromString))
}
