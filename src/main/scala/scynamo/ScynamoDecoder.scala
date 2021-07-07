package scynamo

import cats.data.{EitherNec, NonEmptyChain}
import cats.syntax.either._
import cats.syntax.parallel._
import cats.{Monad, SemigroupK}
import scynamo.StackFrame.Index
import scynamo.generic.auto.AutoDerivationUnlocked
import scynamo.generic.{GenericScynamoDecoder, SemiautoDerivationDecoder}
import shapeless.labelled.{field, FieldType}
import shapeless.tag.@@
import shapeless.{tag, Lazy}
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import java.time.Instant
import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.collection.compat._
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

case class ErrorStack(frames: List[StackFrame]) {
  def push(frame: StackFrame): ErrorStack = ErrorStack(frame +: frames)

  override def toString: String =
    frames.mkString("ErrorStack(", " -> ", ")")
}

object ErrorStack {
  val empty: ErrorStack = ErrorStack(List.empty)
}

sealed trait StackFrame extends Product with Serializable
object StackFrame {
  case class Attr(name: String)   extends StackFrame
  case class Case(name: String)   extends StackFrame
  case class Enum(name: String)   extends StackFrame
  case class Index(value: Int)    extends StackFrame
  case class MapKey[A](value: A)  extends StackFrame
  case class Custom(name: String) extends StackFrame

  private[scynamo] def push[A](
      encoded: EitherNec[ScynamoEncodeError, A],
      frame: => StackFrame
  ): EitherNec[ScynamoEncodeError, A] = {
    lazy val stackFrame = frame
    encoded.leftMap(_.map(_.push(stackFrame)))
  }
}

trait ScynamoDecoder[A] extends ScynamoDecoderFunctions { self =>
  def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, A]

  def map[B](f: A => B): ScynamoDecoder[B] =
    value => decode(value).map(f)

  def flatMap[B](f: A => ScynamoDecoder[B]): ScynamoDecoder[B] =
    value => decode(value).flatMap(f(_).decode(value))

  def orElse[AA >: A](other: ScynamoDecoder[A]): ScynamoDecoder[AA] =
    value => decode(value).orElse(other.decode(value))

  def transform[B](f: EitherNec[ScynamoDecodeError, A] => EitherNec[ScynamoDecodeError, B]): ScynamoDecoder[B] =
    value => f(decode(value))

  def defaultValue: Option[A] = None

  def withDefault(value: A): ScynamoDecoder[A] = new ScynamoDecoder[A] {
    override def decode(attributeValue: AttributeValue) = self.decode(attributeValue)
    override val defaultValue                           = Some(value)
  }
}

object ScynamoDecoder extends DefaultScynamoDecoderInstances {
  def apply[A](implicit instance: ScynamoDecoder[A]): ScynamoDecoder[A] = instance

  def const[A](value: A): ScynamoDecoder[A] =
    _ => Right(value)
}

trait DefaultScynamoDecoderInstances extends ScynamoDecoderFunctions with ScynamoIterableDecoder {
  import scynamo.syntax.attributevalue._

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

        go(a, _)
      }

      override def combineK[A](x: ScynamoDecoder[A], y: ScynamoDecoder[A]): ScynamoDecoder[A] =
        x.orElse(y)
    }

  implicit val stringDecoder: ScynamoDecoder[String] = attributeValue => attributeValue.asEither(ScynamoType.String)

  implicit val intDecoder: ScynamoDecoder[Int] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s, "Int")(_.toInt))

  implicit val longDecoder: ScynamoDecoder[Long] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s, "Long")(_.toLong))

  implicit val bigIntDecoder: ScynamoDecoder[BigInt] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s, "BigInt")(BigInt(_)))

  implicit val floatDecoder: ScynamoDecoder[Float] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s, "Float")(_.toFloat))

  implicit val doubleDecoder: ScynamoDecoder[Double] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s, "Double")(_.toDouble))

  implicit val bigDecimalDecoder: ScynamoDecoder[BigDecimal] =
    attributeValue => attributeValue.asEither(ScynamoType.Number).flatMap(s => convert(s, "BigDecimal")(BigDecimal(_)))

  implicit val booleanDecoder: ScynamoDecoder[Boolean] = attributeValue => attributeValue.asEither(ScynamoType.Bool)

  implicit val instantDecoder: ScynamoDecoder[Instant] =
    attributeValue =>
      for {
        nstring <- attributeValue.asEither(ScynamoType.Number)
        result  <- convert(nstring, "Long")(_.toLong)
      } yield Instant.ofEpochMilli(result)

  implicit val instantTtlDecoder: ScynamoDecoder[Instant @@ TimeToLive] =
    attributeValue =>
      for {
        nstring <- attributeValue.asEither(ScynamoType.Number)
        result  <- convert(nstring, "Long")(_.toLong)
      } yield tag[TimeToLive][Instant](Instant.ofEpochSecond(result))

  implicit def seqDecoder[A: ScynamoDecoder]: ScynamoDecoder[scala.collection.immutable.Seq[A]] = iterableDecoder

  implicit def listDecoder[A: ScynamoDecoder]: ScynamoDecoder[List[A]] = iterableDecoder

  implicit def vectorDecoder[A: ScynamoDecoder]: ScynamoDecoder[Vector[A]] = iterableDecoder

  implicit def setDecoder[A: ScynamoDecoder]: ScynamoDecoder[Set[A]] = iterableDecoder

  implicit def optionDecoder[A: ScynamoDecoder]: ScynamoDecoder[Option[A]] =
    new ScynamoDecoder[Option[A]] {
      override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, Option[A]] =
        if (attributeValue.nul()) Right(None) else ScynamoDecoder[A].decode(attributeValue).map(Some(_))

      override def defaultValue: Option[Option[A]] = Some(None)
    }

  implicit val finiteDurationDecoder: ScynamoDecoder[FiniteDuration] = longDecoder.map(Duration.fromNanos)

  implicit val durationDecoder: ScynamoDecoder[Duration] = longDecoder.map(n => Duration(n, TimeUnit.NANOSECONDS))

  implicit val uuidDecoder: ScynamoDecoder[UUID] = attributeValue =>
    attributeValue.asEither(ScynamoType.String).flatMap(s => convert(s, "UUID")(UUID.fromString))

  implicit def mapDecoder[A, B](implicit
      keyDecoder: ScynamoKeyDecoder[A],
      valueDecoder: ScynamoDecoder[B]
  ): ScynamoDecoder[Map[A, B]] =
    attributeValue =>
      attributeValue.asEither(ScynamoType.Map).flatMap { javaMap =>
        javaMap.asScala.toVector.zipWithIndex
          .parTraverse { case ((key, value), i) =>
            (keyDecoder.decode(key), valueDecoder.decode(value)).parMapN(_ -> _).leftMap(_.map(_.push(Index(i))))
          }
          .map(_.toMap)
      }

  implicit val attributeValueDecoder: ScynamoDecoder[AttributeValue] =
    attributeValue => Right(attributeValue)

  implicit def fieldDecoder[K, V](implicit V: Lazy[ScynamoDecoder[V]]): ScynamoDecoder[FieldType[K, V]] =
    new ScynamoDecoder[FieldType[K, V]] {
      override def decode(attributeValue: AttributeValue) =
        V.value.decode(attributeValue).map(field[K][V])
      override lazy val defaultValue =
        V.value.defaultValue.map(field[K][V])
    }
}

trait ScynamoIterableDecoder extends LowestPrioAutoDecoder {
  import scynamo.syntax.attributevalue._
  def iterableDecoder[A: ScynamoDecoder, C[_] <: Iterable[A], X](implicit factory: Factory[A, C[A]]): ScynamoDecoder[C[A]] =
    attributeValue =>
      attributeValue.asEither(ScynamoType.List).flatMap { theList =>
        val builder = factory.newBuilder
        var elems   = Either.rightNec[ScynamoDecodeError, builder.type](builder)
        var i       = 0

        theList.forEach { elem =>
          val decoded = ScynamoDecoder[A].decode(elem).leftMap(_.map(_.push(Index(i))))
          elems = (elems, decoded).parMapN((builder, dec) => builder += dec)
          i += 1
        }

        elems.map(_.result())
      }
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
    attributes => decodeMap(attributes).map(f)

  override def transform[B](f: EitherNec[ScynamoDecodeError, A] => EitherNec[ScynamoDecodeError, B]): ObjectScynamoDecoder[B] =
    attributes => f(decodeMap(attributes))
}

object ObjectScynamoDecoder extends ScynamoDecoderFunctions with SemiautoDerivationDecoder {
  def apply[A](implicit instance: ObjectScynamoDecoder[A]): ObjectScynamoDecoder[A] = instance

  def const[A](value: A): ObjectScynamoDecoder[A] =
    _ => Right(value)

  implicit val catsInstances: Monad[ObjectScynamoDecoder] with SemigroupK[ObjectScynamoDecoder] =
    new Monad[ObjectScynamoDecoder] with SemigroupK[ObjectScynamoDecoder] {
      override def map[A, B](fa: ObjectScynamoDecoder[A])(f: A => B): ObjectScynamoDecoder[B] =
        fa.map(f)

      override def pure[A](x: A): ObjectScynamoDecoder[A] =
        ObjectScynamoDecoder.const(x)

      override def flatMap[A, B](fa: ObjectScynamoDecoder[A])(f: A => ObjectScynamoDecoder[B]): ObjectScynamoDecoder[B] =
        attributes => fa.decodeMap(attributes).flatMap(f(_).decodeMap(attributes))

      override def tailRecM[A, B](a: A)(f: A => ObjectScynamoDecoder[Either[A, B]]): ObjectScynamoDecoder[B] = {
        @tailrec def go(a: A, attributes: java.util.Map[String, AttributeValue]): EitherNec[ScynamoDecodeError, B] =
          f(a).decodeMap(attributes) match {
            case Right(Left(a))  => go(a, attributes)
            case Right(Right(b)) => Right(b)
            case Left(errors)    => Left(errors)
          }

        go(a, _)
      }

      override def combineK[A](x: ObjectScynamoDecoder[A], y: ObjectScynamoDecoder[A]): ObjectScynamoDecoder[A] =
        attributes => x.decodeMap(attributes).orElse(y.decodeMap(attributes))
    }

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

    result.leftMap(e => NonEmptyChain.one(ScynamoDecodeError.conversionError(s"$s", "UUID", Some(e))))
  }
}
