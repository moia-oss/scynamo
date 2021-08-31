package scynamo

import cats.Contravariant
import cats.data.{Chain, EitherNec, NonEmptyChain}
import cats.syntax.all._
import scynamo.StackFrame.{Index, MapKey}
import scynamo.generic.auto.AutoDerivationUnlocked
import scynamo.generic.{GenericScynamoEncoder, SemiautoDerivationEncoder}
import scynamo.wrapper.DateTimeFormatters
import shapeless._
import shapeless.labelled.FieldType
import shapeless.tag.@@
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import java.time._
import java.util.UUID
import scala.collection.compat._
import scala.collection.immutable.Seq
import scala.concurrent.duration.{Duration, FiniteDuration}

trait ScynamoEncoder[A] { self =>
  def encode(value: A): EitherNec[ScynamoEncodeError, AttributeValue]
  def contramap[B](f: B => A): ScynamoEncoder[B] =
    ScynamoEncoder.instance(value => self.encode(f(value)))
}

object ScynamoEncoder extends DefaultScynamoEncoderInstances {
  def apply[A](implicit instance: ScynamoEncoder[A]): ScynamoEncoder[A] = instance

  /** SAM syntax generates anonymous classes on Scala 2 because of non-abstract methods like `contramap`. */
  def instance[A](encode: A => EitherNec[ScynamoEncodeError, AttributeValue]): ScynamoEncoder[A] = encode(_)
}

trait DefaultScynamoEncoderInstances extends ScynamoIterableEncoder {
  private val rightNul = Right(AttributeValue.builder.nul(true).build())

  implicit val catsInstances: Contravariant[ScynamoEncoder] = new Contravariant[ScynamoEncoder] {
    override def contramap[A, B](fa: ScynamoEncoder[A])(f: B => A): ScynamoEncoder[B] = fa.contramap(f)
  }

  implicit val stringEncoder: ScynamoEncoder[String] =
    ScynamoEncoder.instance(value => Right(AttributeValue.builder.s(value).build()))

  private[this] val numberStringEncoder: ScynamoEncoder[String] =
    ScynamoEncoder.instance(value => Right(AttributeValue.builder.n(value).build()))

  implicit val intEncoder: ScynamoEncoder[Int] =
    numberStringEncoder.contramap(_.toString)

  implicit val longEncoder: ScynamoEncoder[Long] =
    numberStringEncoder.contramap(_.toString)

  implicit val bigIntEncoder: ScynamoEncoder[BigInt] =
    numberStringEncoder.contramap(_.toString)

  implicit val floatEncoder: ScynamoEncoder[Float] =
    numberStringEncoder.contramap(_.toString)

  implicit val doubleEncoder: ScynamoEncoder[Double] =
    numberStringEncoder.contramap(_.toString)

  implicit val bigDecimalEncoder: ScynamoEncoder[BigDecimal] =
    numberStringEncoder.contramap(_.toString)

  implicit val booleanEncoder: ScynamoEncoder[Boolean] =
    ScynamoEncoder.instance(value => Right(AttributeValue.builder.bool(value).build()))

  implicit val instantEncoder: ScynamoEncoder[Instant] =
    numberStringEncoder.contramap(_.toEpochMilli.toString)

  implicit val instantTtlEncoder: ScynamoEncoder[Instant @@ TimeToLive] =
    numberStringEncoder.contramap[Instant @@ TimeToLive](_.getEpochSecond.toString)

  implicit val uuidEncoder: ScynamoEncoder[UUID] =
    stringEncoder.contramap(_.toString)

  implicit def seqEncoder[A](implicit element: ScynamoEncoder[A]): ScynamoEncoder[Seq[A]] =
    ScynamoEncoder.instance { xs =>
      var allErrors  = Chain.empty[ScynamoEncodeError]
      val attrValues = List.newBuilder[AttributeValue]
      for ((x, i) <- xs.iterator.zipWithIndex) element.encode(x) match {
        case Right(attr)  => attrValues += attr
        case Left(errors) => allErrors ++= StackFrame.encoding(errors, Index(i)).toChain
      }

      NonEmptyChain.fromChain(allErrors).toLeft(AttributeValue.builder.l(attrValues.result(): _*).build())
    }

  implicit def listEncoder[A: ScynamoEncoder]: ScynamoEncoder[List[A]] =
    seqEncoder[A].narrow

  implicit def vectorEncoder[A: ScynamoEncoder]: ScynamoEncoder[Vector[A]] =
    seqEncoder[A].narrow

  implicit def setEncoder[A: ScynamoEncoder]: ScynamoEncoder[Set[A]] =
    listEncoder[A].contramap(_.toList)

  implicit def optionEncoder[A](implicit element: ScynamoEncoder[A]): ScynamoEncoder[Option[A]] =
    ScynamoEncoder.instance {
      case Some(value) => element.encode(value)
      case None        => rightNul
    }

  implicit def someEncoder[A](implicit element: ScynamoEncoder[A]): ScynamoEncoder[Some[A]] =
    ScynamoEncoder.instance(some => element.encode(some.get))

  implicit val durationEncoder: ScynamoEncoder[Duration] =
    numberStringEncoder.contramap(_.toNanos.toString)

  implicit val finiteDurationEncoder: ScynamoEncoder[FiniteDuration] =
    durationEncoder.narrow

  implicit val javaDurationEncoder: ScynamoEncoder[java.time.Duration] =
    numberStringEncoder.contramap(_.toNanos.toString)

  implicit val yearMonthEncoder: ScynamoEncoder[YearMonth] =
    stringEncoder.contramap(_.format(DateTimeFormatters.yearMonth))

  implicit val localDateEncoder: ScynamoEncoder[LocalDate] =
    stringEncoder.contramap(_.format(DateTimeFormatters.localDate))

  implicit val localDateTimeEncoder: ScynamoEncoder[LocalDateTime] =
    stringEncoder.contramap(_.format(DateTimeFormatters.localDateTime))

  implicit val zonedDateTimeEncoder: ScynamoEncoder[ZonedDateTime] =
    stringEncoder.contramap(_.format(DateTimeFormatters.zonedDateTime))

  implicit def mapEncoder[A, B](implicit key: ScynamoKeyEncoder[A], value: ScynamoEncoder[B]): ScynamoEncoder[Map[A, B]] =
    ScynamoEncoder.instance { kvs =>
      var allErrors  = Chain.empty[ScynamoEncodeError]
      val attrValues = new java.util.HashMap[String, AttributeValue](kvs.size)
      kvs.foreachEntry { (k, v) =>
        (key.encode(k), value.encode(v)) match {
          case (Right(k), Right(attr)) =>
            // Omit `nul` for efficiency and GSI support (see https://github.com/aws/aws-sdk-go/issues/1803)
            if (!attr.nul) attrValues.put(k, attr)
          case (Left(errors), Right(_)) =>
            allErrors ++= StackFrame.encoding(errors, MapKey(k)).toChain
          case (Right(_), Left(errors)) =>
            allErrors ++= StackFrame.encoding(errors, MapKey(k)).toChain
          case (Left(kErrors), Left(vErrors)) =>
            allErrors ++= StackFrame.encoding(kErrors ++ vErrors, MapKey(k)).toChain
        }
      }

      NonEmptyChain.fromChain(allErrors).toLeft(AttributeValue.builder.m(attrValues).build())
    }

  implicit val attributeValueEncoder: ScynamoEncoder[AttributeValue] =
    ScynamoEncoder.instance { value =>
      import scynamo.syntax.attributevalue._

      def nonEmpty[A](typ: ScynamoType.Aux[java.util.List[A]] with ScynamoType.TypeInvalidIfEmpty) =
        if (value.asOption(typ).exists(!_.isEmpty)) Right(value)
        else Either.leftNec(ScynamoEncodeError.invalidEmptyValue(typ))

      if (value.hasSs) nonEmpty(ScynamoType.StringSet)
      else if (value.hasNs) nonEmpty(ScynamoType.NumberSet)
      else if (value.hasBs) nonEmpty(ScynamoType.BinarySet)
      else Right(value)
    }

  implicit def eitherScynamoErrorEncoder[A](implicit right: ScynamoEncoder[A]): ScynamoEncoder[EitherNec[ScynamoEncodeError, A]] =
    ScynamoEncoder.instance {
      case Left(errors) => Left(errors)
      case Right(value) => right.encode(value)
    }

  implicit def fieldEncoder[K, V](implicit V: Lazy[ScynamoEncoder[V]]): ScynamoEncoder[FieldType[K, V]] =
    ScynamoEncoder.instance(V.value.encode)
}

trait ScynamoIterableEncoder extends LowestPrioAutoEncoder {
  def iterableEncoder[A: ScynamoEncoder]: ScynamoEncoder[Iterable[A]] =
    ScynamoEncoder.listEncoder[A].contramap(_.toList)
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
    encodeMap(value).map(AttributeValue.builder.m(_).build())
}

object ObjectScynamoEncoder extends SemiautoDerivationEncoder {
  def apply[A](implicit instance: ObjectScynamoEncoder[A]): ObjectScynamoEncoder[A] = instance

  // SAM syntax generates anonymous classes because of non-abstract methods like `encode`.
  private[scynamo] def instance[A](
      f: A => EitherNec[ScynamoEncodeError, java.util.Map[String, AttributeValue]]
  ): ObjectScynamoEncoder[A] = f(_)

  implicit val catsInstances: Contravariant[ObjectScynamoEncoder] = new Contravariant[ObjectScynamoEncoder] {
    override def contramap[A, B](fa: ObjectScynamoEncoder[A])(f: B => A): ObjectScynamoEncoder[B] =
      instance(value => fa.encodeMap(f(value)))
  }

  implicit def mapEncoder[A](implicit value: ScynamoEncoder[A]): ObjectScynamoEncoder[Map[String, A]] =
    instance { kvs =>
      var allErrors  = Chain.empty[ScynamoEncodeError]
      val attrValues = new java.util.HashMap[String, AttributeValue](kvs.size)
      kvs.foreachEntry { (k, v) =>
        value.encode(v) match {
          // Omit `nul` for efficiency and GSI support (see https://github.com/aws/aws-sdk-go/issues/1803)
          case Right(attr)  => if (!attr.nul) attrValues.put(k, attr)
          case Left(errors) => allErrors ++= StackFrame.encoding(errors, MapKey(k)).toChain
        }
      }

      NonEmptyChain.fromChain(allErrors).toLeft(attrValues)
    }
}

trait ScynamoKeyEncoder[A] { self =>
  def encode(value: A): EitherNec[ScynamoEncodeError, String]
  def contramap[B](f: B => A): ScynamoKeyEncoder[B] =
    ScynamoKeyEncoder.instance(value => self.encode(f(value)))
}

object ScynamoKeyEncoder {
  def apply[A](implicit encoder: ScynamoKeyEncoder[A]): ScynamoKeyEncoder[A] = encoder

  // SAM syntax generates anonymous classes because of non-abstract methods like `contramap`.
  private[scynamo] def instance[A](f: A => EitherNec[ScynamoEncodeError, String]): ScynamoKeyEncoder[A] = f(_)

  implicit val catsInstances: Contravariant[ScynamoKeyEncoder] = new Contravariant[ScynamoKeyEncoder] {
    override def contramap[A, B](fa: ScynamoKeyEncoder[A])(f: B => A): ScynamoKeyEncoder[B] = fa.contramap(f)
  }

  implicit val stringKeyEncoder: ScynamoKeyEncoder[String] = instance { value =>
    if (value.nonEmpty) Right(value)
    else Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.String))
  }

  implicit val uuidKeyEncoder: ScynamoKeyEncoder[UUID] =
    ScynamoKeyEncoder[String].contramap(_.toString)
}
