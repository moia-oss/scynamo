package scynamo.wrapper

import cats.instances.either._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.parallel._
import scynamo.syntax.attributevalue._
import scynamo._
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.jdk.CollectionConverters._

final class ScynamoStringSet private (val value: Set[String]) extends AnyVal
object ScynamoStringSet {
  def apply(value: Set[String]) = new ScynamoStringSet(value)

  implicit val scynamoStringSetEncoder: ScynamoEncoder[ScynamoStringSet] = value =>
    if (value.value.nonEmpty) {
      Right(AttributeValue.builder().ss(value.value.asJava).build())
    } else {
      Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.StringSet))
    }

  implicit val scynamoStringSetDecoder: ScynamoDecoder[ScynamoStringSet] = attributeValue =>
    attributeValue.asOption(ScynamoType.StringSet) match {
      case Some(value) => Right(new ScynamoStringSet(value.iterator().asScala.toSet))
      case None        => Either.leftNec(ScynamoDecodeError.typeMismatch(ScynamoType.StringSet, attributeValue))
    }
}

final class ScynamoNumberSet[A] private (val value: Set[A]) extends AnyVal

object ScynamoNumberSet {
  type D = DummyImplicit

  def apply(value: Set[Int]): ScynamoNumberSet[Int]                                                      = new ScynamoNumberSet(value)
  def apply(value: Set[Long])(implicit a: D): ScynamoNumberSet[Long]                                     = new ScynamoNumberSet(value)
  def apply(value: Set[Float])(implicit a: D, b: D): ScynamoNumberSet[Float]                             = new ScynamoNumberSet(value)
  def apply(value: Set[Double])(implicit a: D, b: D, c: D): ScynamoNumberSet[Double]                     = new ScynamoNumberSet(value)
  def apply(value: Set[BigInt])(implicit a: D, b: D, c: D, d: D): ScynamoNumberSet[BigInt]               = new ScynamoNumberSet(value)
  def apply(value: Set[BigDecimal])(implicit a: D, b: D, c: D, d: D, e: D): ScynamoNumberSet[BigDecimal] = new ScynamoNumberSet(value)

  implicit def scynamoIntSetEncoder: ScynamoEncoder[ScynamoNumberSet[Int]] = numberSetEncoder[Int]

  implicit def scynamoLongSetEncoder: ScynamoEncoder[ScynamoNumberSet[Long]] = numberSetEncoder[Long]

  implicit def scynamoFloatSetEncoder: ScynamoEncoder[ScynamoNumberSet[Float]] = numberSetEncoder[Float]

  implicit def scynamoDoubleSetEncoder: ScynamoEncoder[ScynamoNumberSet[Double]] = numberSetEncoder[Double]

  implicit def scynamoBigIntSetEncoder: ScynamoEncoder[ScynamoNumberSet[BigInt]] = numberSetEncoder[BigInt]

  implicit def scynamoBigDecimalSetEncoder: ScynamoEncoder[ScynamoNumberSet[BigDecimal]] = numberSetEncoder[BigDecimal]

  implicit val scynamoIntSetDecoder: ScynamoDecoder[ScynamoNumberSet[Int]] = numberSetDecoder("Int")(_.toInt)

  implicit val scynamoLongSetDecoder: ScynamoDecoder[ScynamoNumberSet[Long]] = numberSetDecoder("Long")(_.toLong)

  implicit val scynamoFloatSetDecoder: ScynamoDecoder[ScynamoNumberSet[Float]] = numberSetDecoder("Float")(_.toFloat)

  implicit val scynamoDoubleSetDecoder: ScynamoDecoder[ScynamoNumberSet[Double]] = numberSetDecoder("Double")(_.toDouble)

  implicit val scynamoBigIntSetDecoder: ScynamoDecoder[ScynamoNumberSet[BigInt]] = numberSetDecoder("BigInt")(BigInt(_))

  implicit val scynamoBigDecimalSetDecoder: ScynamoDecoder[ScynamoNumberSet[BigDecimal]] = numberSetDecoder("BigDecimal")(BigDecimal(_))

  private[this] def numberSetEncoder[A]: ScynamoEncoder[ScynamoNumberSet[A]] =
    value =>
      if (value.value.nonEmpty) {
        Right(AttributeValue.builder().ns(value.value.map(_.toString).asJava).build())
      } else {
        Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.NumberSet))
      }

  private[this] def numberSetDecoder[A](targetTypeName: String)(f: String => A): ScynamoDecoder[ScynamoNumberSet[A]] =
    attributeValue =>
      attributeValue.asOption(ScynamoType.NumberSet) match {
        case Some(value) =>
          value.asScala.toList.parTraverse(ScynamoDecoder.convert(_, targetTypeName)(f)).map(x => new ScynamoNumberSet[A](x.toSet))
        case None => Either.leftNec(ScynamoDecodeError.typeMismatch(ScynamoType.NumberSet, attributeValue))
      }
}

final class ScynamoBinarySet private (val value: Set[SdkBytes]) extends AnyVal
object ScynamoBinarySet {
  def apply(value: Set[SdkBytes]): ScynamoBinarySet = new ScynamoBinarySet(value)

  implicit val scynamoBinarySetEncoder: ScynamoEncoder[ScynamoBinarySet] = value =>
    if (value.value.nonEmpty) {
      Right(AttributeValue.builder().bs(value.value.asJava).build())
    } else {
      Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.BinarySet))
    }

  implicit val scynamoBinarySetDecoder: ScynamoDecoder[ScynamoBinarySet] = attributeValue =>
    attributeValue.asOption(ScynamoType.BinarySet) match {
      case Some(value) => Right(new ScynamoBinarySet(value.iterator().asScala.toSet))
      case None        => Either.leftNec(ScynamoDecodeError.typeMismatch(ScynamoType.BinarySet, attributeValue))
    }
}
