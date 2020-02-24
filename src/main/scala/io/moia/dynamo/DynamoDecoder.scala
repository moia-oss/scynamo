package io.moia.dynamo

import java.time.Instant

import cats.instances.vector._
import cats.syntax.traverse._
import scala.jdk.CollectionConverters._
import cats.data.{EitherNec, NonEmptyChain}
import cats.instances.either._
import cats.kernel.Eq
import cats.syntax.apply._
import cats.syntax.either._
import io.moia.dynamo.DynamoType.{DynamoBool, DynamoList, DynamoMap, DynamoNumber, DynamoString}
import shapeless._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.util.control.NonFatal

abstract class DynamoDecodeError                                              extends Product with Serializable
case class MissingField(fieldName: String, attributeValue: AttributeValue)    extends DynamoDecodeError
case class TypeMismatch(expected: DynamoType, attributeValue: AttributeValue) extends DynamoDecodeError
case class InvalidCase(attributeValue: AttributeValue)                        extends DynamoDecodeError
case class ParseError(message: String, cause: Option[Throwable])              extends DynamoDecodeError
case class InvalidTypeTag(attributeValue: AttributeValue)                     extends DynamoDecodeError

object DynamoDecodeError {
  implicit val dynamoDecodeErrorEq: Eq[DynamoDecodeError] = Eq.fromUniversalEquals[DynamoDecodeError]
}

trait DynamoDecoder[A] { self =>
  def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, A]

  def map[B](f: A => B): DynamoDecoder[B] = value => self.decode(value).map(f)
}

object DynamoDecoder extends LabelledTypeClassCompanion[DynamoDecoder] with DynamoDecoderInstances {
  import io.moia.dynamo.attributevalue.dsl._
  object typeClass extends LabelledTypeClass[DynamoDecoder] {
    override def emptyProduct: DynamoDecoder[HNil] = _ => Right(HNil)

    override def product[F, T <: HList](name: String, decoderHead: DynamoDecoder[F], decoderTail: DynamoDecoder[T]): DynamoDecoder[F :: T] =
      value => {
        val decodedHead = for {
          map            <- accessOrTypeMismatch(value, DynamoMap)(_.mOpt)
          fieldAttrValue <- Option(map.get(name)).map(Right(_)).getOrElse(Either.leftNec(MissingField(name, value)))
          result         <- decoderHead.decode(fieldAttrValue)
        } yield result

        (decodedHead, decoderTail.decode(value)).mapN(_ :: _)
      }

    override def emptyCoproduct: DynamoDecoder[CNil] = value => Either.leftNec(InvalidCase(value))

    override def coproduct[L, R <: Coproduct](
        name: String,
        decodeL: => DynamoDecoder[L],
        decodeR: => DynamoDecoder[R]
    ): DynamoDecoder[L :+: R] =
      value => {
        for {
          map <- accessOrTypeMismatch(value, DynamoMap)(_.mOpt)
          typeTagAttrValue <- Option(map.get(DynamoEncoder.MAGIC_TYPE_ATTRIBUTE_NAME))
            .map(Right(_))
            .getOrElse(Either.leftNec(MissingField(name, value)))
          typeTag <- accessOrTypeMismatch(typeTagAttrValue, DynamoString)(_.sOpt)
          result  <- if (name == typeTag) decodeL.decode(value).map(Inl(_)) else decodeR.decode(value).map(Inr(_))
        } yield result
      }

    override def project[F, G](instance: => DynamoDecoder[G], to: F => G, from: G => F): DynamoDecoder[F] =
      attributeValue => instance.decode(attributeValue).map(from)
  }
}

trait DynamoDecoderInstances {
  import io.moia.dynamo.attributevalue.dsl._

  private[dynamo] def accessOrTypeMismatch[A](attributeValue: AttributeValue, typ: DynamoType)(
      access: AttributeValue => Option[A]
  ): Either[NonEmptyChain[TypeMismatch], A] =
    access(attributeValue) match {
      case None        => Either.leftNec(TypeMismatch(typ, attributeValue))
      case Some(value) => Right(value)
    }

  private[this] def convert[A, B](s: A)(convertor: A => B): EitherNec[DynamoDecodeError, B] =
    try {
      Right(convertor(s))
    } catch {
      case NonFatal(e) => Either.leftNec(ParseError(s"Could not convert: ${e.getMessage}", Some(e)))
    }

  implicit def stringDecoder: DynamoDecoder[String] = attributeValue => accessOrTypeMismatch(attributeValue, DynamoString)(_.sOpt)

  implicit def numericDecoder[A](implicit N: Numeric[A]): DynamoDecoder[A] =
    attributeValue =>
      for {
        nString <- accessOrTypeMismatch(attributeValue, DynamoNumber)(_.nOpt)
        n       <- convert(nString)(s => N.parseString(s).get)
      } yield n

  implicit def booleanDecoder: DynamoDecoder[Boolean] =
    attributeValue => accessOrTypeMismatch(attributeValue, DynamoBool)(_.boolOpt)

  implicit def instantDecoder: DynamoDecoder[Instant] =
    attributeValue =>
      for {
        nString <- accessOrTypeMismatch(attributeValue, DynamoNumber)(_.nOpt)
        long    <- convert(nString)(_.toLong)
        result  <- convert(long)(Instant.ofEpochMilli)
      } yield result

  implicit def seqDecoder[A: DynamoDecoder]: DynamoDecoder[Seq[A]] =
    attributeValue =>
      for {
        list   <- accessOrTypeMismatch(attributeValue, DynamoList)(_.lOpt)
        result <- list.iterator.asScala.toVector.traverse(DynamoDecoder[A].decode)
      } yield result
}
