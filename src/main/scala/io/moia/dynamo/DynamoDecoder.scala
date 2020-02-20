package io.moia.dynamo.foo

import cats.data.EitherNec
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.either._
import io.moia.dynamo.DynamoEncoder
import shapeless._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait DynamoDecodeError
case class MissingField(fieldName: String) extends DynamoDecodeError
case class TypeMismatch(x: String)         extends DynamoDecodeError

trait DynamoDecoder[A] { self =>
  def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, A]

  def map[B](f: A => B): DynamoDecoder[B] = value => self.decode(value).map(f)
}

object DynamoDecoder extends LabelledTypeClassCompanion[DynamoDecoder] {
  implicit def stringDecoder: DynamoDecoder[String] = new DynamoDecoder[String] {
    override def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, String] =
      Option(attributeValue.s()).map(Right(_)).getOrElse(Either.leftNec(TypeMismatch("TODO (string)"))) // TODO: parse
  }

  implicit def intEncoder: DynamoDecoder[Int] = new DynamoDecoder[Int] {
    override def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, Int] =
      Option(attributeValue.n()).map(_.toInt).map(Right(_)).getOrElse(Either.leftNec(TypeMismatch("TODO (int)")))
    // TODO: parse
  }

  object typeClass extends LabelledTypeClass[DynamoDecoder] {
    override def emptyProduct: DynamoDecoder[HNil] = _ => Right(HNil)

    override def product[F, T <: HList](name: String, decoderHead: DynamoDecoder[F], decoderTail: DynamoDecoder[T]): DynamoDecoder[F :: T] =
      value => {
        val decodedHead = decoderHead.decode(value.m().get(name))
        (decodedHead, decoderTail.decode(value)).mapN(_ :: _)
      }

    override def emptyCoproduct: DynamoDecoder[CNil] = _ => Either.leftNec(TypeMismatch("TODO"))

    override def coproduct[L, R <: Coproduct](
        name: String,
        decodeL: => DynamoDecoder[L],
        decodeR: => DynamoDecoder[R]
    ): DynamoDecoder[L :+: R] =
      new DynamoDecoder[L :+: R] {
        override def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, L :+: R] = {
          // TODO: make it nicer
          val typeTag = attributeValue.m().get(DynamoEncoder.MAGIC_TYPE_ATTRIBUTE_NAME).s()

          if (typeTag == name) {
            decodeL.decode(attributeValue).map(Inl(_))
          } else {
            decodeR.decode(attributeValue).map(Inr(_))
          }
        }
      }

    override def project[F, G](instance: => DynamoDecoder[G], to: F => G, from: G => F): DynamoDecoder[F] =
      attributeValue => instance.decode(attributeValue).map(from)

  }
}
