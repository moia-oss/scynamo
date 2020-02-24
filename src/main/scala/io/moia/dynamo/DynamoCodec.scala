package io.moia.dynamo

import cats.data.EitherNec
import shapeless.Lazy
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait DynamoCodec[A] extends DynamoEncoder[A] with DynamoDecoder[A]

object DynamoCodec {
  def apply[A](implicit codec: DynamoCodec[A]): DynamoCodec[A] = codec

  implicit def fromEncoderAndDecoder[A](implicit dynamoEncoder: DynamoEncoder[A], dynamoDecoder: DynamoDecoder[A]): DynamoCodec[A] =
    new DynamoCodec[A] {
      override def encode(value: A): AttributeValue                                        = dynamoEncoder.encode(value)
      override def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, A] = dynamoDecoder.decode(attributeValue)
    }

  implicit def toGeneric[A](implicit Codec: DynamoCodec[A]): GenericDynamoCodec[A] = new GenericDynamoCodec[A] {
    override def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, A] = Codec.decode(attributeValue)

    override def encode(value: A): AttributeValue = Codec.encode(value)
  }
}

trait GenericDynamoCodec[A] extends GenericDynamoEncoder[A] with GenericDynamoDecoder[A]

object GenericDynamoCodec {
  def apply[A](implicit lenc: Lazy[GenericDynamoEncoder[A]], ldec: Lazy[GenericDynamoDecoder[A]]): GenericDynamoCodec[A] =
    new GenericDynamoCodec[A] {
      override def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, A] = ldec.value.decode(attributeValue)

      override def encode(value: A): AttributeValue = lenc.value.encode(value)
    }
}
