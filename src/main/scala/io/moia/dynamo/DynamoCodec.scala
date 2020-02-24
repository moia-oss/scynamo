package io.moia.dynamo

import cats.data.EitherNec
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait DynamoCodec[A] extends DynamoEncoder[A] with DynamoDecoder[A] {
  def encoder: DynamoEncoder[A]
  def decoder: DynamoDecoder[A]
}

object DynamoCodec {
  def apply[A](implicit codec: DynamoCodec[A]): DynamoCodec[A] = codec

  implicit def fromEncoderAndDecoder[A](implicit dynamoEncoder: DynamoEncoder[A], dynamoDecoder: DynamoDecoder[A]): DynamoCodec[A] =
    new DynamoCodec[A] {
      override val encoder: DynamoEncoder[A] = dynamoEncoder
      override val decoder: DynamoDecoder[A] = dynamoDecoder

      override def encode(value: A): AttributeValue                                        = encoder.encode(value)
      override def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, A] = decoder.decode(attributeValue)
    }
}
