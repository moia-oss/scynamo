package io.moia.dynamo

import cats.data.EitherNec
import shapeless.Lazy
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait DynamoCodec[A] extends DynamoEncoder[A] with DynamoDecoder[A]

object DynamoCodec {
  def apply[A](implicit codec: DynamoCodec[A]): DynamoCodec[A] = codec

  def derive[A](implicit encoder: Lazy[DynamoEncoder[A]], decoder: Lazy[DynamoDecoder[A]]): DynamoCodec[A] =
    fromEncoderAndDecoder(encoder.value, decoder.value)

  implicit def fromEncoderAndDecoder[A](implicit dynamoEncoder: DynamoEncoder[A], dynamoDecoder: DynamoDecoder[A]): DynamoCodec[A] =
    new DynamoCodec[A] {
      override def encode(value: A): AttributeValue                                        = dynamoEncoder.encode(value)
      override def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, A] = dynamoDecoder.decode(attributeValue)
    }
}
