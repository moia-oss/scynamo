package scynamo

import cats.data.EitherNec
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait DynamoCodec[A] extends DynamoEncoder[A] with DynamoDecoder[A]

object DynamoCodec {
  def apply[A](implicit codec: DynamoCodec[A]): DynamoCodec[A] = codec

  implicit def fromEncoderAndDecoder[A](
      implicit dynamoEncoder: DynamoEncoder[A],
      dynamoDecoder: DynamoDecoder[A]
  ): DynamoCodec[A] =
    new DynamoCodec[A] {
      override def encode(value: A): AttributeValue                                        = dynamoEncoder.encode(value)
      override def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, A] = dynamoDecoder.decode(attributeValue)
    }
}

trait ObjectDynamoCodec[A] extends ObjectDynamoEncoder[A] with DynamoCodec[A] { self =>
  def imap[B](f: A => B)(g: B => A): ObjectDynamoCodec[B] = new ObjectDynamoCodec[B] {
    override def decode(attributeValue: AttributeValue): EitherNec[DynamoDecodeError, B] = self.decode(attributeValue).map(f)
    override def encodeMap(value: B): java.util.Map[String, AttributeValue]              = self.encodeMap(g(value))
  }
}

object ObjectDynamoCodec {
  def apply[A](implicit codec: ObjectDynamoCodec[A]): ObjectDynamoCodec[A] = codec

  implicit def fromEncoderAndDecoder[A](implicit encoder: ObjectDynamoEncoder[A], decoder: DynamoDecoder[A]): ObjectDynamoCodec[A] =
    new ObjectDynamoCodec[A] {
      override def encodeMap(a: A): java.util.Map[String, AttributeValue] = encoder.encodeMap(a)

      override def decode(value: AttributeValue): EitherNec[DynamoDecodeError, A] = decoder.decode(value)
    }
}
