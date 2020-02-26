package scynamo

import cats.data.EitherNec
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait ScynamoCodec[A] extends ScynamoEncoder[A] with ScynamoDecoder[A]

object ScynamoCodec {
  def apply[A](implicit codec: ScynamoCodec[A]): ScynamoCodec[A] = codec

  implicit def fromEncoderAndDecoder[A](
      implicit scynamoEncoder: ScynamoEncoder[A],
      scynamoDecoder: ScynamoDecoder[A]
  ): ScynamoCodec[A] =
    new ScynamoCodec[A] {
      override def encode(value: A): AttributeValue                                         = scynamoEncoder.encode(value)
      override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, A] = scynamoDecoder.decode(attributeValue)
    }
}

trait ObjectScynamoCodec[A] extends ObjectScynamoEncoder[A] with ScynamoCodec[A] { self =>
  def imap[B](f: A => B)(g: B => A): ObjectScynamoCodec[B] = new ObjectScynamoCodec[B] {
    override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, B] = self.decode(attributeValue).map(f)
    override def encodeMap(value: B): java.util.Map[String, AttributeValue]               = self.encodeMap(g(value))
  }
}

object ObjectScynamoCodec {
  def apply[A](implicit codec: ObjectScynamoCodec[A]): ObjectScynamoCodec[A] = codec

  implicit def fromEncoderAndDecoder[A](implicit encoder: ObjectScynamoEncoder[A], decoder: ScynamoDecoder[A]): ObjectScynamoCodec[A] =
    new ObjectScynamoCodec[A] {
      override def encodeMap(a: A): java.util.Map[String, AttributeValue] = encoder.encodeMap(a)

      override def decode(value: AttributeValue): EitherNec[ScynamoDecodeError, A] = decoder.decode(value)
    }
}
