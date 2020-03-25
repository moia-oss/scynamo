package scynamo.syntax.encoder

import java.util

import cats.data.EitherNec
import scynamo.{ObjectScynamoEncoder, ScynamoEncodeError, ScynamoEncoder}
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class ScynamoEncoderDslOps[A](val value: A) extends AnyVal {
  def encoded(implicit encoder: ScynamoEncoder[A]): EitherNec[ScynamoEncodeError, AttributeValue] = encoder.encode(value)

  def encodedUnsafe(implicit encoder: ScynamoEncoder[A]): AttributeValue = unsafeUnwrap(encoder.encode(value))

  def encodedMap(implicit encoder: ObjectScynamoEncoder[A]): EitherNec[ScynamoEncodeError, util.Map[String, AttributeValue]] =
    encoder.encodeMap(value)

  def encodedMapUnsafe(implicit encoder: ObjectScynamoEncoder[A]): util.Map[String, AttributeValue] =
    unsafeUnwrap(encoder.encodeMap(value))

  private[this] def unsafeUnwrap[B](input: EitherNec[ScynamoEncodeError, B]): B = input match {
    case Left(errors) => throw new IllegalArgumentException(s"Decoding as a map failed with: ${errors.map(_.show)}")
    case Right(value) => value
  }
}

trait ScynamoEncoderDsl {
  implicit def toScynamoEncoderDslOps[A: ScynamoEncoder](value: A): ScynamoEncoderDslOps[A] = new ScynamoEncoderDslOps(value)
}
