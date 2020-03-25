package scynamo.syntax.encoder

import java.util

import cats.data.EitherNec
import scynamo.{ObjectScynamoEncoder, ScynamoEncodeError, ScynamoEncoder}
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class ScynamoEncoderDslOps[A](val value: A) extends AnyVal {
  def encoded(implicit encoder: ScynamoEncoder[A]): EitherNec[ScynamoEncodeError, AttributeValue] = encoder.encode(value)

  def encodedMap(implicit encoder: ObjectScynamoEncoder[A]): EitherNec[ScynamoEncodeError, util.Map[String, AttributeValue]] =
    encoder.encodeMap(value)
}

trait ScynamoEncoderDsl {
  implicit def toScynamoEncoderDslOps[A: ScynamoEncoder](value: A): ScynamoEncoderDslOps[A] = new ScynamoEncoderDslOps(value)
}
