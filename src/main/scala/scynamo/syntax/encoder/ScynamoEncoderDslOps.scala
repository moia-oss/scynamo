package scynamo.syntax.encoder

import java.util

import scynamo.{ObjectScynamoEncoder, ScynamoEncoder}
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class ScynamoEncoderDslOps[A](val value: A) extends AnyVal {
  def encoded(implicit encoder: ScynamoEncoder[A]): AttributeValue                            = encoder.encode(value)
  def encodedMap(implicit encoder: ObjectScynamoEncoder[A]): util.Map[String, AttributeValue] = encoder.encodeMap(value)
}

trait ScynamoEncoderDsl {
  implicit def toScynamoEncoderDslOps[A: ScynamoEncoder](value: A): ScynamoEncoderDslOps[A] = new ScynamoEncoderDslOps(value)
}
