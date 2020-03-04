package scynamo.dsl.encoder

import scynamo.ScynamoEncoder
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class ScynamoEncoderDslOps[A](val value: A) extends AnyVal {
  def toAttributeValue(implicit encoder: ScynamoEncoder[A]): AttributeValue = encoder.encode(value)
}

trait ScynamoEncoderDsl {
  implicit def toScynamoEncoderDslOps[A: ScynamoEncoder](value: A): ScynamoEncoderDslOps[A] = new ScynamoEncoderDslOps(value)
}
