package scynamo.dsl.decoder

import cats.data.EitherNec
import scynamo.{ScynamoDecodeError, ScynamoDecoder}
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class ScynamoDecoderDslOps(val attributeValue: AttributeValue) extends AnyVal {
  def fromAttributeValue[A: ScynamoDecoder]: EitherNec[ScynamoDecodeError, A] = ScynamoDecoder[A].decode(attributeValue)
}

trait ScynamoDecoderDsl {
  implicit def toScynamoDecoderDsl(attributeValue: AttributeValue): ScynamoDecoderDslOps = new ScynamoDecoderDslOps(attributeValue)
}
