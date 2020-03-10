package scynamo.syntax.decoder

import cats.data.EitherNec
import scynamo.{ObjectScynamoDecoder, ScynamoDecodeError, ScynamoDecoder}
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class ScynamoDecoderDslOps(val attributeValue: AttributeValue) extends AnyVal {
  def decode[A: ScynamoDecoder]: EitherNec[ScynamoDecodeError, A] = ScynamoDecoder[A].decode(attributeValue)
}

class ObjectScynamoDecoderDslOps(val attributeValueMap: java.util.Map[String, AttributeValue]) extends AnyVal {
  def decode[A: ObjectScynamoDecoder]: EitherNec[ScynamoDecodeError, A] =
    ObjectScynamoDecoder[A].decodeMap(attributeValueMap)
}

trait ScynamoDecoderDsl {
  implicit def toScynamoDecoderDsl(attributeValue: AttributeValue): ScynamoDecoderDslOps = new ScynamoDecoderDslOps(attributeValue)

  implicit def toObjectScynamoDecoderDsl(attributeValueMap: java.util.Map[String, AttributeValue]): ObjectScynamoDecoderDslOps =
    new ObjectScynamoDecoderDslOps(attributeValueMap)
}
