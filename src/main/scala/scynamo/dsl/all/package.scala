package scynamo.dsl

import scynamo.dsl.attributevalue.AttributeValueDsl
import scynamo.dsl.decoder.ScynamoDecoderDsl
import scynamo.dsl.encoder.ScynamoEncoderDsl

package object all extends ScynamoEncoderDsl with ScynamoDecoderDsl with AttributeValueDsl
