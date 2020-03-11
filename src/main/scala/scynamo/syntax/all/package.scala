package scynamo.syntax

import scynamo.syntax.attributevalue.AttributeValueDsl
import scynamo.syntax.decoder.ScynamoDecoderDsl
import scynamo.syntax.encoder.ScynamoEncoderDsl

package object all extends ScynamoEncoderDsl with ScynamoDecoderDsl with AttributeValueDsl
