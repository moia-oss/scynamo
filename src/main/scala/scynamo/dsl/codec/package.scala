package scynamo.dsl

import scynamo.dsl.decoder.ScynamoDecoderDsl
import scynamo.dsl.encoder.ScynamoEncoderDsl

package object codec extends ScynamoEncoderDsl with ScynamoDecoderDsl
