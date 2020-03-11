package scynamo.syntax

import scynamo.syntax.decoder.ScynamoDecoderDsl
import scynamo.syntax.encoder.ScynamoEncoderDsl

package object codec extends ScynamoEncoderDsl with ScynamoDecoderDsl
