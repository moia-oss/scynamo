package scynamo.generic

import scynamo.{ObjectScynamoCodec, ObjectScynamoDecoder, ObjectScynamoEncoder}
import shapeless.Lazy

package object semiauto extends Semiauto

trait Semiauto {
  def deriveScynamoEncoder[A](implicit genericEncoder: Lazy[GenericScynamoEncoder[A]]): ObjectScynamoEncoder[A] = genericEncoder.value

  def deriveScynamoDecoder[A](implicit genericDecoder: Lazy[GenericScynamoDecoder[A]]): ObjectScynamoDecoder[A] = genericDecoder.value

  def deriveScynamoCodec[A](
      implicit
      genericEncoder: GenericScynamoEncoder[A],
      genericDecoder: GenericScynamoDecoder[A]
  ): ObjectScynamoCodec[A] = ObjectScynamoCodec.fromEncoderAndDecoder(genericEncoder, genericDecoder)
}
