package scynamo.generic

import scynamo.{ObjectScynamoCodec, ObjectScynamoDecoder, ObjectScynamoEncoder, ScynamoDecoder, ScynamoEncoder, ScynamoEnumCodec}
import shapeless.Lazy

package object semiauto extends Semiauto

trait Semiauto {
  def deriveScynamoEncoder[A](implicit genericEncoder: Lazy[GenericScynamoEncoder[A]]): ObjectScynamoEncoder[A] = genericEncoder.value

  def deriveScynamoEnumEncoder[A](implicit genericEnumEncoder: Lazy[GenericScynamoEnumEncoder[A]]): ScynamoEncoder[A] =
    genericEnumEncoder.value

  def deriveScynamoDecoder[A](implicit genericDecoder: Lazy[GenericScynamoDecoder[A]]): ObjectScynamoDecoder[A] = genericDecoder.value

  def deriveScynamoEnumDecoder[A](implicit genericEnumDecoder: Lazy[GenericScynamoEnumDecoder[A]]): ScynamoDecoder[A] =
    genericEnumDecoder.value

  def deriveScynamoCodec[A](
      implicit
      genericEncoder: GenericScynamoEncoder[A],
      genericDecoder: GenericScynamoDecoder[A]
  ): ObjectScynamoCodec[A] = ObjectScynamoCodec.fromEncoderAndDecoder(genericEncoder, genericDecoder)

  def deriveScynamoEnumCodec[A](
      implicit genericEncoder: GenericScynamoEnumEncoder[A],
      genericDecoder: GenericScynamoEnumDecoder[A]
  ): ScynamoEnumCodec[A] = ScynamoEnumCodec.fromEncoderAndDecoder(genericEncoder, genericDecoder)
}
