package scynamo.generic

import scynamo.{ObjectScynamoCodec, ObjectScynamoDecoder, ObjectScynamoEncoder, ScynamoDecoder, ScynamoEncoder, ScynamoEnumCodec}
import shapeless.Lazy

package object semiauto extends SemiautoDerivation

trait SemiautoDerivation extends SemiautoDerivationEncoder with SemiautoDerivationDecoder with SemiautoDerivationCodec

trait SemiautoDerivationEncoder {
  def deriveScynamoEncoder[A](
      implicit genericEncoder: Lazy[GenericScynamoEncoder[A]]
  ): ObjectScynamoEncoder[A] = genericEncoder.value

  def deriveScynamoEnumEncoder[A](implicit genericEnumEncoder: GenericScynamoEnumEncoder[A]): ScynamoEncoder[A] =
    genericEnumEncoder

}

trait SemiautoDerivationDecoder {
  def deriveScynamoDecoder[A](
      implicit genericDecoder: Lazy[GenericScynamoDecoder[A]]
  ): ObjectScynamoDecoder[A] = genericDecoder.value

  def deriveScynamoEnumDecoder[A](implicit genericEnumDecoder: GenericScynamoEnumDecoder[A]): ScynamoDecoder[A] =
    genericEnumDecoder
}

trait SemiautoDerivationCodec {
  def deriveScynamoCodec[A](
      implicit
      genericEncoder: Lazy[GenericScynamoEncoder[A]],
      genericDecoder: Lazy[GenericScynamoDecoder[A]]
  ): ObjectScynamoCodec[A] = ObjectScynamoCodec.fromEncoderAndDecoder(genericEncoder.value, genericDecoder.value)

  def deriveScynamoEnumCodec[A](
      implicit genericEncoder: GenericScynamoEnumEncoder[A],
      genericDecoder: GenericScynamoEnumDecoder[A]
  ): ScynamoEnumCodec[A] = ScynamoEnumCodec.fromEncoderAndDecoder(genericEncoder, genericDecoder)
}
