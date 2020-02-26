package scynamo.generic.semiauto

import io.moia.dynamo.{DynamoDecoder, ObjectDynamoCodec, ObjectDynamoEncoder}
import scynamo.{DynamoDecoder, ObjectDynamoCodec, ObjectDynamoEncoder}
import scynamo.generic.{GenericDynamoDecoder, GenericDynamoEncoder}
import shapeless.Lazy

package object semiauto extends Semiauto

trait Semiauto {
  def deriveDynamoEncoder[A](implicit genericEncoder: Lazy[GenericDynamoEncoder[A]]): ObjectDynamoEncoder[A] = genericEncoder.value

  def deriveDynamoDecoder[A](implicit genericDecoder: Lazy[GenericDynamoDecoder[A]]): DynamoDecoder[A] = genericDecoder.value

  def deriveDynamoCodec[A](
      implicit
      genericEncoder: GenericDynamoEncoder[A],
      genericDecoder: GenericDynamoDecoder[A]
  ): ObjectDynamoCodec[A] = ObjectDynamoCodec.fromEncoderAndDecoder(genericEncoder, genericDecoder)
}
