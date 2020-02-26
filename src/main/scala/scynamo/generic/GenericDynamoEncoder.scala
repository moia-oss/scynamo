package scynamo.generic

import io.moia.dynamo.ObjectDynamoEncoder
import scynamo.ObjectDynamoEncoder
import shapeless.{LabelledGeneric, Lazy}

trait GenericDynamoEncoder[A] extends ObjectDynamoEncoder[A]

object GenericDynamoEncoder extends GenericDynamoEncoderInstances

trait GenericDynamoEncoderInstances {
  implicit def deriveEncoderInstance[F, G](
      implicit gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[ShapelessDynamoEncoder[G]]
  ): GenericDynamoEncoder[F] =
    value => sg.value.encodeMap(gen.to(value))
}
