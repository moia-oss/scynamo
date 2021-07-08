package scynamo.generic

import scynamo.ObjectScynamoEncoder
import shapeless.{LabelledGeneric, Lazy}

import java.util.Collections

trait GenericScynamoEncoder[A] extends ObjectScynamoEncoder[A]

object GenericScynamoEncoder extends GenericScynamoEncoderInstances

trait GenericScynamoEncoderInstances {
  implicit def deriveEncoderInstance[F, G](implicit
      gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[ShapelessScynamoEncoder[F, G]]
  ): GenericScynamoEncoder[F] =
    value => sg.value.encodeMap(gen.to(value)).map(Collections.unmodifiableMap(_))
}
