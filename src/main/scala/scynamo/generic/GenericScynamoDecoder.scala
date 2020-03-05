package scynamo.generic

import scynamo.ObjectScynamoDecoder
import shapeless.{LabelledGeneric, Lazy}

trait GenericScynamoDecoder[A] extends ObjectScynamoDecoder[A]

object GenericScynamoDecoder extends GenericscynamoDecoderInstances

trait GenericscynamoDecoderInstances {
  implicit def deriveDecoderInstance[F, G](
      implicit gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[ShapelessScynamoDecoder[F, G]]
  ): GenericScynamoDecoder[F] =
    value => sg.value.decodeMap(value).map(gen.from)
}
