package scynamo.generic

import cats.syntax.either._
import scynamo.ScynamoType.ScynamoMap
import scynamo.{ScynamoDecoder, TypeMismatch}
import shapeless.{LabelledGeneric, Lazy}
import scynamo.attributevalue.dsl._

trait GenericScynamoDecoder[A] extends ScynamoDecoder[A]

object GenericScynamoDecoder extends GenericscynamoDecoderInstances

trait GenericscynamoDecoderInstances {

  implicit def deriveDecoderInstance[F, G](
      implicit gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[ShapelessScynamoDecoder[G]]
  ): GenericScynamoDecoder[F] =
    value => {
      value.mOpt match {
        case Some(hm) => sg.value.decodeMap(hm).map(gen.from)
        case None     => Either.leftNec(TypeMismatch(ScynamoMap, value))
      }
    }
}
