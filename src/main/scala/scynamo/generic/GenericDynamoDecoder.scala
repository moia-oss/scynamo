package scynamo.generic

import cats.syntax.either._
import io.moia.dynamo.DynamoType.DynamoMap
import io.moia.dynamo.{DynamoDecoder, TypeMismatch}
import scynamo.{DynamoDecoder, TypeMismatch}
import scynamo.DynamoType.DynamoMap
import shapeless.{LabelledGeneric, Lazy}

trait GenericDynamoDecoder[A] extends DynamoDecoder[A]

object GenericDynamoDecoder extends GenericDynamoDecoderInstances

trait GenericDynamoDecoderInstances {
  import io.moia.dynamo.attributevalue.dsl._

  implicit def deriveDecoderInstance[F, G](
      implicit gen: LabelledGeneric.Aux[F, G],
      sg: Lazy[ShapelessDynamoDecoder[G]]
  ): GenericDynamoDecoder[F] =
    value => {
      value.mOpt match {
        case Some(hm) => sg.value.decodeMap(hm).map(gen.from)
        case None     => Either.leftNec(TypeMismatch(DynamoMap, value))
      }
    }
}
