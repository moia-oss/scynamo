package scynamo.generic

import cats.data.EitherNec
import cats.syntax.all._
import scynamo.StackFrame.{Attr, Case}
import scynamo._
import shapeless._
import shapeless.labelled._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait ShapelessScynamoDecoder[Base, A] {
  def decodeMap(value: java.util.Map[String, AttributeValue]): EitherNec[ScynamoDecodeError, A]
}

object ShapelessScynamoDecoder extends DecoderHListInstances with DecoderCoproductInstances

trait DecoderHListInstances extends ScynamoDecoderFunctions {
  implicit def deriveHNil[Base]: ShapelessScynamoDecoder[Base, HNil] = _ => Right(HNil)

  implicit def deriveHCons[Base, K <: Symbol, V, T <: HList](implicit
      key: Witness.Aux[K],
      sv: ScynamoDecoder[FieldType[K, V]],
      st: ShapelessScynamoDecoder[Base, T],
      opts: ScynamoDerivationOpts[Base] = ScynamoDerivationOpts.default[Base]
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :: T] = { attributes =>
    val fieldName      = opts.transform(key.value.name)
    val fieldAttrValue = Option(attributes.get(fieldName))
    val decodedHead    = (fieldAttrValue, sv.defaultValue) match {
      case (Some(field), _)      => StackFrame.decoding(sv.decode(field), Attr(fieldName))
      case (None, Some(default)) => Right(default)
      case (None, None)          => Either.leftNec(ScynamoDecodeError.missingField(fieldName, attributes))
    }

    (decodedHead, st.decodeMap(attributes)).mapN(_ :: _)
  }
}

trait DecoderCoproductInstances extends ScynamoDecoderFunctions {
  import scynamo.syntax.attributevalue._

  implicit def deriveCNil[Base]: ShapelessScynamoDecoder[Base, CNil] =
    value => Either.leftNec(ScynamoDecodeError.invalidCoproductCaseMap(value))

  implicit def deriveCCons[Base, K <: Symbol, V, T <: Coproduct](implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: ShapelessScynamoDecoder[Base, T],
      opts: ScynamoSealedTraitOpts[Base] = ScynamoSealedTraitOpts.default[Base]
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :+: T] = {
    lazy val tagged = deriveCConsTagged[Base, K, V, T]
    lazy val nested = deriveCConsNested[Base, K, V, T]
    attributes => (if (attributes.containsKey(opts.discriminator)) tagged else nested).decodeMap(attributes)
  }

  def deriveCConsTagged[Base, K <: Symbol, V, T <: Coproduct](implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: ShapelessScynamoDecoder[Base, T],
      opts: ScynamoSealedTraitOpts[Base]
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :+: T] = { attributes =>
    val name = opts.transform(key.value.name)
    for {
      typeTagAttrValue <- Option(attributes.get(opts.discriminator)).toRightNec(ScynamoDecodeError.missingField(name, attributes))
      typeTag          <- typeTagAttrValue.asEither(ScynamoType.String)
      result           <-
        if (name == typeTag) {
          val attr = AttributeValue.builder.m(attributes).build()
          StackFrame.decoding(sv.value.decode(attr).map(v => Inl(field[K](v))), Case(name))
        } else {
          st.decodeMap(attributes).map(Inr.apply)
        }
    } yield result
  }

  def deriveCConsNested[Base, K <: Symbol, V, T <: Coproduct](implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: ShapelessScynamoDecoder[Base, T],
      opts: ScynamoSealedTraitOpts[Base]
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :+: T] = { attributes =>
    val name = opts.transform(key.value.name)
    Option(attributes.get(name)) match {
      case Some(attr) => StackFrame.decoding(sv.value.decode(attr).map(v => Inl(field[K](v))), Case(name))
      case _          => st.decodeMap(attributes).map(Inr.apply)
    }
  }

}
