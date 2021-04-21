package scynamo.generic

import cats.data.EitherNec
import cats.syntax.apply._
import cats.syntax.either._
import scynamo.StackFrame.{Attr, Case}
import scynamo._
import shapeless._
import shapeless.labelled._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import java.util

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
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :: T] =
    value => {
      val fieldName      = opts.transform(key.value.name)
      val fieldAttrValue = Option(value.get(fieldName))

      val decodedHead = (fieldAttrValue, sv.defaultValue) match {
        case (Some(field), _)      => sv.decode(field).leftMap(_.map(_.push(Attr(fieldName))))
        case (None, Some(default)) => Right(default)
        case (None, None)          => Either.leftNec(ScynamoDecodeError.missingField(fieldName, value))
      }

      (decodedHead, st.decodeMap(value)).mapN(_ :: _)
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
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :+: T] = new ShapelessScynamoDecoder[Base, FieldType[K, V] :+: T] {
    lazy val tagged = deriveCConsTagged[Base, K, V, T]
    lazy val nested = deriveCConsNested[Base, K, V, T]
    override def decodeMap(value: util.Map[String, AttributeValue]) =
      (if (value.containsKey(opts.discriminator)) tagged else nested).decodeMap(value)
  }

  def deriveCConsTagged[Base, K <: Symbol, V, T <: Coproduct](implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: ShapelessScynamoDecoder[Base, T],
      opts: ScynamoSealedTraitOpts[Base]
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :+: T] =
    value => {
      val name = opts.transform(key.value.name)
      for {
        typeTagAttrValue <- Option(value.get(opts.discriminator))
          .map(Right(_))
          .getOrElse(Either.leftNec(ScynamoDecodeError.missingField(name, value)))
        typeTag <- typeTagAttrValue.asEither(ScynamoType.String)
        result <-
          if (name == typeTag)
            sv.value.decode(AttributeValue.builder().m(value).build()).map(v => Inl(field[K](v))).leftMap(_.map(_.push(Case(name))))
          else
            st.decodeMap(value).map(Inr(_))
      } yield result
    }

  def deriveCConsNested[Base, K <: Symbol, V, T <: Coproduct](implicit
      key: Witness.Aux[K],
      sv: Lazy[ScynamoDecoder[V]],
      st: ShapelessScynamoDecoder[Base, T],
      opts: ScynamoSealedTraitOpts[Base]
  ): ShapelessScynamoDecoder[Base, FieldType[K, V] :+: T] =
    value => {
      val name = opts.transform(key.value.name)
      Option(value.get(name)) match {
        case Some(nestedValue) => sv.value.decode(nestedValue).map(v => Inl(field[K](v))).leftMap(_.map(_.push(Case(name))))
        case _                 => st.decodeMap(value).map(Inr(_))
      }
    }

}
