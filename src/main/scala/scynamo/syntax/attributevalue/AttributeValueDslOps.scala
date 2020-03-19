package scynamo.syntax.attributevalue

import java.util

import cats.syntax.either._
import cats.data.NonEmptyChain
import scynamo.ScynamoDecodeError.TypeMismatch
import scynamo.{ScynamoDecodeError, ScynamoType}
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait AttributeValueDsl {
  implicit def toAttributeValueOps(attributeValue: AttributeValue): AttributeValueDslOps = new AttributeValueDslOps(attributeValue)
}

class AttributeValueDslOps(val attributeValue: AttributeValue) extends AnyVal {
  private[this] def nulOpt: Option[Boolean]                        = Option(attributeValue.nul).map(_.booleanValue)
  private[this] def bOpt: Option[SdkBytes]                         = Option(attributeValue.b)
  private[this] def nOpt: Option[String]                           = Option(attributeValue.n)
  private[this] def sOpt: Option[String]                           = Option(attributeValue.s)
  private[this] def boolOpt: Option[Boolean]                       = Option(attributeValue.bool).map(_.booleanValue)
  private[this] def mOpt: Option[util.Map[String, AttributeValue]] = if (attributeValue.hasM) Some(attributeValue.m) else None
  private[this] def lOpt: Option[util.List[AttributeValue]]        = if (attributeValue.hasL) Some(attributeValue.l) else None
  private[this] def bsOpt: Option[util.List[SdkBytes]]             = if (attributeValue.hasBs) Some(attributeValue.bs) else None
  private[this] def nsOpt: Option[util.List[String]]               = if (attributeValue.hasNs) Some(attributeValue.ns) else None
  private[this] def ssOpt: Option[util.List[String]]               = if (attributeValue.hasSs) Some(attributeValue.ss) else None

  def asOption[A, R](typ: ScynamoType.Aux[R]): Option[R] = typ match {
    case ScynamoType.Null      => nulOpt
    case ScynamoType.Bool      => boolOpt
    case ScynamoType.String    => sOpt
    case ScynamoType.Number    => nOpt
    case ScynamoType.Binary    => bOpt
    case ScynamoType.Map       => mOpt
    case ScynamoType.List      => lOpt
    case ScynamoType.StringSet => ssOpt
    case ScynamoType.NumberSet => nsOpt
    case ScynamoType.BinarySet => bsOpt
  }

  def asEither[A, R](typ: ScynamoType.Aux[R]): Either[NonEmptyChain[TypeMismatch], R] =
    attributeValue.asOption(typ) match {
      case None        => Either.leftNec(ScynamoDecodeError.typeMismatch(typ, attributeValue))
      case Some(value) => Right(value)
    }
}
