package scynamo.syntax.attributevalue

import java.util

import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait AttributeValueDsl {
  implicit def toAttributeValueOps(attributeValue: AttributeValue): AttributeValueDslOps = new AttributeValueDslOps(attributeValue)
}

class AttributeValueDslOps(val attributeValue: AttributeValue) extends AnyVal {
  def bOpt: Option[SdkBytes]                         = Option(attributeValue.b)
  def nOpt: Option[String]                           = Option(attributeValue.n)
  def sOpt: Option[String]                           = Option(attributeValue.s)
  def boolOpt: Option[Boolean]                       = Option(attributeValue.bool).map(_.booleanValue)
  def mOpt: Option[util.Map[String, AttributeValue]] = if (attributeValue.hasM) Some(attributeValue.m) else None
  def lOpt: Option[util.List[AttributeValue]]        = if (attributeValue.hasL) Some(attributeValue.l) else None
  def bsOpt: Option[util.List[SdkBytes]]             = if (attributeValue.hasBs) Some(attributeValue.bs) else None
  def nsOpt: Option[util.List[String]]               = if (attributeValue.hasNs) Some(attributeValue.ns) else None
  def ssOpt: Option[util.List[String]]               = if (attributeValue.hasSs) Some(attributeValue.ss) else None
}
