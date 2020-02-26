package scynamo.attributevalue

import scynamo.attributevalue.dsl.AttributeValueOps
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

package object dsl extends AttributeValueDsl

trait AttributeValueDsl {
  implicit def toAttributeValueOps(attributeValue: AttributeValue): AttributeValueOps = new AttributeValueOps(attributeValue)
}
