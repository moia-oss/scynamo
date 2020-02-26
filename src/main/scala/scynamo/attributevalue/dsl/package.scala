package scynamo.attributevalue.dsl

import software.amazon.awssdk.services.dynamodb.model.AttributeValue

package object dsl extends AttributeValueDsl

trait AttributeValueDsl {
  implicit def toAttributeValueOps(attributeValue: AttributeValue): AttributeValueOps = new AttributeValueOps(attributeValue)
}
