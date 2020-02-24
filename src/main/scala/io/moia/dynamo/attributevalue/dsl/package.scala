package io.moia.dynamo.attributevalue

import io.moia.dynamo.attributevalue.dsl.AttributeValueOps
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

package object dsl extends AttributeValueDsl

trait AttributeValueDsl {
  implicit def toAttributeValueOps(attributeValue: AttributeValue): AttributeValueOps = new AttributeValueOps(attributeValue)
}
