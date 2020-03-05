package scynamo

import software.amazon.awssdk.services.dynamodb.model.{AttributeValue, GetItemResponse}

class ScynamoTest extends UnitTest {
  "Scynamo" should {
    "return None if the response has no item" in {
      val response = GetItemResponse.builder().build()

      Scynamo.decodeGetItemResponse[Map[String, AttributeValue]](response)
    }

    "return the decoded result if it has an item that is well formed" in {
      import scynamo.syntax.encoder._
      val input = Map("foo" -> "bar")

      val response = GetItemResponse.builder().item(input.toAttributeValueMap).build()

      val result = Scynamo.decodeGetItemResponse[Map[String, String]](response)

      result should ===(Some(Right(input)))
    }
  }
}
