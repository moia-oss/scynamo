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

      val result = for {
        encodedInput <- input.encodedMap
        response = GetItemResponse.builder().item(encodedInput).build()
        result <- Scynamo.decodeGetItemResponse[Map[String, String]](response)
      } yield {
        result
      }

      result should ===(Right(Some(input)))
    }
  }
}
