package scynamo

import software.amazon.awssdk.services.dynamodb.model.{AttributeValue, GetItemResponse, QueryResponse}

class ScynamoTest extends UnitTest {
  "Scynamo" should {
    "return None if the get item response has no item" in {
      val response = GetItemResponse.builder().build()
      val result = for {
        result <- Scynamo.decodeGetItemResponse[Map[String, AttributeValue]](response)
      } yield result

      result should ===(Right(None))
    }

    "return an empty List if the query response has no items" in {
      val response = QueryResponse.builder().build()

      val result = for {
        result <- Scynamo.decodeQueryResponse[Map[String, AttributeValue]](response)
      } yield result

      result should ===(Right(List.empty))
    }

    "return the decoded result if it has an item that is well formed" in {
      import scynamo.syntax.encoder._
      val input = Map("foo" -> "bar")

      val result = for {
        encodedInput <- input.encodedMap
        response = GetItemResponse.builder().item(encodedInput).build()
        result <- Scynamo.decodeGetItemResponse[Map[String, String]](response)
      } yield result

      result should ===(Right(Some(input)))
    }

    "return the decoded result if it has multiple items that are well formed" in {
      import scynamo.syntax.encoder._
      val input1 = Map("foo" -> "bar")
      val input2 = Map("Miami" -> "Ibiza")

      val result = for {
        encodedInput1 <- input1.encodedMap
        encodedInput2 <- input2.encodedMap
        response = QueryResponse.builder().items(encodedInput1, encodedInput2).build()
        result <- Scynamo.decodeQueryResponse[Map[String, String]](response)
      } yield result
      result should ===(Right(List(input1, input2)))
    }
  }
}
