package scynamo

import cats.data.{EitherNec}

import software.amazon.awssdk.services.dynamodb.model.{AttributeValue, GetItemResponse, QueryResponse}

class ScynamoTest extends UnitTest {
  "Scynamo" should {
    "return None if the get item response has no item" in {
      val response = GetItemResponse.builder().build()

      Scynamo.decodeGetItemResponse[Map[String, AttributeValue]](response)
    }

    "return an empty Sequence if the query response has no items" in {
      val response = QueryResponse.builder().build()

      Scynamo.decodeQueryResponse[Map[String, AttributeValue]](response)
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
      val input = Map("foo" -> "bar", "Miami" -> "Ibiza")

      val result = for {
        encodedInput <- input.encodedMap
        response                                                             = QueryResponse.builder().items(encodedInput).build()
        result: Iterable[EitherNec[ScynamoDecodeError, Map[String, String]]] = Scynamo.decodeQueryResponse[Map[String, String]](response)
      } yield result.toSeq

      print(s"Result is:  $result")
      result should ===(Right(List(Right(input))))
    }
  }
}
