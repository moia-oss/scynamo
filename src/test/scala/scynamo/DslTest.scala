package scynamo

import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class DslTest extends UnitTest {
  "DSL" should {
    "allow decoding" in {
      import scynamo.syntax.decoder._

      val result = AttributeValue.builder().s("some-string").build().decode[String]

      result should ===(Right("some-string"))
    }

    "allow encoding" in {
      import scynamo.syntax.encoder._

      val result = "some-string".encoded

      result.map(_.s) should ===(Right("some-string"))
    }

    "allow option access to fields in AttributeValue" in {
      import scynamo.syntax.attributevalue._

      val input = AttributeValue.builder().build()

      val result = input.asOption(ScynamoType.Bool)

      result should ===(None)
    }

    "throw an exception with unsafe encoding" in {
      import scynamo.syntax.encoder._

      val input = List("foo", "", "bar")

      an[IllegalArgumentException] should be thrownBy {
        input.encodedUnsafe
      }
    }
  }
}
