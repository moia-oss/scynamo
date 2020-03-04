package scynamo

import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class DslTest extends UnitTest {
  "DSL" should {
    "allow decoding" in {
      import scynamo.dsl.decoder._

      val result = AttributeValue.builder().s("some-string").build().fromAttributeValue[String]

      result should ===(Right("some-string"))
    }

    "allow encoding" in {
      import scynamo.dsl.encoder._

      val result = "some-string".toAttributeValue

      result.s should ===("some-string")
    }

    "allow option access to fields in AttributeValue" in {
      import scynamo.dsl.attributevalue._

      val input = AttributeValue.builder().build()

      val result = input.sOpt

      result should ===(None)
    }
  }
}
