package scynamo

import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class ScynamoDecoderTest extends UnitTest {
  "ScynamoDecoder" should {
    "support map" in {
      implicit val newDecoder: ScynamoDecoder[Int] = ScynamoDecoder.numericDecoder[Int].map(_ + 1)

      val int   = 41
      val input = AttributeValue.builder().n(s"$int").build()

      val result = ScynamoDecoder[Int].decode(input)

      result should ===(Right(42))
    }

    "support fallback" in {
      implicit val newDecoder: ScynamoDecoder[Int] = ScynamoDecoder.numericDecoder[Int].orElse(_ => Right(42))

      val input = AttributeValue.builder().nul(true).build()

      val result = ScynamoDecoder[Int].decode(input)

      result should ===(Right(42))
    }
  }
}
