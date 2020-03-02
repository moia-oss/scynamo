package scynamo

import org.scalatest.Inside
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class ScynamoDecoderTest extends UnitTest {
  "ScynamoDecoder" should {
    "support map" in {
      implicit val newDecoder: ScynamoDecoder[Int] = ScynamoDecoder.intDecoder.map(_ + 1)

      val int   = 41
      val input = AttributeValue.builder().n(s"$int").build()

      val result = ScynamoDecoder[Int].decode(input)

      result should ===(Right(42))
    }

    "support fallback" in {
      implicit val newDecoder: ScynamoDecoder[Int] = ScynamoDecoder.intDecoder.orElse(_ => Right(42))

      val input = AttributeValue.builder().nul(true).build()

      val result = ScynamoDecoder[Int].decode(input)

      result should ===(Right(42))
    }

    "return all errors" in {
      val input = AttributeValue
        .builder()
        .l(
          AttributeValue.builder().n("eins").build(),
          AttributeValue.builder().n("zwei").build(),
          AttributeValue.builder().n("drei").build()
        )
        .build()

      val result = ScynamoDecoder[Seq[Int]].decode(input)

      Inside.inside(result) {
        case Left(value) => value.toNonEmptyList.toList should have size 3
      }
    }
  }
}
