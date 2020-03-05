package scynamo

import scynamo.generic.semiauto._
import scynamo.syntax.all._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class SemiautoDerivationTest extends UnitTest {
  "Semiauto Derivation" when {
    "deriving for an enum" should {
      "encode values as a single string" in {
        val input: Shape = Shape.Rectangle

        ScynamoEncoder[Shape].encode(input) should ===("Rectangle".toAttributeValue)
      }

      "decode values from a single string" in {
        val input: AttributeValue = "Square".toAttributeValue

        ScynamoDecoder[Shape].decode(input) should ===(Right(Shape.Square))
      }

      "encode/decode from a single string" in {
        val input = Shape.Rectangle

        val codec = ScynamoEnumCodec[Shape]

        val result = codec.decode(codec.encode(input))

        result should ===(Right(input))
      }
    }
  }

  sealed trait Shape

  object Shape {
    case object Rectangle extends Shape
    case object Square    extends Shape

    implicit val encoder: ScynamoEncoder[Shape] = deriveScynamoEnumEncoder[Shape]
    implicit val decoder: ScynamoDecoder[Shape] = deriveScynamoEnumDecoder[Shape]
  }
}
