package scynamo

import org.scalatest.Inside
import scynamo.generic.ScynamoDerivationOpts
import scynamo.generic.semiauto._
import scynamo.syntax.all._
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class SemiautoDerivationTest extends UnitTest {
  "Semiauto Derivation" when {
    "providing custom derivation options" should {
      "field names are transformed" in {
        case class Foo(thisNameHasUpperCaseLetters: String)
        object Foo {
          implicit val scynamoDerivationOpts: ScynamoDerivationOpts[Foo] =
            ScynamoDerivationOpts(_.toLowerCase)

          implicit val scynamoCodec: ObjectScynamoCodec[Foo] = deriveScynamoCodec[Foo]
        }

        val input = Foo("test")

        val encoded = ObjectScynamoCodec[Foo].encode(input)
        val decoded = ObjectScynamoCodec[Foo].decode(encoded)

        decoded should ===(Right(input))

        Inside.inside(encoded.fromAttributeValue[Map[String, AttributeValue]]) {
          case Right(value) => value.keySet should contain("thisnamehasuppercaseletters")
        }
      }
    }

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
