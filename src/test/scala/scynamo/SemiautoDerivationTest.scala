package scynamo

import org.scalatest.{Inside, Inspectors}
import scynamo.generic.{ScynamoDerivationOpts, ScynamoSealedTraitOpts}
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

        Inside.inside(encoded.decode[Map[String, AttributeValue]]) {
          case Right(value) => value.keySet should contain("thisnamehasuppercaseletters")
        }
      }

      "uses the specified discriminator" in {
        sealed trait Foobar
        case object Foo extends Foobar
        case object Bar extends Foobar

        object Foobar {
          implicit val sealedTraitOpts: ScynamoSealedTraitOpts[Foobar] = ScynamoSealedTraitOpts("my-custom-discriminator", _.toUpperCase)

          implicit val fooCodec: ObjectScynamoCodec[Foo.type]  = ObjectScynamoCodec.deriveScynamoCodec[Foo.type]
          implicit val barCodec: ObjectScynamoCodec[Bar.type]  = ObjectScynamoCodec.deriveScynamoCodec[Bar.type]
          implicit val foobarCodec: ObjectScynamoCodec[Foobar] = ObjectScynamoCodec.deriveScynamoCodec[Foobar]
        }

        Inspectors.forAll(List[Foobar](Foo, Bar)) { input =>
          val encoded = input.encodeMap
          val decoded = encoded.decode[Foobar]

          decoded should ===(Right(input))
          Inside.inside(encoded.decode[Map[String, AttributeValue]]) {
            case Right(encodedMap) =>
              Inside.inside(encodedMap.get(Foobar.sealedTraitOpts.discriminator).map(_.decode[String])) {
                case Some(Right(tag)) => tag should be("FOO").or(be("BAR"))
              }
          }
        }
      }

      "companion methods work" in {
        case class Foo(s: String)
        object Foo {
          implicit val codec: ObjectScynamoCodec[Foo] = ObjectScynamoCodec.deriveScynamoCodec[Foo]
        }

        val input = Foo("test")

        val result = input.encode.decode[Foo]

        result should ===(Right(input))
      }
    }

    "deriving for an enum" should {
      "encode values as a single string" in {
        val input: Shape = Shape.Rectangle

        ScynamoEncoder[Shape].encode(input) should ===("Rectangle".encode)
      }

      "decode values from a single string" in {
        val input: AttributeValue = "Square".encode

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
