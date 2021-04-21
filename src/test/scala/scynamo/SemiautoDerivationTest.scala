package scynamo

import cats.data.EitherNec
import org.scalatest.{Inside, Inspectors}
import scynamo.Mixed.{CaseClass, CaseObject}
import scynamo.generic.{ScynamoDerivationOpts, ScynamoSealedTraitOpts}
import scynamo.generic.semiauto._
import scynamo.syntax.all._
import shapeless.Witness
import shapeless.labelled.FieldType
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
        val decoded = encoded.flatMap(ObjectScynamoCodec[Foo].decode)

        decoded should ===(Right(input))

        Inside.inside(encoded.flatMap(_.decode[Map[String, AttributeValue]])) { case Right(value) =>
          value.keySet should contain("thisnamehasuppercaseletters")
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
          val encoded = input.encodedMap
          val decoded = encoded.flatMap(_.decode[Foobar])

          decoded should ===(Right(input))
          Inside.inside(encoded.flatMap(_.decode[Map[String, AttributeValue]])) { case Right(encodedMap) =>
            Inside.inside(encodedMap.get(Foobar.sealedTraitOpts.discriminator).map(_.decode[String])) { case Some(Right(tag)) =>
              tag should be("FOO").or(be("BAR"))
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

        val result = input.encoded.flatMap(_.decode[Foo])

        result should ===(Right(input))
      }

      "work if the sealed trait contains case classes and case objects" in {
        import scynamo.syntax.encoder._

        implicit val codec: ObjectScynamoCodec[Mixed] = deriveScynamoCodec[Mixed]

        val result = List[Mixed](CaseObject, CaseClass(42)).encoded

        Inside.inside(result) { case Right(av) =>
          av.l.size should ===(2)
        }
      }
    }

    "deriving for an enum" should {
      "encode values as a single string" in {
        val input: Shape = Shape.Rectangle

        ScynamoEncoder[Shape].encode(input) should ===("Rectangle".encoded)
      }

      "decode values from a single string" in {
        val input: EitherNec[ScynamoEncodeError, AttributeValue] = "Square".encoded

        input.flatMap(ScynamoDecoder[Shape].decode) should ===(Right(Shape.Square))
      }

      "encode/decode from a single string" in {
        val input = Shape.Rectangle

        val codec = ScynamoEnumCodec[Shape]

        val result = codec.encode(input).flatMap(codec.decode)

        result should ===(Right(input))
      }

      "fail if the trait contains non case-objects" in {
        """
          |deriveScynamoEnumCodec[Mixed]
          |""".stripMargin shouldNot compile
      }
    }

    "a custom field type encoder or decoder is provided" should {
      "override the default one" in {
        final case class Apple(variety: String, ripe: Boolean)
        object Apple {
          val variety = Witness(Symbol("variety"))
          val ripe    = Witness(Symbol("ripe"))

          implicit val varietyEncoder: ScynamoEncoder[FieldType[variety.T, String]] =
            ScynamoEncoder.fieldEncoder(ScynamoEncoder.stringEncoder.contramap[String](_.trim))

          implicit val ripeDecoder: ScynamoDecoder[FieldType[ripe.T, Boolean]] =
            ScynamoDecoder.fieldDecoder(ScynamoDecoder.booleanDecoder.withDefault(true))

          implicit val codec: ObjectScynamoCodec[Apple] =
            ObjectScynamoCodec.deriveScynamoCodec[Apple]
        }

        val encoded = Apple("\tGranny Smith\n", ripe = false).encoded.flatMap(_.decode[Apple])
        val decoded = Map("variety" -> "Pink Lady".encodedUnsafe).encodedUnsafe.decode[Apple]
        encoded should ===(Right(Apple("Granny Smith", ripe = false)))
        decoded should ===(Right(Apple("Pink Lady", ripe = true)))
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

sealed trait Mixed
object Mixed {
  implicit val caseObjectCodec: ObjectScynamoCodec[CaseObject.type] = deriveScynamoCodec[CaseObject.type]
  case object CaseObject           extends Mixed
  case class CaseClass(value: Int) extends Mixed
  object CaseClass {
    implicit val caseClassCodec: ObjectScynamoCodec[CaseClass] = deriveScynamoCodec[CaseClass]
  }
}
