package scynamo

import cats.data.NonEmptyChain
import org.scalatest.Inspectors
import scynamo.syntax.encoder._

class ScynamoCodecTest extends UnitTest {
  "ScynamoCodec" should {
    import scynamo.generic.auto._

    "encode then decode a case class" in {
      case class Foo(someString: String, someNumber: Int)
      val input = Foo("theString", 42)

      val result = for {
        encoded <- ObjectScynamoCodec[Foo].encode(input)
        decoded <- ObjectScynamoCodec[Foo].decode(encoded)
      } yield decoded

      result should ===(Right(input))
    }

    "encode then decode a nested case class" in {
      case class Bar(someBoolean: Boolean)
      case class Foo(someString: String, someNumber: Int, someBar: Bar)

      val input = Foo("theString", 42, Bar(true))

      val result = for {
        encoded <- ObjectScynamoCodec[Foo].encode(input)
        decoded <- ObjectScynamoCodec[Foo].decode(encoded)
      } yield decoded

      result should ===(Right(input))
    }

    "encode then decode a sealed trait with two cases" in {
      sealed trait Foobar
      case class Foo(someString: String) extends Foobar
      case class Bar(someNumber: Int)    extends Foobar

      val input: Foobar = Foo("theString")

      val result = for {
        encoded <- ObjectScynamoCodec[Foobar].encode(input)
        result  <- ObjectScynamoCodec[Foobar].decode(encoded)
      } yield result

      result should ===(Right(input))
    }

    "encode then decode a sealed trait with many cases" in {

      sealed trait Alphabet
      case object A            extends Alphabet
      case object B            extends Alphabet
      case object C            extends Alphabet
      case object D            extends Alphabet
      case object E            extends Alphabet
      case object F            extends Alphabet
      case object G            extends Alphabet
      case class H(value: Int) extends Alphabet

      Inspectors.forAll(List[Alphabet](A, B, C, D, E, F, G, H(42))) { input =>
        val result = for {
          encoded <- ObjectScynamoCodec[Alphabet].encode(input)
          result  <- ObjectScynamoCodec[Alphabet].decode(encoded)
        } yield result

        result should ===(Right(input))
      }
    }

    "encode then decode a recursive ADT" in {
      sealed trait Foobar
      case class Foo(someString: String) extends Foobar
      case class Bar(someFoobar: Foobar) extends Foobar

      val input: Foobar = Bar(Foo("some-string"))

      val result = for {
        encoded <- ObjectScynamoCodec[Foobar].encode(input)
        result  <- ObjectScynamoCodec[Foobar].decode(encoded)
      } yield result

      result should ===(Right(input))
    }

    "support transformation" in {
      sealed trait Foobar
      case object Foo extends Foobar
      case object Bar extends Foobar

      val error = NonEmptyChain.one(ScynamoDecodeError.generalError(s"Unknown tag", None))
      val codec = ScynamoCodec[String].itransform[Foobar] {
        case Left(value) => Left(value)
        case Right(value) =>
          value match {
            case "Foo" => Right(Foo)
            case "Bar" => Right(Bar)
            case _     => Left(error)
          }
      }(_.toString)

      codec.encode(Bar) should ===("Bar".encoded)

      "Baz".encoded.flatMap(codec.decode) should matchPattern { case Left(`error`) => }
    }
  }
}
