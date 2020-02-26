package scynamo

import org.scalatest.Inspectors

class ScynamoCodecTest extends UnitTest {
  "DynamoCodec" should {
    import scynamo.generic.auto._

    "encode then decode a case class" in {
      case class Foo(someString: String, someNumber: Int)
      val input = Foo("theString", 42)

      val encoded = ObjectScynamoCodec[Foo].encode(input)
      val result  = ObjectScynamoCodec[Foo].decode(encoded)

      result should ===(Right(input))
    }

    "encode then decode a nested case class" in {
      case class Bar(someBoolean: Boolean)
      case class Foo(someString: String, someNumber: Int, someBar: Bar)

      val input = Foo("theString", 42, Bar(true))

      val encoded = ObjectScynamoCodec[Foo].encode(input)
      val result  = ObjectScynamoCodec[Foo].decode(encoded)

      result should ===(Right(input))
    }

    "encode then decode a sealed trait with two cases" in {
      sealed trait Foobar
      case class Foo(someString: String) extends Foobar
      case class Bar(someNumber: Int)    extends Foobar

      val input: Foobar = Foo("theString")

      val encoded = ObjectScynamoCodec[Foobar].encode(input)
      val result  = ObjectScynamoCodec[Foobar].decode(encoded)

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
        val encoded = ObjectScynamoCodec[Alphabet].encode(input)
        val result  = ObjectScynamoCodec[Alphabet].decode(encoded)

        result should ===(Right(input))
      }
    }

    "encode then decode a recursive ADT" in {
      sealed trait Foobar
      case class Foo(someString: String) extends Foobar
      case class Bar(someFoobar: Foobar) extends Foobar

      val input: Foobar = Bar(Foo("some-string"))

      val encoded = ObjectScynamoCodec[Foobar].encode(input)
      val result  = ObjectScynamoCodec[Foobar].decode(encoded)

      result should ===(Right(input))
    }
  }
}
