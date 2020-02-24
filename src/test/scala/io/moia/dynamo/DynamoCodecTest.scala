package io.moia.dynamo

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DynamoCodecTest extends AnyWordSpec with Matchers with TypeCheckedTripleEquals {
  "DynamoCodec" should {
    "encode then decode a case class" in {
      case class Foo(someString: String, someNumber: Int)
      val input = Foo("theString", 42)

      val encoded = GenericDynamoCodec[Foo].encode(input)
      val result  = GenericDynamoCodec[Foo].decode(encoded)

      result should ===(Right(input))
    }

    "encode then decode a nested case class" in {
      case class Bar(someBoolean: Boolean)
      case class Foo(someString: String, someNumber: Int, someBar: Bar)

      val input = Foo("theString", 42, Bar(true))

      val encoded = GenericDynamoCodec[Foo].encode(input)
      val result  = GenericDynamoCodec[Foo].decode(encoded)

      result should ===(Right(input))
    }

    "encode then decode a sealed trait with two cases" in {
      sealed trait Foobar
      case class Foo(someString: String) extends Foobar
      case class Bar(someNumber: Int)    extends Foobar

      val input: Foobar = Foo("theString")

      val encoded = GenericDynamoCodec[Foobar].encode(input)
      val result  = GenericDynamoCodec[Foobar].decode(encoded)

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
        val encoded = GenericDynamoCodec[Alphabet].encode(input)
        val result  = GenericDynamoCodec[Alphabet].decode(encoded)

        result should ===(Right(input))
      }
    }

    "encode then decode a recursive ADT" in {
      sealed trait Foobar
      case class Foo(someString: String) extends Foobar
      case class Bar(someFoobar: Foobar) extends Foobar

      val input: Foobar = Bar(Foo("some-string"))

      val encoded = GenericDynamoCodec[Foobar].encode(input)
      val result  = GenericDynamoCodec[Foobar].decode(encoded)

      result should ===(Right(input))
    }
  }
}
