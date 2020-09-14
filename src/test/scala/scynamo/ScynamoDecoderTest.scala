package scynamo

import java.util.Collections

import cats.data.EitherNec
import org.scalatest.{Inside, Inspectors}
import scynamo.StackFrame.{Attr, Case, Enum}
import scynamo.generic.ScynamoSealedTraitOpts
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

      val result = ScynamoDecoder[scala.collection.immutable.Seq[Int]].decode(input)

      Inside.inside(result) { case Left(value) =>
        value.toNonEmptyList.toList should have size 3
      }
    }

    "support the nested type tag when decoding" in {
      import scynamo.generic.auto._

      sealed trait Test
      case class Foo(i: Int)    extends Test
      case class Bar(s: String) extends Test

      val inputNestedFormat = AttributeValue
        .builder()
        .m(
          Collections
            .singletonMap(
              "Foo",
              AttributeValue.builder().m(Collections.singletonMap("i", AttributeValue.builder().n("42").build())).build()
            )
        )
        .build()

      val resultNested = ScynamoDecoder[Test].decode(inputNestedFormat)

      resultNested should ===(Right(Foo(42)))
    }

    "decode missing attributes" in {
      import scynamo.generic.auto._
      case class Foo(a: String, b: Option[String])
      val input = AttributeValue.builder().m(Collections.singletonMap("a", AttributeValue.builder().s("the-a").build())).build()

      val result = ScynamoDecoder[Foo].decode(input)

      result should ===(Right(Foo("the-a", None)))
    }

    "provide a stack to the error for nested case classes" in {
      import scynamo.generic.auto._
      import scynamo.syntax.encoder._

      case class Foo(bar: Bar)
      case class Bar(baz: Baz)
      sealed trait Baz
      case class Qux(answer: Int) extends Baz

      val input: EitherNec[ScynamoEncodeError, AttributeValue] = Map(
        "bar" -> Map(
          "baz" -> Map(ScynamoSealedTraitOpts.DEFAULT_DISCRIMINATOR -> "Qux", "answer" -> "wrong type!")
        )
      ).encoded

      val decoded = input.flatMap(ObjectScynamoCodec[Foo].decode)

      Inside.inside(decoded) { case Left(errs) =>
        Inspectors.forAll(errs.toNonEmptyList.toList)(err =>
          (err.stack.frames should contain).theSameElementsInOrderAs(List(Attr("bar"), Attr("baz"), Case("Qux"), Attr("answer")))
        )
      }
    }

    "provide a stack to the error with enum codec" in {
      import EnumCodecTest._
      import scynamo.syntax.encoder._
      val input = Map("test" -> "ABCDEFG").encoded

      val result = input.flatMap(ObjectScynamoCodec[Foobar].decode)

      Inside.inside(result) { case Left(errs) =>
        errs.head.stack.frames should ===(List[StackFrame](Attr("test"), Enum("Foo")))
      }
    }
  }
}

object EnumCodecTest {
  sealed trait Test
  case object Foo extends Test
  object Test {
    implicit val codec: ScynamoEnumCodec[Test] = scynamo.generic.semiauto.deriveScynamoEnumCodec[Test]
  }

  case class Foobar(test: Test)
  object Foobar {
    implicit val codec: ObjectScynamoCodec[Foobar] = ObjectScynamoCodec.deriveScynamoCodec[Foobar]
  }
}
