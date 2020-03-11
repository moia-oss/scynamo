package scynamo

import java.util.Collections

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

      val result = ScynamoDecoder[scala.collection.immutable.Seq[Int]].decode(input)

      Inside.inside(result) {
        case Left(value) => value.toNonEmptyList.toList should have size 3
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
  }
}
