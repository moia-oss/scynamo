package scynamo

import cats.data.EitherNec
import scynamo.ScynamoDecoderTest.Foo
import scynamo.ScynamoType.ScynamoString
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

class ScynamoDecoderTest extends UnitTest {
  "ScynamoDecoder" should {
    "support map" in {
      implicit val newDecoder: ScynamoDecoder[Int] = ScynamoDecoder.numericDecoder[Int].map(_ + 1)

      val int   = 41
      val input = AttributeValue.builder().n(s"$int").build()

      val result = ScynamoDecoder[Int].decode(input)

      result should ===(Right(42))
    }

    "support fallback" in {
      implicit val newDecoder: ScynamoDecoder[Int] = ScynamoDecoder.numericDecoder[Int].orElse(_ => Right(42))

      val input = AttributeValue.builder().nul(true).build()

      val result = ScynamoDecoder[Int].decode(input)

      result should ===(Right(42))
    }

    "support custom overrides" in {
      val result = ObjectScynamoCodec[Foo].encode(Foo(42))
      result.m.get("i").s should ===(s"${Foo.prefix}42")
    }
  }
}

object ScynamoDecoderTest {
  case class Foo(i: Int)

  object Foo {
    import scynamo.attributevalue.dsl._
    val prefix = "this-is-a-"
    implicit val customIntCodec: ScynamoCodec[Int] = new ScynamoCodec[Int] {
      override def encode(value: Int): AttributeValue =
        AttributeValue.builder().s(s"$prefix$value").build()

      override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, Int] =
        accessOrTypeMismatch(attributeValue, ScynamoString)(_.sOpt).map(_.stripPrefix(prefix).toInt)
    }

    implicit val fooCodec: ObjectScynamoCodec[Foo] = scynamo.generic.semiauto.deriveDynamoCodec[Foo]
  }
}
