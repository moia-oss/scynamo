package scynamo

import cats.data.EitherNec
import scynamo.ScynamoEncoderTest.Foo
import scynamo.ScynamoType.ScynamoString
import software.amazon.awssdk.services.dynamodb.model.AttributeValue
import scynamo.generic.auto._

class ScynamoEncoderTest extends UnitTest {
  "ScynamoEncoder" should {
    "support custom overrides" in {
      val result = ObjectScynamoCodec[Foo].encode(Foo(42))
      result.m.get("i").s should ===(s"${Foo.prefix}42")
    }

    "auto derivation does not kick-in for Option" in {
      val encoder = ScynamoEncoder[Option[Int]]

      val result = encoder.encode(Some(1))

      result.n should ===("1")
    }

    "auto derivation works" in {
      case class TestClass(someInteger: Int)

      val result = ScynamoEncoder[TestClass].encode(TestClass(42))

      result.m.get("someInteger").n should ===("42")
    }
  }
}

object ScynamoEncoderTest {
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
