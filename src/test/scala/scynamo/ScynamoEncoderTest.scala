package scynamo

import cats.data.EitherNec
import cats.syntax.all._
import org.scalatest.Inside
import scynamo.syntax.all._
import scynamo.ScynamoEncoderTest._
import scynamo.StackFrame.{Attr, Case, Index, MapKey}
import scynamo.wrapper.{ScynamoBinarySet, ScynamoNumberSet, ScynamoStringSet}
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.dynamodb.model.AttributeValue
import scala.jdk.CollectionConverters._

class ScynamoEncoderTest extends UnitTest {
  "ScynamoEncoder" should {
    "support custom overrides using semiauto derivation" in {
      val result = ObjectScynamoCodec[Foo].encode(Foo(42))

      result.map(_.m.get("i").s) should ===(Right(s"${Foo.prefix}42"))
    }

    "NOT support custom overrides using auto derivation" in {
      import scynamo.generic.auto._
      val result = ObjectScynamoCodec[Foo2].encode(Foo2(42))
      result.map(_.m.get("i").n) should ===(Right(s"42"))
    }

    "auto derivation does not kick-in for Option" in {
      val encoder = ScynamoEncoder[Option[Int]]

      val result = encoder.encode(Some(1))

      result.map(_.n) should ===(Right("1"))
    }

    "auto derivation does not kick-in for Some with DSL" in {
      import scynamo.syntax.encoder._

      val result = Some(1).encoded

      result.map(_.n) should ===(Right("1"))
    }

    "auto derivation works" in {
      import scynamo.generic.auto._

      case class TestClass(someInteger: Int)

      val result = ScynamoEncoder[Option[TestClass]].encode(Some(TestClass(42)))

      result.map(_.m.get("someInteger").n) should ===(Right("42"))
    }

    "provide an error stack" in {
      import scynamo.generic.auto._

      case class Root(level1: Level1)
      case class Level1(level2: Level2)
      sealed trait Level2
      case class Level2Impl(value: ScynamoStringSet) extends Level2

      val input = Root(Level1(Level2Impl(ScynamoStringSet(Set()))))

      val result = ScynamoEncoder[Root].encode(input)

      Inside.inside(result) { case Left(errs) =>
        errs.head.stack.frames should ===(List(Attr("level1"), Attr("level2"), Case("Level2Impl"), Attr("value")))
      }
    }

    "provide a stack to error for lists" in {
      import scynamo.syntax.encoder._

      val input = List(ScynamoStringSet(Set("foo")), ScynamoStringSet(Set()), ScynamoStringSet(Set("bar")))

      val result = input.encoded

      Inside.inside(result) { case Left(es) =>
        es.head.stack.frames should ===(List(Index(1)))
      }
    }

    "provide a stack to error for maps" in {
      import scynamo.syntax.encoder._

      val keyName = "key-for-invalid-set"
      val input   = Map(keyName -> ScynamoStringSet(Set()))

      val result = input.encoded

      Inside.inside(result) { case Left(es) =>
        es.head.stack.frames should ===(List(MapKey(keyName)))
      }
    }

    "fail on empty string set" in {
      ScynamoEncoder[ScynamoStringSet].encode(ScynamoStringSet(Set())) should ===(
        Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.StringSet))
      )
    }

    "fail on empty number set" in {
      ScynamoEncoder[ScynamoNumberSet[Int]].encode(ScynamoNumberSet(Set[Int]())) should ===(
        Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.NumberSet))
      )

      ScynamoEncoder[ScynamoNumberSet[Long]].encode(ScynamoNumberSet(Set[Long]())) should ===(
        Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.NumberSet))
      )

      ScynamoEncoder[ScynamoNumberSet[Float]].encode(ScynamoNumberSet(Set[Float]())) should ===(
        Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.NumberSet))
      )

      ScynamoEncoder[ScynamoNumberSet[Double]].encode(ScynamoNumberSet(Set[Double]())) should ===(
        Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.NumberSet))
      )

      ScynamoEncoder[ScynamoNumberSet[BigInt]].encode(ScynamoNumberSet(Set[BigInt]())) should ===(
        Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.NumberSet))
      )

      ScynamoEncoder[ScynamoNumberSet[BigDecimal]].encode(ScynamoNumberSet(Set[BigDecimal]())) should ===(
        Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.NumberSet))
      )
    }

    "fail on empty binary set" in {
      ScynamoEncoder[ScynamoBinarySet].encode(ScynamoBinarySet(Set[SdkBytes]())) should ===(
        Either.leftNec(ScynamoEncodeError.invalidEmptyValue(ScynamoType.BinarySet))
      )
    }

    "omit empty fields from case classes" in {
      ScynamoEncoder[Snake[Int]]
        .encode(Snake(1, None))
        .map(attr => Option(attr.m.get("tail"))) should ===(Right(None))

      ObjectScynamoEncoder[Snake[Int]]
        .encodeMap(Snake(1, Some(Snake(2, None))))
        .map(attrs => Option(attrs.get("tail").m.get("tail"))) should ===(Right(None))
    }

    "allow modifying encoded maps" in {
      val prices = Typed("price", Map("milk" -> 1.29, "hummus" -> 0.99))
      prices.encodedMap.map(_.asScala) shouldBe Right(
        Map(
          "type"   -> AttributeValue.builder.s("price").build(),
          "milk"   -> AttributeValue.builder.n("1.29").build(),
          "hummus" -> AttributeValue.builder.n("0.99").build()
        )
      )
    }

    "allow modifying encoded objects" in {
      val prices = Typed("snake", Snake(42, None))
      prices.encodedMap.map(_.asScala) shouldBe Right(
        Map(
          "type" -> AttributeValue.builder.s("snake").build(),
          "head" -> AttributeValue.builder.n("42").build()
        )
      )
    }
  }
}

object ScynamoEncoderTest {
  case class Typed[A](tpe: String, value: A)
  object Typed {
    implicit def encoder[A: ObjectScynamoEncoder]: ObjectScynamoEncoder[Typed[A]] =
      ObjectScynamoEncoder.instance { case Typed(tpe, value) =>
        for {
          encodedType  <- tpe.encoded
          encodedValue <- value.encodedMap
        } yield {
          encodedValue.put("type", encodedType)
          encodedValue
        }
      }
  }

  case class Snake[A](head: A, tail: Option[Snake[A]])
  object Snake {
    implicit def codec[A: ScynamoEncoder]: ObjectScynamoEncoder[Snake[A]] =
      scynamo.generic.semiauto.deriveScynamoEncoder[Snake[A]]
  }

  case class Foo(i: Int)
  object Foo {
    import scynamo.syntax.attributevalue._
    val prefix                                     = "this-is-a-"
    implicit val customIntCodec: ScynamoCodec[Int] = new ScynamoCodec[Int] {
      override def encode(value: Int): EitherNec[ScynamoEncodeError, AttributeValue] =
        Right(AttributeValue.builder().s(s"$prefix$value").build())

      override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, Int] =
        attributeValue.asEither(ScynamoType.String).map(_.stripPrefix(prefix).toInt)
    }

    implicit val fooCodec: ObjectScynamoCodec[Foo] = scynamo.generic.semiauto.deriveScynamoCodec[Foo]
  }

  case class Foo2(i: Int)
  object Foo2 {
    import scynamo.syntax.attributevalue._
    val prefix                                     = "this-is-a-"
    implicit val customIntCodec: ScynamoCodec[Int] = new ScynamoCodec[Int] {
      override def encode(value: Int): EitherNec[ScynamoEncodeError, AttributeValue] =
        Right(AttributeValue.builder().s(s"$prefix$value").build())

      override def decode(attributeValue: AttributeValue): EitherNec[ScynamoDecodeError, Int] =
        attributeValue.asEither(ScynamoType.String).map(_.stripPrefix(prefix).toInt)
    }
  }
}
