package scynamo

import java.time.Instant
import java.util.UUID

import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import scynamo.ScynamoCodecProps.Shape
import scynamo.generic.semiauto._
import scynamo.wrapper.{ScynamoNumberSet, ScynamoStringSet}

import scala.concurrent.duration.Duration

class ScynamoCodecProps extends Properties("ScynamoCodec") {
  val propertySeed: Option[String] = None

  private[this] val bigIntGen: Gen[BigInt] = Gen.nonEmptyListOf(Gen.numChar).map(_.mkString).map(BigInt(_))

  private[this] val bigDecimalGen: Gen[BigDecimal] = for {
    prefix <- Gen.nonEmptyListOf(Gen.numChar).map(_.mkString)
    suffix <- Gen.nonEmptyListOf(Gen.numChar).map(_.mkString)
  } yield BigDecimal(s"${prefix}.${suffix}")

  propertyWithSeed("decode.encode === id (int)", propertySeed) = Prop.forAll { value: Int => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (long)", propertySeed) = Prop.forAll { value: Long => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (BigInt)", propertySeed) = Prop.forAll { value: BigInt => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (float)", propertySeed) = Prop.forAll { value: Float => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (double)", propertySeed) = Prop.forAll { value: Double => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (BigDecimal)", propertySeed) = Prop.forAll { value: BigDecimal =>
    decodeAfterEncodeIsIdentity(value)
  }

  propertyWithSeed("decode.encode === id (nonempty string)", propertySeed) =
    Prop.forAll(Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.mkString)) { value: String => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (boolean)", propertySeed) = Prop.forAll { value: Boolean => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (instant)", propertySeed) = Prop.forAll(Gen.calendar.map(_.toInstant)) { value: Instant =>
    decodeAfterEncodeIsIdentity(value)
  }

  propertyWithSeed("decode.encode === id (seq)", propertySeed) = Prop.forAll { value: scala.collection.immutable.Seq[Int] =>
    decodeAfterEncodeIsIdentity(value)
  }

  propertyWithSeed("decode.encode === id (list)", propertySeed) = Prop.forAll { value: List[Int] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (vector)", propertySeed) = Prop.forAll { value: Vector[Int] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (set)", propertySeed) = Prop.forAll { value: Set[Int] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (option)", propertySeed) = Prop.forAll { value: Option[Int] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (finite duration)", propertySeed) =
    Prop.forAll(Gen.chooseNum[Long](-9223372036854775807L, 9223372036854775807L)) { value: Long =>
      decodeAfterEncodeIsIdentity(Duration.fromNanos(value))
    }

  propertyWithSeed("decode.encode === id (duration)", propertySeed) =
    Prop.forAll(Gen.chooseNum[Long](-9223372036854775807L, 9223372036854775807L)) { value: Long =>
      decodeAfterEncodeIsIdentity(Duration.fromNanos(value): Duration)
    }

  propertyWithSeed("decode.encode === id (case class)", propertySeed) = Prop.forAll { value: Int =>
    decodeAfterEncodeIsIdentity(ScynamoCodecProps.Foo(value))
  }

  propertyWithSeed("decode.encode === id (sealed trait)", propertySeed) = Prop.forAll(Shape.shapeGen) { value: Shape =>
    decodeAfterEncodeIsIdentity(value)
  }

  propertyWithSeed("decode.encode === id (uuid)", propertySeed) = Prop.forAll { value: UUID => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (scala map)", propertySeed) = Prop.forAll { value: Map[UUID, Int] =>
    decodeAfterEncodeIsIdentity(value)
  }

  propertyWithSeed("decode.encode === id (string set)", propertySeed) =
    Prop.forAll(Gen.nonEmptyListOf(Gen.numStr).map(_.toSet).map(ScynamoStringSet(_))) { value: ScynamoStringSet =>
      decodeAfterEncodeIsIdentity(value)
    }

  propertyWithSeed("decode.encode === id (number set, int)", propertySeed) =
    Prop.forAll(Gen.nonEmptyListOf(Gen.posNum[Int]).map(_.toSet).map(ScynamoNumberSet(_))) { value: ScynamoNumberSet[Int] =>
      decodeAfterEncodeIsIdentity(value)
    }

  propertyWithSeed("decode.encode === id (number set, long)", propertySeed) =
    Prop.forAll(Gen.nonEmptyListOf(Gen.posNum[Long]).map(_.toSet).map(ScynamoNumberSet(_))) { value: ScynamoNumberSet[Long] =>
      decodeAfterEncodeIsIdentity(value)
    }

  propertyWithSeed("decode.encode === id (number set, float)", propertySeed) =
    Prop.forAll(Gen.nonEmptyListOf(Gen.posNum[Float]).map(_.toSet).map(ScynamoNumberSet(_))) { value: ScynamoNumberSet[Float] =>
      decodeAfterEncodeIsIdentity(value)
    }

  propertyWithSeed("decode.encode === id (number set, double)", propertySeed) =
    Prop.forAll(Gen.nonEmptyListOf(Gen.posNum[Double]).map(_.toSet).map(ScynamoNumberSet(_))) { value: ScynamoNumberSet[Double] =>
      decodeAfterEncodeIsIdentity(value)
    }

  propertyWithSeed("decode.encode === id (number set, bigint)", propertySeed) = Prop.forAll(
    Gen.nonEmptyListOf(bigIntGen).map(_.toSet).map(ScynamoNumberSet(_))
  ) { value: ScynamoNumberSet[BigInt] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (number set, bigdecimal)", propertySeed) = Prop.forAll(
    Gen.nonEmptyListOf(bigDecimalGen).map(_.toSet).map(ScynamoNumberSet(_))
  ) { value: ScynamoNumberSet[BigDecimal] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (string set)", propertySeed) =
    Prop.forAll(Gen.nonEmptyListOf(Gen.numStr).map(_.toSet).map(ScynamoStringSet(_))) { value: ScynamoStringSet =>
      decodeAfterEncodeIsIdentity(value)
    }

  private[this] def decodeAfterEncodeIsIdentity[A](value: A)(implicit codec: ScynamoCodec[A]): Prop = {
    val encoded = codec.encode(value)
    val decoded = encoded.flatMap(codec.decode)

    (decoded == Right(value)) :| s"encoded = $encoded" :| s"decoded = $decoded"
  }
}

object ScynamoCodecProps {
  case class Foo(intAttribute: Int)
  object Foo {
    implicit val instance: ObjectScynamoCodec[Foo] = deriveScynamoCodec[Foo]
  }

  sealed trait Shape
  case class Square(size: Int) extends Shape

  object Square {
    implicit val instance: ObjectScynamoCodec[Square] = deriveScynamoCodec[Square]
  }

  case class Rectangle(width: Int, height: Int) extends Shape

  object Rectangle {
    implicit val instance: ObjectScynamoCodec[Rectangle] = deriveScynamoCodec[Rectangle]
  }

  object Shape {
    implicit val instance: ObjectScynamoCodec[Shape] = deriveScynamoCodec[Shape]
    val squareGen: Gen[Square]                       = Gen.posNum[Int].map(Square(_))
    val rectangleGen: Gen[Rectangle] = for {
      w <- Gen.posNum[Int]
      h <- Gen.posNum[Int]
    } yield Rectangle(w, h)

    val shapeGen: Gen[Shape] = Gen.oneOf(squareGen, rectangleGen)
  }
}
