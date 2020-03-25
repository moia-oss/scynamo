package scynamo

import java.time.Instant
import java.util.UUID

import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import scynamo.ScynamoCodecProps.Shape
import scynamo.generic.semiauto._

import scala.concurrent.duration.Duration

class ScynamoCodecProps extends Properties("ScynamoCodec") {
  propertyWithSeed("decode.encode === id (int)", None) = Prop.forAll { value: Int => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (long)", None) = Prop.forAll { value: Long => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (BigInt)", None) = Prop.forAll { value: BigInt => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (float)", None) = Prop.forAll { value: Float => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (double)", None) = Prop.forAll { value: Double => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (BigDecimal)", None) = Prop.forAll { value: BigDecimal => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (nonempty string)", None) = Prop.forAll(Gen.nonEmptyListOf(Gen.alphaNumChar).map(_.mkString)) {
    value: String => decodeAfterEncodeIsIdentity(value)
  }

  propertyWithSeed("decode.encode === id (boolean)", None) = Prop.forAll { value: Boolean => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (instant)", None) = Prop.forAll(Gen.calendar.map(_.toInstant)) { value: Instant =>
    decodeAfterEncodeIsIdentity(value)
  }

  propertyWithSeed("decode.encode === id (seq)", None) = Prop.forAll { value: scala.collection.immutable.Seq[Int] =>
    decodeAfterEncodeIsIdentity(value)
  }

  propertyWithSeed("decode.encode === id (list)", None) = Prop.forAll { value: List[Int] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (vector)", None) = Prop.forAll { value: Vector[Int] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (set)", None) = Prop.forAll { value: Set[Int] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (option)", None) = Prop.forAll { value: Option[Int] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (finite duration)", None) =
    Prop.forAll(Gen.chooseNum[Long](-9223372036854775807L, 9223372036854775807L)) { value: Long =>
      decodeAfterEncodeIsIdentity(Duration.fromNanos(value))
    }

  propertyWithSeed("decode.encode === id (duration)", None) =
    Prop.forAll(Gen.chooseNum[Long](-9223372036854775807L, 9223372036854775807L)) { value: Long =>
      decodeAfterEncodeIsIdentity(Duration.fromNanos(value): Duration)
    }

  propertyWithSeed("decode.encode === id (case class)", None) = Prop.forAll { value: Int =>
    decodeAfterEncodeIsIdentity(ScynamoCodecProps.Foo(value))
  }

  propertyWithSeed("decode.encode === id (sealed trait)", None) = Prop.forAll(Shape.shapeGen) { value: Shape =>
    decodeAfterEncodeIsIdentity(value)
  }

  propertyWithSeed("decode.encode === id (uuid)", None) = Prop.forAll { value: UUID => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (scala map)", None) = Prop.forAll { value: Map[UUID, Int] => decodeAfterEncodeIsIdentity(value) }

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
