package io.moia.dynamo

import java.time.Instant

import io.moia.dynamo.DynamoCodecProps.Shape
import org.scalacheck.Prop.propBoolean
import org.scalacheck.{Gen, Prop, Properties}
import io.moia.dynamo.generic.semiauto._

class DynamoCodecProps extends Properties("DynamoCodec") {
  propertyWithSeed("decode.encode === id (int)", None) = Prop.forAll { value: Int => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (long)", None) = Prop.forAll { value: Long => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (float)", None) = Prop.forAll { value: Float => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (double)", None) = Prop.forAll { value: Double => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (string)", None) = Prop.forAll { value: String => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (boolean)", None) = Prop.forAll { value: Boolean => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (instant)", None) = Prop.forAll(Gen.calendar.map(_.toInstant)) { value: Instant =>
    decodeAfterEncodeIsIdentity(value)
  }

  propertyWithSeed("decode.encode === id (seq)", None) = Prop.forAll { value: Seq[Int] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (option)", None) = Prop.forAll { value: Option[Int] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode.encode === id (case class)", None) = Prop.forAll { value: Int =>
    decodeAfterEncodeIsIdentity(DynamoCodecProps.Foo(value))
  }

  propertyWithSeed("decode.encode === id (sealed trait)", None) = Prop.forAll(Shape.shapeGen) { value: Shape =>
    decodeAfterEncodeIsIdentity(value)
  }

  private[this] def decodeAfterEncodeIsIdentity[A](value: A)(implicit codec: DynamoCodec[A]): Prop = {
    val encoded = codec.encode(value)
    val decoded = codec.decode(encoded)
    (decoded == Right(value)) :| s"encoded = $encoded" :| s"decoded = $decoded"
  }
}

object DynamoCodecProps {
  case class Foo(intAttribute: Int)
  object Foo {
    implicit val instance: ObjectDynamoCodec[Foo] = deriveDynamoCodec[Foo]
  }

  sealed trait Shape
  case class Square(size: Int) extends Shape

  object Square {
    implicit val instance: ObjectDynamoCodec[Square] = deriveDynamoCodec[Square]
  }

  case class Rectangle(width: Int, height: Int) extends Shape

  object Rectangle {
    implicit val instance: ObjectDynamoCodec[Rectangle] = deriveDynamoCodec[Rectangle]
  }

  object Shape {
    implicit val instance: ObjectDynamoCodec[Shape] = deriveDynamoCodec[Shape]
    val squareGen: Gen[Square]                      = Gen.posNum[Int].map(Square(_))
    val rectangleGen: Gen[Rectangle] = for {
      w <- Gen.posNum[Int]
      h <- Gen.posNum[Int]
    } yield Rectangle(w, h)

    val shapeGen: Gen[Shape] = Gen.oneOf(squareGen, rectangleGen)
  }
}
