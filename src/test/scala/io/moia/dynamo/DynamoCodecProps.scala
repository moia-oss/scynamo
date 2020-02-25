package io.moia.dynamo

import java.time.Instant

import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.Prop.propBoolean

class DynamoCodecProps extends Properties("DynamoCodec") {
  propertyWithSeed("decode .encode === id (int)", None) = Prop.forAll { value: Int => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode .encode === id (long)", None) = Prop.forAll { value: Long => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode .encode === id (float)", None) = Prop.forAll { value: Float => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode .encode === id (double)", None) = Prop.forAll { value: Double => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode .encode === id (string)", None) = Prop.forAll { value: String => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode .encode === id (boolean)", None) = Prop.forAll { value: Boolean => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode .encode === id (instant)", None) = Prop.forAll(Gen.calendar.map(_.toInstant)) { value: Instant =>
    decodeAfterEncodeIsIdentity(value)
  }

  propertyWithSeed("decode .encode === id (seq)", None) = Prop.forAll { value: Seq[Int] => decodeAfterEncodeIsIdentity(value) }

  propertyWithSeed("decode . encode === id (case class)", None) = Prop.forAll { value: Int =>
    decodeAfterEncodeIsIdentity(DynamoCodecProps.Foo(value))
  }

  private[this] def decodeAfterEncodeIsIdentity[A: DynamoCodec](value: A): Prop = {
    val codec = DynamoCodec[A]

    val encoded = codec.encode(value)
    val decoded = codec.decode(encoded)
    (decoded == Right(value)) :| s"encoded = $encoded" :| s"decoded = $decoded"
  }
}

object DynamoCodecProps {
  case class Foo(intAttribute: Int)
  object Foo {
    implicit val fooCodec: GenericDynamoCodec[Foo] = GenericDynamoCodec[Foo]
  }
}
