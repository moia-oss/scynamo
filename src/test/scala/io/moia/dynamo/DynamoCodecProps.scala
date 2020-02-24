package io.moia.dynamo

import java.time.Instant

import org.scalacheck.{Gen, Prop, Properties}

class DynamoCodecProps extends Properties("DynamoCodec") {
  property("decode .encode === id (int)") = Prop.forAll { value: Int => decodeAfterEncodeIsIdentity(value) }

  property("decode .encode === id (long)") = Prop.forAll { value: Long => decodeAfterEncodeIsIdentity(value) }

  property("decode .encode === id (float)") = Prop.forAll { value: Float => decodeAfterEncodeIsIdentity(value) }

  property("decode .encode === id (double)") = Prop.forAll { value: Double => decodeAfterEncodeIsIdentity(value) }

  property("decode .encode === id (string)") = Prop.forAll { value: String => decodeAfterEncodeIsIdentity(value) }

  property("decode .encode === id (boolean)") = Prop.forAll { value: Boolean => decodeAfterEncodeIsIdentity(value) }

  property("decode .encode === id (instant)") = Prop.forAll(Gen.calendar.map(_.toInstant)) { value: Instant =>
    decodeAfterEncodeIsIdentity(value)
  }

  private[this] def decodeAfterEncodeIsIdentity[A: DynamoCodec](value: A): Boolean = {
    val codec = DynamoCodec[A]

    codec.decode(codec.encode(value)) == Right(value)
  }
}
