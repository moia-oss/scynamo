package io.moia.dynamo

import java.time.Instant

import org.scalacheck.{Gen, Prop, Properties}
import org.scalacheck.Prop.propBoolean

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

  property("decode .encode === id (seq)") = Prop.forAll { value: Seq[Int] => decodeAfterEncodeIsIdentity(value) }

  private[this] def decodeAfterEncodeIsIdentity[A: DynamoCodec](value: A): Prop = {
    val codec = DynamoCodec[A]

    val encoded = codec.encode(value)
    val decoded = codec.decode(encoded)
    (decoded == Right(value)) :| s"encoded = $encoded" :| s"decoded = $decoded"
  }
}
