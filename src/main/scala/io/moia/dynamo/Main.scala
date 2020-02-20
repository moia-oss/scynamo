package io.moia.dynamo

import io.moia.dynamo.foo.DynamoDecoder

sealed trait Test
case class Foo(i: Int, s: String)    extends Test
case class Bar(i: Int)               extends Test
case class BarRec(i: Int, rec: Test) extends Test

object Test {
  implicit val encoder = DynamoEncoder[Test]
  implicit val decoder = DynamoDecoder[Test]
}

object Examples {
  def main(args: Array[String]): Unit = {
    check(1)
    check("foo")
    check(Foo(42, "some string"))
    check(BarRec(1, Foo(0, "foo")))

    check2(1)
    check2("foo")
    check2(BarRec(1, Foo(0, "foo")))
  }

  def check[A: DynamoDecoder: DynamoEncoder](x: A): Unit = {
    println(s"Input: $x")
    val encoded = DynamoEncoder[A].encode(x)
    println(s"Encoded: $encoded")

    val decoded = DynamoDecoder[A].decode(encoded)
    println(s"Decoded: $decoded")
  }

  def check2[A: DynamoCodec](x: A): Unit = {
    println(s"Input: $x")
    val encoded = DynamoCodec[A].encode(x)
    println(s"Encoded: $encoded")

    val decoded = DynamoCodec[A].decode(encoded)
    println(s"Decoded: $decoded")
  }
}
