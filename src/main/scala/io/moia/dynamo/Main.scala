package io.moia.dynamo

import io.moia.dynamo.foo.DynamoDecoder

sealed trait Super
case class Foo(i: Int, s: String)     extends Super
case class Bar(i: Int)                extends Super
case class BarRec(i: Int, rec: Super) extends Super

object Super {
  implicit val superEncoder = DynamoEncoder[Super]
  implicit val superDecoder = DynamoDecoder[Super]
}

object Examples {
  def main(args: Array[String]): Unit = {
    check(1)
    check("foo")
    check(Foo(42, "some string"))
    check(BarRec(1, Foo(0, "foo")))
  }

  def check[A: DynamoDecoder: DynamoEncoder](x: A): Unit = {
    println(s"Input: $x")
    val encoded = DynamoEncoder[A].encode(x)
    println(s"Encoded: $encoded")

    val decoded = DynamoDecoder[A].decode(encoded)
    println(s"Decoded: $decoded")
  }
}
