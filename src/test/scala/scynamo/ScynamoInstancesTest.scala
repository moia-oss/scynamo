package scynamo

import cats.Eq
import cats.data.EitherNec
import cats.laws.discipline.{MonadTests, SemigroupKTests}
import cats.laws.discipline.arbitrary._
import cats.tests.{StrictCatsEquality, TestSettings}
import org.scalacheck.rng.Seed
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.jdk.CollectionConverters._
import scala.util.control.NoStackTrace

class ScynamoInstancesTest extends AnyFunSuite with Checkers with FunSuiteDiscipline with StrictCatsEquality with TestSettings {
  import ScynamoInstancesTest.ArbitraryError
  type AttributeMap = java.util.Map[String, AttributeValue]

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration

  implicit def arbitraryJavaMap[K: Arbitrary, V: Arbitrary]: Arbitrary[java.util.Map[K, V]] =
    Arbitrary(Arbitrary.arbitrary[Map[K, V]].map(_.asJava))

  implicit def javaMapCogen[K: Cogen: Ordering, V: Cogen]: Cogen[java.util.Map[K, V]] =
    Cogen[Map[K, V]].contramap(_.asScala.toMap)

  implicit val arbitraryThrowable: Arbitrary[Throwable] =
    Arbitrary(Arbitrary.arbitrary[String].map(ArbitraryError))

  val primitiveAttributeValueGen: Gen[AttributeValue] = {
    def attr[A](f: (AttributeValue.Builder, A) => AttributeValue.Builder): A => AttributeValue =
      a => f(AttributeValue.builder, a).build()

    Gen.oneOf(
      Gen.const(AttributeValue.builder.nul(true).build()),
      Arbitrary.arbitrary[Boolean].map(attr(_.bool(_))),
      Arbitrary.arbitrary[String].map(attr(_.s(_))),
      Arbitrary.arbitrary[Double].map(_.toString).map(attr(_.n(_))),
      Arbitrary.arbitrary[Array[Byte]].map(SdkBytes.fromByteArray).map(attr(_.b(_)))
    )
  }

  // Ideally this would be recursive, but the tests become too slow.
  val attributeValueGen: Gen[AttributeValue] = {
    implicit val arbitraryAttributeValue: Arbitrary[AttributeValue] = Arbitrary(primitiveAttributeValueGen)
    Gen.oneOf(
      Arbitrary.arbitrary[AttributeValue],
      Arbitrary.arbitrary[List[AttributeValue]].map(attributes => AttributeValue.builder.l(attributes: _*).build()),
      Arbitrary.arbitrary[AttributeMap].map(attributes => AttributeValue.builder.m(attributes).build())
    )
  }

  implicit val arbitraryAttributeValue: Arbitrary[AttributeValue] = Arbitrary(attributeValueGen)
  implicit val attributeValueCogen: Cogen[AttributeValue]         = Cogen(_.hashCode)

  val scynamoTypeGen: Gen[ScynamoType] = Gen.oneOf(
    ScynamoType.Null,
    ScynamoType.Bool,
    ScynamoType.String,
    ScynamoType.Number,
    ScynamoType.Binary,
    ScynamoType.Map,
    ScynamoType.List,
    ScynamoType.StringSet,
    ScynamoType.NumberSet,
    ScynamoType.BinarySet
  )

  val missingFieldGen: Gen[ScynamoDecodeError.MissingField] = for {
    fieldName <- Arbitrary.arbitrary[String]
    hmap      <- Arbitrary.arbitrary[AttributeMap]
  } yield ScynamoDecodeError.MissingField(fieldName, hmap, ErrorStack.empty)

  val typeMismatchGen: Gen[ScynamoDecodeError.TypeMismatch] = for {
    expected       <- scynamoTypeGen
    attributeValue <- attributeValueGen
  } yield ScynamoDecodeError.TypeMismatch(expected, attributeValue, ErrorStack.empty)

  val invalidCoproductCaseMapGen: Gen[ScynamoDecodeError.InvalidCoproductCaseMap] = for {
    hmap <- Arbitrary.arbitrary[AttributeMap]
  } yield ScynamoDecodeError.InvalidCoproductCaseMap(hmap, ErrorStack.empty)

  val invalidCoproductCaseAttrGen: Gen[ScynamoDecodeError.InvalidCoproductCaseAttr] = for {
    attributeValue <- attributeValueGen
  } yield ScynamoDecodeError.InvalidCoproductCaseAttr(attributeValue, ErrorStack.empty)

  val conversionErrorGen: Gen[ScynamoDecodeError.ConversionError] = for {
    input <- Arbitrary.arbitrary[String]
    to    <- Arbitrary.arbitrary[String]
    cause <- Arbitrary.arbitrary[Option[Throwable]]
  } yield ScynamoDecodeError.ConversionError(input, to, cause, ErrorStack.empty)

  val generalErrorGen: Gen[ScynamoDecodeError.GeneralError] = for {
    message <- Arbitrary.arbitrary[String]
    cause   <- Arbitrary.arbitrary[Option[Throwable]]
  } yield ScynamoDecodeError.GeneralError(message, cause, ErrorStack.empty)

  implicit val arbitraryDecodeError: Arbitrary[ScynamoDecodeError] = Arbitrary(
    Gen.oneOf(
      missingFieldGen,
      typeMismatchGen,
      invalidCoproductCaseMapGen,
      invalidCoproductCaseAttrGen,
      conversionErrorGen,
      generalErrorGen
    )
  )

  implicit def arbitraryDecoder[A: Arbitrary]: Arbitrary[ScynamoDecoder[A]] =
    Arbitrary(Arbitrary.arbitrary[AttributeValue => EitherNec[ScynamoDecodeError, A]].map(f => f(_)))

  implicit def arbitraryObjectDecoder[A: Arbitrary]: Arbitrary[ObjectScynamoDecoder[A]] =
    Arbitrary(Arbitrary.arbitrary[AttributeMap => EitherNec[ScynamoDecodeError, A]].map(f => f(_)))

  implicit val decodeErrorEq: Eq[ScynamoDecodeError] = Eq.fromUniversalEquals

  def probabilisticEq[A: Arbitrary](f: A => Boolean): Boolean = {
    val params = Gen.Parameters.default.withSize(checkConfiguration.sizeRange.value)
    Iterator
      .from(0)
      .flatMap(n => Arbitrary.arbitrary[A].apply(params, Seed(n)))
      .take(checkConfiguration.minSuccessful.value)
      .forall(f)
  }

  implicit def decoderEq[A: Eq]: Eq[ScynamoDecoder[A]] = (x, y) =>
    probabilisticEq[AttributeValue](value => x.decode(value) === y.decode(value))

  implicit def objectDecoderEq[A: Eq]: Eq[ObjectScynamoDecoder[A]] = (x, y) =>
    probabilisticEq[AttributeMap](attributes => x.decodeMap(attributes) === y.decodeMap(attributes))

  checkAll("Monad[ScynamoDecoder]", MonadTests[ScynamoDecoder].monad[Int, Int, Int])
  checkAll("Monad[ObjectScynamoDecoder]", MonadTests[ObjectScynamoDecoder].monad[Int, Int, Int])
  checkAll("SemigroupK[ScynamoDecoder]", SemigroupKTests[ScynamoDecoder].semigroupK[Int])
  checkAll("SemigroupK[ObjectScynamoDecoder]", SemigroupKTests[ObjectScynamoDecoder].semigroupK[Int])
}

object ScynamoInstancesTest {
  final case class ArbitraryError(message: String) extends Exception(message) with NoStackTrace
}
