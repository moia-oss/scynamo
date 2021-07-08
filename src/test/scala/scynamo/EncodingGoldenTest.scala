package scynamo

import java.time.Instant
import java.util.UUID

import scynamo.EncodingGoldenTest._
import scynamo.syntax.encoder._

import scala.concurrent.duration._

class EncodingGoldenTest extends UnitTest {
  private[this] val uuid = UUID.fromString("1c4b009c-ee7e-4a47-a401-36b468ef7d1e")

  "Encoding" should {
    "not change" in {
      val input = TestCaseClass(
        "abc",
        Some(42),
        uuid,
        testTraitList = List[TestTrait](TraitObject, TraitCaseClass(21.21f)),
        Map(uuid -> "foo"),
        Instant.ofEpochSecond(0),
        Duration.fromNanos(1000 * 1000 * 1000),
        1.second
      )

      val actual =
        input.encoded.map(_.toString)

      val expected =
        "AttributeValue(M={testOption=AttributeValue(N=42), testInstant=AttributeValue(N=0), testString=AttributeValue(S=abc), testFiniteDuration=AttributeValue(N=1000000000), testTraitList=AttributeValue(L=[AttributeValue(M={_SCYNAMO_DEFAULT_DISCRIMINATOR_=AttributeValue(S=TraitObject)}), AttributeValue(M={testFloat=AttributeValue(N=21.21), _SCYNAMO_DEFAULT_DISCRIMINATOR_=AttributeValue(S=TraitCaseClass)})]), testUuid=AttributeValue(S=1c4b009c-ee7e-4a47-a401-36b468ef7d1e), testMap=AttributeValue(M={1c4b009c-ee7e-4a47-a401-36b468ef7d1e=AttributeValue(S=foo)}), testDuration=AttributeValue(N=1000000000)})"

      actual shouldBe Right(expected)
    }

    "not change for empty fields" in {
      val input = TestCaseClass(
        "foobar",
        None,
        uuid,
        testTraitList = Nil,
        Map.empty,
        Instant.ofEpochSecond(0),
        Duration.fromNanos(0),
        0.seconds
      )

      val actual =
        input.encoded.map(_.toString)

      val expected =
        "AttributeValue(M={testInstant=AttributeValue(N=0), testString=AttributeValue(S=foobar), testFiniteDuration=AttributeValue(N=0), testTraitList=AttributeValue(L=[]), testUuid=AttributeValue(S=1c4b009c-ee7e-4a47-a401-36b468ef7d1e), testMap=AttributeValue(M={}), testDuration=AttributeValue(N=0)})"

      actual shouldBe Right(expected)
    }
  }
}

object EncodingGoldenTest {
  sealed trait TestTrait
  case object TraitObject                     extends TestTrait
  case class TraitCaseClass(testFloat: Float) extends TestTrait

  object TestTrait {
    implicit val encoderTraitObject: ObjectScynamoEncoder[TraitObject.type]  = ObjectScynamoEncoder.deriveScynamoEncoder[TraitObject.type]
    implicit val encoderTraitCaseClass: ObjectScynamoEncoder[TraitCaseClass] = ObjectScynamoEncoder.deriveScynamoEncoder[TraitCaseClass]
    implicit val encoderTestTrait: ObjectScynamoEncoder[TestTrait]           = ObjectScynamoEncoder.deriveScynamoEncoder[TestTrait]
  }

  case class TestCaseClass(
      testString: String,
      testOption: Option[Int],
      testUuid: UUID,
      testTraitList: List[TestTrait],
      testMap: Map[UUID, String],
      testInstant: Instant,
      testDuration: Duration,
      testFiniteDuration: FiniteDuration
  )

  object TestCaseClass {
    implicit val encoder: ObjectScynamoEncoder[TestCaseClass] = ObjectScynamoEncoder.deriveScynamoEncoder[TestCaseClass]
  }
}
