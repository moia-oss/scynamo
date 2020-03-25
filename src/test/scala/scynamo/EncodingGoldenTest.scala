package scynamo

import java.time.Instant
import java.util.UUID
import scala.concurrent.duration._
import scynamo.syntax.encoder._

import scynamo.EncodingGoldenTest._

import scala.concurrent.duration.{Duration, FiniteDuration}

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

      val result = input.encoded.map(_.toString)

      val expected =
        "AttributeValue(SS=[], NS=[], BS=[], M={testOption=AttributeValue(N=42, SS=[], NS=[], BS=[], M={}, L=[]), testInstant=AttributeValue(N=0, SS=[], NS=[], BS=[], M={}, L=[]), testString=AttributeValue(S=abc, SS=[], NS=[], BS=[], M={}, L=[]), testFiniteDuration=AttributeValue(N=1000000000, SS=[], NS=[], BS=[], M={}, L=[]), testTraitList=AttributeValue(SS=[], NS=[], BS=[], M={}, L=[AttributeValue(SS=[], NS=[], BS=[], M={_SCYNAMO_DEFAULT_DISCRIMINATOR_=AttributeValue(S=TraitObject, SS=[], NS=[], BS=[], M={}, L=[])}, L=[]), AttributeValue(SS=[], NS=[], BS=[], M={_SCYNAMO_DEFAULT_DISCRIMINATOR_=AttributeValue(S=TraitCaseClass, SS=[], NS=[], BS=[], M={}, L=[]), testFloat=AttributeValue(N=21.21, SS=[], NS=[], BS=[], M={}, L=[])}, L=[])]), testUuid=AttributeValue(S=1c4b009c-ee7e-4a47-a401-36b468ef7d1e, SS=[], NS=[], BS=[], M={}, L=[]), testMap=AttributeValue(SS=[], NS=[], BS=[], M={1c4b009c-ee7e-4a47-a401-36b468ef7d1e=AttributeValue(S=foo, SS=[], NS=[], BS=[], M={}, L=[])}, L=[]), testDuration=AttributeValue(N=1000000000, SS=[], NS=[], BS=[], M={}, L=[])}, L=[])"

      result should ===(Right(expected))
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
