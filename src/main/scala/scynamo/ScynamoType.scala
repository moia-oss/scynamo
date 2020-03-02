package scynamo

sealed trait ScynamoType

object TypeTest extends App

object ScynamoType {
  val MAGIC_TYPE_ATTRIBUTE_NAME = "_MAGIC_TYPE_TAG_"

  case object ScynamoNull      extends ScynamoType
  case object ScynamoBool      extends ScynamoType
  case object ScynamoString    extends ScynamoType
  case object ScynamoNumber    extends ScynamoType
  case object ScynamoBinary    extends ScynamoType
  case object ScynamoMap       extends ScynamoType
  case object ScynamoList      extends ScynamoType
  case object ScynamoStringSet extends ScynamoType
  case object ScynamoNumberSet extends ScynamoType
  case object ScynamoBinarySet extends ScynamoType
}
