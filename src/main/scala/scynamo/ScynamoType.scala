package scynamo

sealed trait ScynamoType

object TypeTest extends App

object ScynamoType {
  val MAGIC_TYPE_ATTRIBUTE_NAME = "_MAGIC_TYPE_TAG_"

  object ScynamoNull      extends ScynamoType
  object ScynamoBool      extends ScynamoType
  object ScynamoString    extends ScynamoType
  object ScynamoNumber    extends ScynamoType
  object ScynamoBinary    extends ScynamoType
  object ScynamoMap       extends ScynamoType
  object ScynamoList      extends ScynamoType
  object ScynamoStringSet extends ScynamoType
  object ScynamoNumberSet extends ScynamoType
  object ScynamoBinarySet extends ScynamoType
}
