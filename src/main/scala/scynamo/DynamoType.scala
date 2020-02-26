package scynamo

sealed trait DynamoType

object TypeTest extends App

object DynamoType {
  val MAGIC_TYPE_ATTRIBUTE_NAME = "_MAGIC_TYPE_TAG_"

  object DynamoNull      extends DynamoType
  object DynamoBool      extends DynamoType
  object DynamoString    extends DynamoType
  object DynamoNumber    extends DynamoType
  object DynamoBinary    extends DynamoType
  object DynamoMap       extends DynamoType
  object DynamoList      extends DynamoType
  object DynamoStringSet extends DynamoType
  object DynamoNumberSet extends DynamoType
  object DynamoBinarySet extends DynamoType
}
