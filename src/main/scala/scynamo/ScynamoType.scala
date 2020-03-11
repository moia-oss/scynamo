package scynamo

import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

sealed abstract class ScynamoType {
  type Result
}

object TypeTest extends App

object ScynamoType {
  type Aux[A] = ScynamoType { type Result = A }

  case object Null      extends ScynamoType { type Result = Boolean                               }
  case object Bool      extends ScynamoType { type Result = Boolean                               }
  case object String    extends ScynamoType { type Result = String                                }
  case object Number    extends ScynamoType { type Result = String                                }
  case object Binary    extends ScynamoType { type Result = SdkBytes                              }
  case object Map       extends ScynamoType { type Result = java.util.Map[String, AttributeValue] }
  case object List      extends ScynamoType { type Result = java.util.List[AttributeValue]        }
  case object StringSet extends ScynamoType { type Result = java.util.List[String]                }
  case object NumberSet extends ScynamoType { type Result = java.util.List[String]                }
  case object BinarySet extends ScynamoType { type Result = java.util.List[SdkBytes]              }
}
