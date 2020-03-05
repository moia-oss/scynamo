package scynamo

import cats.data.EitherNec
import software.amazon.awssdk.services.dynamodb.model.GetItemResponse

object Scynamo extends ScynamoFunctions

trait ScynamoFunctions {
  def decodeGetItemResponse[A: ObjectScynamoDecoder](response: GetItemResponse): Option[EitherNec[ScynamoDecodeError, A]] =
    if (response.hasItem) {
      Some(ObjectScynamoDecoder[A].decodeMap(response.item()))
    } else {
      None
    }
}
