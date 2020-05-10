package scynamo

import cats.data.EitherNec
import software.amazon.awssdk.services.dynamodb.model.GetItemResponse

object Scynamo extends ScynamoFunctions

trait ScynamoFunctions {
  def decodeGetItemResponse[A: ObjectScynamoDecoder](response: GetItemResponse): EitherNec[ScynamoDecodeError, Option[A]] =
    if (response.hasItem)
      ObjectScynamoDecoder[A].decodeMap(response.item()).map(Some(_))
    else
      Right(None)

}
