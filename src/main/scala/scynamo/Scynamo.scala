package scynamo

import cats.data.EitherNec
import software.amazon.awssdk.services.dynamodb.model.{GetItemResponse, QueryResponse}
import cats.syntax.all._

import scala.jdk.CollectionConverters._
object Scynamo extends ScynamoFunctions

trait ScynamoFunctions {
  def decodeGetItemResponse[A: ObjectScynamoDecoder](response: GetItemResponse): EitherNec[ScynamoDecodeError, Option[A]] =
    if (response.hasItem)
      ObjectScynamoDecoder[A].decodeMap(response.item()).map(Some(_))
    else
      Right(None)

  def decodeQueryResponse[A: ObjectScynamoDecoder](response: QueryResponse): EitherNec[ScynamoDecodeError, List[A]] =
    response.items().asScala.toList.traverse(ObjectScynamoDecoder[A].decodeMap(_))
}
