package scynamo

import cats.data.EitherNec
import software.amazon.awssdk.services.dynamodb.model.{GetItemResponse, QueryResponse, ScanResponse, UpdateItemResponse}
import cats.syntax.all._

import scala.jdk.CollectionConverters._
object Scynamo extends ScynamoFunctions

trait ScynamoFunctions {
  def decodeGetItemResponse[A: ObjectScynamoDecoder](response: GetItemResponse): EitherNec[ScynamoDecodeError, Option[A]] =
    if (response.hasItem)
      ObjectScynamoDecoder[A].decodeMap(response.item()).map(Some(_))
    else
      Right(None)

  def decodeUpdateItemResponse[A: ObjectScynamoDecoder](response: UpdateItemResponse): EitherNec[ScynamoDecodeError, Option[A]] =
    if (response.hasAttributes)
      ObjectScynamoDecoder[A].decodeMap(response.attributes()).map(Some(_))
    else
      Right(None)

  def decodeQueryResponse[A: ObjectScynamoDecoder](response: QueryResponse): EitherNec[ScynamoDecodeError, List[A]] =
    response.items().asScala.toList.traverse(ObjectScynamoDecoder[A].decodeMap(_))

  def decodeScanResponse[A: ObjectScynamoDecoder](response: ScanResponse): EitherNec[ScynamoDecodeError, List[A]] =
    response.items().asScala.toList.traverse(ObjectScynamoDecoder[A].decodeMap(_))
}
