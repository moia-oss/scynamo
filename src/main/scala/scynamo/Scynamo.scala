package scynamo

import cats.data.{EitherNec, EitherNel, NonEmptyChain}
import software.amazon.awssdk.services.dynamodb.model.{GetItemResponse, QueryResponse}

import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import cats.data.EitherNel

object Scynamo extends ScynamoFunctions

trait ScynamoFunctions {
  def decodeGetItemResponse[A: ObjectScynamoDecoder](response: GetItemResponse): EitherNec[ScynamoDecodeError, Option[A]] =
    if (response.hasItem)
      ObjectScynamoDecoder[A].decodeMap(response.item()).map(Some(_))
    else
      Right(None)

  def decodeQueryResponse[A: ObjectScynamoDecoder](
      response: QueryResponse
  ): EitherNel[ScynamoDecodeError, Seq[EitherNec[ScynamoDecodeError, A]]] =
    if (response.hasItems) {
      val decodedValues = response.items().map(ObjectScynamoDecoder[A].decodeMap(_)).toSeq
      Right(decodedValues)
    } else
      Right(Seq.empty)
}
