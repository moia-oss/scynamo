package scynamo

import cats.data.{EitherNec, NonEmptyChain}
import cats.syntax.all._
import cats.{Eq, Show}
import scynamo.ScynamoType.TypeInvalidIfEmpty
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

sealed abstract class ScynamoError extends Product with Serializable {
  def stack: ErrorStack

  def show: String = ScynamoError.scynamoErrorShow.show(this)
}

object ScynamoError {
  implicit val scynamoErrorShow: Show[ScynamoError] = {
    case error: ScynamoEncodeError => Show[ScynamoEncodeError].show(error)
    case error: ScynamoDecodeError => Show[ScynamoDecodeError].show(error)
  }
}

case class ErrorStack(frames: List[StackFrame]) {
  def push(frame: StackFrame): ErrorStack = ErrorStack(frame +: frames)

  override def toString: String =
    frames.mkString("ErrorStack(", " -> ", ")")
}

object ErrorStack {
  val empty: ErrorStack = ErrorStack(List.empty)
}

sealed trait StackFrame extends Product with Serializable
object StackFrame {
  case class Attr(name: String)   extends StackFrame
  case class Case(name: String)   extends StackFrame
  case class Enum(name: String)   extends StackFrame
  case class Index(value: Int)    extends StackFrame
  case class MapKey[A](value: A)  extends StackFrame
  case class Custom(name: String) extends StackFrame

  private[scynamo] def encoding[A](encoded: EitherNec[ScynamoEncodeError, A], frame: StackFrame): EitherNec[ScynamoEncodeError, A] =
    encoded.leftMap(encoding(_, frame))

  private[scynamo] def encoding[A](errors: NonEmptyChain[ScynamoEncodeError], frame: StackFrame): NonEmptyChain[ScynamoEncodeError] =
    errors.map(_.push(frame))

  private[scynamo] def decoding[A](encoded: EitherNec[ScynamoDecodeError, A], frame: StackFrame): EitherNec[ScynamoDecodeError, A] =
    encoded.leftMap(decoding(_, frame))

  private[scynamo] def decoding[A](errors: NonEmptyChain[ScynamoDecodeError], frame: StackFrame): NonEmptyChain[ScynamoDecodeError] =
    errors.map(_.push(frame))
}

sealed abstract class ScynamoEncodeError extends ScynamoError {
  def push(frame: StackFrame): ScynamoEncodeError =
    this match {
      case err @ ScynamoEncodeError.InvalidEmptyValue(_, _) => err.copy(stack = err.stack.push(frame))
      case err @ ScynamoEncodeError.GeneralError(_, _, _)   => err.copy(stack = err.stack.push(frame))
    }
}

object ScynamoEncodeError {
  case class InvalidEmptyValue(scynamoType: ScynamoType, stack: ErrorStack) extends ScynamoEncodeError
  def invalidEmptyValue(scynamoType: ScynamoType with TypeInvalidIfEmpty): InvalidEmptyValue =
    InvalidEmptyValue(scynamoType, ErrorStack.empty)

  case class GeneralError(message: String, cause: Option[Throwable], stack: ErrorStack) extends ScynamoEncodeError
  def generalError(message: String, cause: Option[Throwable]): GeneralError = GeneralError(message, cause, ErrorStack.empty)

  implicit val scynamoEncoderErrorShow: Show[ScynamoEncodeError] = {
    case InvalidEmptyValue(typ, stack)       => s"DynamoDB does not support encoding the empty value of type $typ, error stack: $stack."
    case GeneralError(message, cause, stack) =>
      s"General encoder error: $message${cause.fold("")(e => s" with cause: ${e.getMessage}")}, stack: $stack."
  }
}

sealed abstract class ScynamoDecodeError extends ScynamoError {
  def push(frame: StackFrame): ScynamoDecodeError =
    this match {
      case err @ ScynamoDecodeError.MissingField(_, _, _)          => err.copy(stack = err.stack.push(frame))
      case err @ ScynamoDecodeError.TypeMismatch(_, _, _)          => err.copy(stack = err.stack.push(frame))
      case err @ ScynamoDecodeError.InvalidCoproductCaseMap(_, _)  => err.copy(stack = err.stack.push(frame))
      case err @ ScynamoDecodeError.InvalidCoproductCaseAttr(_, _) => err.copy(stack = err.stack.push(frame))
      case err @ ScynamoDecodeError.ConversionError(_, _, _, _)    => err.copy(stack = err.stack.push(frame))
      case err @ ScynamoDecodeError.GeneralError(_, _, _)          => err.copy(stack = err.stack.push(frame))
    }
}

object ScynamoDecodeError {
  case class MissingField(fieldName: String, hmap: java.util.Map[String, AttributeValue], stack: ErrorStack) extends ScynamoDecodeError
  def missingField(fieldName: String, hmap: java.util.Map[String, AttributeValue]): MissingField =
    MissingField(fieldName, hmap, ErrorStack.empty)

  case class TypeMismatch(expected: ScynamoType, attributeValue: AttributeValue, stack: ErrorStack) extends ScynamoDecodeError
  def typeMismatch(expected: ScynamoType, attributeValue: AttributeValue): TypeMismatch =
    TypeMismatch(expected, attributeValue, ErrorStack.empty)

  case class InvalidCoproductCaseMap(hmap: java.util.Map[String, AttributeValue], stack: ErrorStack) extends ScynamoDecodeError
  def invalidCoproductCaseMap(hmap: java.util.Map[String, AttributeValue]): InvalidCoproductCaseMap =
    InvalidCoproductCaseMap(hmap, ErrorStack.empty)

  case class InvalidCoproductCaseAttr(attributeValue: AttributeValue, stack: ErrorStack) extends ScynamoDecodeError
  def invalidCoproductCaseAttr(attributeValue: AttributeValue): InvalidCoproductCaseAttr =
    InvalidCoproductCaseAttr(attributeValue, ErrorStack.empty)

  case class ConversionError(input: String, to: String, cause: Option[Throwable], stack: ErrorStack) extends ScynamoDecodeError
  def conversionError(input: String, to: String, cause: Option[Throwable]): ConversionError =
    ConversionError(input, to, cause, ErrorStack.empty)

  case class GeneralError(message: String, cause: Option[Throwable], stack: ErrorStack) extends ScynamoDecodeError
  def generalError(message: String, cause: Option[Throwable]): GeneralError =
    GeneralError(message, cause, ErrorStack.empty)

  implicit val scynamoDecodeErrorEq: Eq[ScynamoDecodeError] = Eq.fromUniversalEquals[ScynamoDecodeError]

  implicit val scynamoDecodeErrorShow: Show[ScynamoDecodeError] = {
    case MissingField(fieldName, hmap, stack)          => s"Could not find field '$fieldName' inside $hmap, stack: $stack."
    case TypeMismatch(expected, attributeValue, stack) => s"Type mismatch, expected type $expected, given: $attributeValue, stack: $stack."
    case InvalidCoproductCaseMap(hmap, stack)          => s"Could not decode into one of the sealed trait's cases: $hmap, stack: $stack."
    case InvalidCoproductCaseAttr(av, stack)           => s"Could not decode into one of the sealed trait's cases: $av, stack: $stack."
    case ConversionError(in, to, eOpt, stack)          =>
      s"Error during conversion of '$in' to $to${eOpt.fold("")(e => s" cause: ${e.getMessage}")}, stack: $stack."
    case GeneralError(message, cause, stack) =>
      s"General decoder error: $message${cause.fold("")(e => s" with cause: ${e.getMessage}")}, stack: $stack."
  }
}
