# Scynamo

### Quick Start

1. Add the dependency:

```scala
"io.moia" %% "scynamo" % "@VERSION@"
```

2. Derive an `ObjectScynamoCodec` instance for your custom type:

```scala mdoc
import scynamo._

case class User(id: String, firstName: String, lastName: String)

object User {
  implicit val userCodec: ObjectScynamoCodec[User] = ObjectScynamoCodec.deriveScynamoCodec[User]
}

val user = User("42", "John", "Doe")
```

3. Use it without any further imports:

```scala mdoc
import scynamo._

val result1 = for {
  encoded <- ObjectScynamoCodec[User].encodeMap(user)
  decoded <- ObjectScynamoCodec[User].decodeMap(encoded)
} yield (encoded, decoded)
```

4. (Optional) You can import some sugar, which changes the above example to:
```scala mdoc
import scynamo._
import scynamo.syntax.codec._
import scynamo.generic.semiauto._

val result2 = for {
  encoded <- user.encodedMap
  decoded <- encoded.decode[User]
} yield (encoded, decoded)
```

You can also look at the [minimal
example](#minimal-example-using-the-aws-sdk) below that uses the AWS
SDK (v2) with `scynamo`.

### Derivation of ScynamoCodec

Scynamo provides both `semiauto` and `auto` derivation via
`shapeless`.  Note that mixing them is not a good idea and can lead to
compile errors due to ambiguity of implicits.

To use them you need to import `scynamo.generic.semiauto._` or
`scynamo.generic.auto._`, respectively.  There is a third alternative
that works without any imports, which works by accessing the
`deriveScynamoCodec` method on the `ObjectScynamoCodec` object.

Important notes:
- prefer `semiauto` derivation
- if you use `auto` derivation, make the import as local as possible
  to reduce the scope
- never mix `auto` and `semiauto` derivation
- `auto` derivation does not respect custom overrides of
  Encoders/Decoders from the companion of your classes.  If you need
  that functionality, use `semiauto` derivation instead and avoid
  `auto` derivation.
- to derive for a **sealed trait** there MUST be encoders/decoders for
  **every subtype**, i.e., you need to add derived instances for each
  of them.

#### Customizing Derivation

Scynamo allows you to customize some aspects of the `case class` and
`sealed trait` derivation functionality.

It provides:
- `ScynamoDerivationOpts` for `case class` derivation
- `ScynamoSealedTraitOpts` for `sealed trait` derivation

An example use case would be to customize how fields are encoded:

```scala mdoc
import scynamo.generic.ScynamoDerivationOpts

case class Dog(name: String, age: Int)
object Dog {
  implicit val scynamoDerivationOpts: ScynamoDerivationOpts[Dog] =
  ScynamoDerivationOpts(s => s"dog-$s")

  implicit val scynamoCodec: ObjectScynamoCodec[Dog] = deriveScynamoCodec[Dog]
}

val dogAttrValue = Map("dog-name" -> "Charlie".encoded, "dog-age" -> 3.encoded).encodedMapUnsafe

ObjectScynamoDecoder[Dog].decodeMap(dogAttrValue) == Right(Dog("Charlie", 3))
```

#### The ScynamoEnumCodec

Scynamo also has a `ScynamoEnum{Encoder,Decoder,Codec}`.  It can only
be used with a `sealed trait` that consists ONLY of `case objects`.
Then every `case object` is encoded directly as a `String`.

Example:

```scala mdoc
sealed trait Color
case object Red extends Color
case object Green extends Color
case object Blue extends Color

implicit val colorCodec: ScynamoEnumCodec[Color] = deriveScynamoEnumCodec[Color]

ScynamoEncoder[Color].encode(Blue) // AttributeValue with the String "Blue"
```

### Writing Encoders and Decoders

#### Using methods on `ScynamoEncoder`/`ScynamoDecoder`

All encoder/decoder instances provide some helpful methods to create
new instances.

##### `contramap`/`map`/`imap`

As an example you can modify the standard `String` encoder.decoder to *not*
fail with empty strings:

```scala mdoc
import scynamo._

val emptyStringEncoder: ScynamoEncoder[String] = ScynamoEncoder.stringEncoder.contramap[String] {
  case "" => "some-magic-empty-string"
  case s => s
}

val emptyStringDecoder: ScynamoDecoder[String] = ScynamoDecoder.stringDecoder.map {
  case "some-magic-empty-string" => ""
  case s => s
}
```

Or you can use `imap` to do it directly on a `ScynamoCodec`:

```scala mdoc
val emptyStringCodec = ScynamoCodec.fromEncoderAndDecoder[String].imap[String]{
  case "" => "some-magic-empty-string"
  case s => s
}{
  case "some-magic-empty-string" => ""
  case s => s
}
```
##### `orElse`

Use `orElse` to specify fallbacks:

```scala mdoc
val attributeValue = "some-string".encodedUnsafe // yolo

// unsafe conversion via `toInt`, don't do this at home!
val tolerantDecoder: ScynamoDecoder[Int] = ScynamoDecoder[Int].orElse(ScynamoDecoder[String].map(_.toInt))
```

##### `itransform`/`transform`

There is also `ScynamoCodec.itransform`/`ScynamoDecoder#transform`
which is very handy to chain additional validation:

```scala mdoc
import cats.syntax.either._

// A decoder that only accepts the one answer
ScynamoDecoder[Int].transform {
  case Left(e) => Either.leftNec(ScynamoDecodeError.generalError(s"Not even a number! $e", None))
  case Right(42) => Right(42)
  case Right(i) => Either.leftNec(ScynamoDecodeError.generalError(s"Not the answer: $i", None))
}
```

#### From scratch

If you are writing your own encoders/decoders from scratch, you have
to be careful when accessing the `AttributeValue` from the AWS SDK or
a `java.util.Map[String ,AttributeValue]`.

To provide a null-safe way of accessing attributes, `scynamo` provides
syntax that adds `asOption` and `asEither` methods that take an
`ScynamoType` as input and perform a null-safe access to the
corresponding field of the `AttributeValue`:

```scala mdoc
import scynamo._
import scynamo.syntax.attributevalue._

import software.amazon.awssdk.services.dynamodb.model.AttributeValue

val av: AttributeValue = AttributeValue.builder().build()

val optionalString: Option[String] = av.asOption(ScynamoType.String)
val eitherMap = av.asEither(ScynamoType.Map)
```

#### Don't break the `ErrorStack`

Internally, `scynamo` uses the `ErrorStack` type to keep track of
errors inside a nested structure.

It's important that you always prefer the `par-` methods from
https://typelevel.org/cats/typeclasses/parallel.html where possible.
If you use `flatMap` (or `for-expressions`) you will short-circuit on
the first error and not be able to see all of them at once.

If you write an `ScynamoEncoder` or `ScynamoDecoder` that can/should
provide error stack information, have a look at the built-in instances
for, e.g., `List`, `Map` or even `ShapelessScynamoDecoder`.

The `ErrorStack` supports different `StackFrame` types:

- `Attr` used when accessing a named attribute
- `Case` used when choosing an alternative of a sealed trait
- `Enum` used for Enum-style encoding/decoding
- `Index` used for indices into a list/vector/...
- `MapKey` used when accessing keys of a `Map`
- `Custom` allows users to provide custom information

### Background: cat's `Parallel`

`scynamo` makes heave use of the `Parallel` typeclass provided by
cats: https://typelevel.org/cats/typeclasses/parallel.html

An `EitherNec[E, A]` is a regular `Either` that has a `NonEmptyChain`
of `E`s on the left or a value of type `A` on the right side.

Decoding/Encoding always gives you back an `EitherNec`, so you either
get `>=1` errors or the result of the operation.

It's important to always use the `par-` version of functions if they exist, for example:

- `parTraverse` to decode multiple values in, e.g., `Vector`
- `parMapN` to decode a tuple of individual elements

NOTE: You can also use `flatMap` on `Either`, but that *will short
circuit* and not give you all the errors (you get only the first
instead of all).

The most common imports you would need are:

```
// for `parTraverse` and `parMapN`
import cats.syntax.parallel._
import cats.instances.either._
import cats.instances.list._ // or .vector._

// for `Either.leftNec`
import cats.syntax.either._
```

### Useful tips

#### Building up `AttributeValue`s

`scynamo` provides a very handy `ScynamoEncoder` instance to make it
easier for you to build `AttributeValue`s using the DSL:

```scala mdoc
import cats.data.EitherNec
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

val result = Map(
 "attr1" -> "some-string".encoded,
 "attr2" -> Some(42).encoded,
 "attr3" -> List(4.0, 2.0).encoded
).encodedMap

result: EitherNec[ScynamoEncodeError, java.util.Map[String, AttributeValue]]
```

#### Formatting `ScynamoError`s with `Show`

Every `ScynamoError` provides an instance of `Show` to format the
error message in a more human friendly way:

```scala mdoc
ScynamoEncoder[String].encode("") match {
  case Right(_) => ()
  case Left(errors) =>
    println("Errors: ${errors.map(_.show)}")
}
```

### Minimal Example using the AWS SDK

```scala mdoc
import java.net.URI
import java.util.UUID
import java.util.Collections

import cats.data.EitherNec
import scynamo.syntax.codec._
import scynamo.{ObjectScynamoCodec, Scynamo, ScynamoDecodeError}
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.dynamodb.DynamoDbAsyncClient
import software.amazon.awssdk.services.dynamodb.model.{GetItemRequest, PutItemRequest, PutItemResponse}

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future // don't do this

case class Customer(id: UUID, name: String, age: Int)

object Customer {
  implicit val customerCodec: ObjectScynamoCodec[Customer] = deriveScynamoCodec[Customer]
}

val theCustomer: Customer = Customer(UUID.randomUUID(), "John", 42)

val dynamoEndpoint = "http://127.0.0.1:4569" // localstack
val tableName      = "my-table"

val client: DynamoDbAsyncClient = DynamoDbAsyncClient
  .builder()
  .endpointOverride(URI.create(dynamoEndpoint))
  .region(Region.EU_CENTRAL_1)
  .build()

def writeToDynamo(): Future[PutItemResponse] = theCustomer.encodedMap match {
  case Left(value) => Future.failed(new IllegalArgumentException(s"Failed to encode your customer: ${value.map(_.show)}"))
  case Right(encodedItem) =>
    client.putItem(PutItemRequest.builder().tableName(tableName).item(encodedItem).build()).toScala
}

def readFromDynamo(): Future[EitherNec[ScynamoDecodeError, Option[Customer]]] =
  theCustomer.id.encoded match {
    case Left(value) => Future.failed(new IllegalArgumentException(s"Customer id could not be encoded: ${value.map(_.show)}"))
    case Right(customerId) =>
      client
        .getItem(GetItemRequest.builder().tableName(tableName).key(Collections.singletonMap("id", customerId)).build())
        .toScala
        .map(Scynamo.decodeGetItemResponse[Customer])
  }

for {
  putItemResponse <- writeToDynamo()
  decodedResponse <- readFromDynamo()
} yield (putItemResponse, decodedResponse)
```
