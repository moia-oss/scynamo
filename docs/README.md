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

### Derivation of ScynamoCodec

Scynamo provides both `semiauto` and `auto` derivation.  Note that
mixing them is not a good idea and can lead to compile errors due to
ambiguity of implicits.

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

### Writing Encoders and Decoders

#### Using methods on `ScynamoEncoder`/`ScynamoDecoder`

All encoder/decoder instances provide some helpful methods to create
new instances.

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
