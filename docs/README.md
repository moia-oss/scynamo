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

```scala
import scynamo._

val result1 = for {
  encoded <- ObjectScynamoCodec[User].encodeMap(user)
  decoded <- ObjectScynamoCodec[User].decodeMap(encoded)
} yield (encoded, decoded)
```

4. (Optional) You can import some sugar, which changes the above example to:
```scala
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
