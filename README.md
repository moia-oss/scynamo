# Scynamo - Library to convert between DynamoDB and Scala types

Scynamo is a library to map your Scala case classes/sealed traits to
and from DynamoDB.  For that the library uses the typeclass approach
and is very similar to other libraries in the Scala ecosystem.

## Documentation

### Features

- derive instances for your ADTs to encode/decode from/to DynamoDB
- customize the derivation of case classes and sealed traits using options
- `AttributeValue`-Syntax for null-safe accessors
- provides optional syntax for encoding/decoding (see `scynamo.syntax.*`)

### Derivation

Scynamo provides both `semiauto` and `auto` derivation.  Note that
mixing them is not a good idea and can lead to compile errors due to
ambiguity of implicits.

To use the derivation mechanisms you can choose between:

- import scynamo.generic.auto.__
- import scynamo.generic.semiauto.__
- directly use the `derive*` methods on the companions.

#### Semiautomatic Derivation via import

```scala
import scynamo.generic.semiauto._

clase class Foo(i: Int, s: String)

object Foo {
  implicit val codec: ObjectScynamoCodec[Foo] = deriveScynamoCodec[Foo]
}
```

#### Semiautomatic Derivation via companion

```scala
clase class Foo(i: Int, s: String)

object Foo {
  implicit val codec: ObjectScynamoCodec[Foo] = ObjectScynamoCodec.deriveScynamoCodec[Foo]
}
```

#### Automatic Derivation
```scala
import scynamo.generic.auto._

clase class Foo(i: Int, s: String)

// and it automagically works
val codec = implicitly[ObjectScynamoCodec[Foo]]
```

NOTE: auto derivation does not respect custom overrides of
Encoders/Decoders from the companion of your classes.  If you need
that functionality, use `semiauto` derivation instead and avoid `auto`
derivation.
