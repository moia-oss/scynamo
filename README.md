# Scynamo - Library to convert between DynamoDB and Scala types

## Documentation

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
