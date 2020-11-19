# Scynamo - Library to convert between DynamoDB and Scala types

Scynamo is a library to map your Scala case classes/sealed traits to
and from DynamoDB.  For that the library uses the typeclass approach
and is very similar to other libraries in the Scala ecosystem.

## Overview of Features

- derive instances for your ADTs to encode/decode from/to DynamoDB
- precise error messages for nested case classes/lists/maps/...
- customize the derivation of case classes and sealed traits using options
- `AttributeValue`-Syntax for null-safe accessors
- provides optional syntax for encoding/decoding (see `scynamo.syntax.*`)
- *safe*: Instead of silently encoding empty {String,Number,Binary} Sets, this operation *fails* in `scynamo`,
  allowing you to catch the error before even reaching DynamoDB.  See
  https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html.

## Documentation

You can find the documentation [here](./docs/README.md).
