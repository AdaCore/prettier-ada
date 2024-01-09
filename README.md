# Prettier-Ada

This project is a port of the [Prettier](https://github.com/prettier/prettier)
formatter to the Ada programming language. The aim of this project is to
provide a tool that allows Ada programmers to create formatters for any
language of their choosing.

Please note that only the [core generic formatting algorithm](https://github.com/prettier/prettier/blob/main/src/document/printer.js)
and its dependencies were ported. The formatters for all supported languages
are not included in this project.

## Usage

This library allows users to build their own Prettier documents and format it.
To use this library, users need to instantiate the
`Prettier_Ada.Generic_Formatters` package and use its `Print` function to
format a node:

```ada
generic
  type Node_Type is private;
  with function Print_IR
    (Node : Node_Type)
     return Prettier_Ada.Documents.Document_Type;
package Prettier_Ada.Generic_Formatters is
  function Print
    (Node    : Node_Type;
     Options : Prettier_Ada.Documents.Format_Options_Type)
     return Ada.Strings.Unbounded.Unbounded_String;
end Prettier_Ada.Generic_Formatters;
```

`Node_Type` is the user's Abstract Syntax Tree (AST) type.

`Print_IR` is a function that uses the `Prettier_Ada.Documents.Builders`
package to create a `Prettier_Ada.Documents.Document_Type` value from a
`Node_Type` value. Prettier will then be able to turn this document into
formatted source code. The `Prettier_Ada.Documents.Builders` API tries to match
as closely as possible the [original Prettier's API](https://github.com/prettier/prettier/blob/main/src/document/builders.js).

The `Prettier_Ada.Documents.Json` package provides serialization and
deserialization functions for `Prettier_Ada.Documents.Document_Type`.

## Building and Installing

### Dependencies

- [GNATcoll](https://github.com/AdaCore/gnatcoll-core)
- [VSS](https://github.com/AdaCore/vss)

### Instructions

```sh
# To build in a single LIBRARY_TYPE which can be 'static', 'static-pic' or
# 'relocatable' (default)
make lib
# Or to build all types
make all

# Then install it in a specific location, for instance /usr/local
PREFIX=/usr/local make install
```

## Testing

### Test Dependencies

- [e3-testsuite](https://e3-testsuite.readthedocs.io/en/latest/)

### Running the testsuite

```sh
make test
```

### Ported Prettier tests

This library has been tested against Prettier's tests that test how a Prettier
document is formatted. The port methodology consisted in serializing the
document that Prettier formats, deserializing and formatting using this
library, and finally comparing the result with Prettier's one. This ported
testsuite is currently internal and not shared under this repo.

## Work in progress and contributing

Please note that this library is still a work in progress. In particular, there
are two points that will soon be addressed:

- Remove global state which will allow easy paralelism
- Manage allocated memory

However, contributions are welcome! Please feel free to submit a pull request
or open an issue if you find a bug or have a feature suggestion.

## License

See [LICENSE.txt](LICENSE.txt) file for details.
