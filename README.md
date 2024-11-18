# shopify-function-types-rust

This is a non-official, personal project (yet work in progress) created to generate Rust structs and enums from a GraphQL schema and query. It’s tailored to fit specific needs and is not affiliated with or endorsed by Shopify. The purpose of this project is to automate the generation of Rust types based on GraphQL schemas used by shopify functions. This helps maintain consistency between the GraphQL API and the discount app, reducing manual effort and potential errors. Also, it implements miniserde instead of serde, allowing us to reduce the generated file's size. Your first stop must be https://crates.io/crates/shopify_function before considering using this one.

## Features

* Generates Rust structs and enums from schemas and queries.
* Handles input object types, enums, interfaces, unions, and custom scalars.
* Ignores __typename fields in queries.
* Customizable type mappings, such as treating union types like Merchandise as a specific type (ProductVariant) [it can be extended to use CustomProduc].
* Respects nullability as specified in the GraphQL schema, correctly mapping to Option<T> where appropriate.
* Includes documentation comments from the GraphQL schema in the generated code.


## Motivation

Existing solutions that relies on serde are powerful but can lead to larger binary sizes. To optimize for smaller binaries, this project utilizes miniserde internally, which offers a lightweight serialization/deserialization mechanism suitable for generating smaller binaries, see https://shopify.dev/docs/apps/build/functions/programming-languages/rust-for-functions

## Limitations

* Not an official Shopify tool: This project is a personal utility and is not affiliated with Shopify.
* Custom adjustments: The code generation is tailored for specific use cases and may require adjustments to fit different schemas or requirements.
* Miniserde vs. Serde: While miniserde is used internally for smaller binary sizes, it doesn’t support all features of serde. Ensure that miniserde meets your serialization needs.

## Contributing

As this is a personal project developed to meet specific needs, contributions are welcome for increasing its capacibilities and meet more use cases.
