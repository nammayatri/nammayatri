# Abstract Syntax Tree (AST) and Code Generation Patterns

While the Nammayatri backend primarily focuses on business logic and API implementation, several patterns indicate interaction with or generation of Abstract Syntax Trees (ASTs), particularly through the use of Template Haskell and generic programming.

## 1. Template Haskell for Code Generation

Template Haskell (TH) is extensively used throughout the codebase to generate boilerplate code at compile time. This process inherently involves manipulating or generating Haskell ASTs.

*   **Automatic Instance Derivation:** TH splices like `$(mkBeamInstancesForEnumAndList '')`, `$(enableKVPG '')`, `$(mkTableInstancesGenericSchema '')`, and `$(mkHttpInstancesForEnum '')` are prime examples. These macros take type names as input and generate the necessary `Beam`, `Aeson`, `Servant`, or HTTP-related instances. This generation process operates on the AST representation of the types.
*   **Database Schema Definition (`mkPersist` and Quasi-Quoters):** The `mkPersist` function combined with `[defaultQQ| ... |]` allows defining database schemas using a SQL-like syntax directly within Haskell code. This SQL-like definition is parsed into an internal representation (an AST of the schema definition), which then guides the generation of Haskell data types and Beam/Esqueleto instances.
*   **Custom Instance Generation:** In some cases, TH is used to generate custom instances that might not be covered by standard `deriving` clauses or generic functions.

## 2. Generic Programming and Data.Data

The `Data.Data` type class, often enabled via `DeriveDataTypeable`, allows for generic programming over algebraic data types. While not directly AST manipulation, it provides a mechanism to traverse and query the structure of data types at runtime, which is conceptually similar to working with an AST of the data.

*   **`allNothing` Function:** The `allNothing` function (seen in `BecknV2.OnDemand.Utils.Common.hs`) uses `gmapQ` and `ext1Q` from `Data.Generics.Aliases` to generically check if all fields in a data structure are `Nothing`. This is a form of generic traversal over the data type's structure.

## 3. Parser Combinators for Structured Data

Libraries like `Text.Parsec` are used for parsing structured text into Haskell data types. While not directly parsing Haskell code, these parsers build an internal representation (an AST of the parsed data) from a string input.

*   **`Gps` Parsing:** The custom `FromJSON` instance for `Gps` (in `Beckn.Types.Core.Taxi.Common.Gps.hs` and `Beckn.Types.Core.Metro.Search.Gps.hs`) uses `Text.Parsec` to parse a comma-separated string into `lat` and `lon` `Double` values. The parsing process effectively constructs a `Gps` data structure from a string representation.

## 4. Implications for Development

*   **Reduced Boilerplate:** TH significantly reduces the amount of manual boilerplate code, especially for data types that need many type class instances or database mappings.
*   **Compile-Time Safety:** Since code generation happens at compile time, many errors related to schema mismatches or instance definitions are caught early.
*   **Maintainability:** Changes to data models can often be propagated automatically through TH, reducing manual refactoring.
*   **Understanding Generated Code:** Developers need to understand the conventions and transformations applied by TH macros to effectively debug or extend the generated code.
