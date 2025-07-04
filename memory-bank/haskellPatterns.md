# Haskell Patterns

This file captures common Haskell patterns used in the Nammayatri backend.

## Common Patterns

*   **Functional Programming:** The codebase heavily utilizes functional programming principles, such as pure functions, immutability, and higher-order functions.
*   **Monads:** Monads are used extensively for managing side effects, such as I/O and state.
*   **Functors and Applicatives:** Functors and Applicatives are used for working with data structures in a generic way.
*   **Type Classes:** Type classes are used for defining interfaces and providing polymorphism.
*   **Lenses:** Lenses are used for accessing and modifying data structures in a composable way, especially for functional record updates (`Control.Lens`).
*   **Phantom Types:** Used to provide type-level distinctions for `Id` newtypes without adding runtime overhead (e.g., `data Person`, `data Merchant`).
*   **Newtype Wrappers:** Frequently used to provide type safety and semantic meaning to existing types (e.g., `Id`, `ShortId`, `Time`, `Duration`, `Image`, `Tags`).
*   **`Maybe` for Optional Fields:** Extensive use of `Maybe` to represent optional fields in data structures, allowing for flexible and nullable data.
*   **Enumerations:** Widely used to categorize domain concepts, often with custom serialization rules.

## Specific Patterns

*   **DerivingVia:** Used for deriving type class instances via another type (e.g., `PrettyShow` via `Showable`).
*   **GeneralizedNewtypeDeriving:** Used for deriving type class instances for newtypes.
*   **Template Haskell:** Used for generating code at compile time, including:
    *   `$(mkBeamInstancesForEnumAndList '')`: For generating Beam instances for enums and lists.
    *   `$(enableKVPG '')`: For enabling Key-Value Pair Generation for Beam tables.
    *   `$(mkTableInstancesGenericSchema '')`: For generating Beam table instances and schemas.
    *   `$(mkHttpInstancesForEnum '')`: For generating HTTP-related instances for enums (e.g., `FromHttpApiData`, `ToHttpApiData`).
    *   `mkPersist` and Quasi-Quoters (`defaultQQ`): For defining database table schemas in a declarative SQL-like syntax.

### Data Serialization

*   `Data.Aeson`: Used extensively for parsing and generating JSON data.
*   **Custom JSON Options:**
    *   `stripPrefixUnderscoreIfAny`: For handling JSON fields with leading underscores.
    *   `omitNothingFields = True`: For omitting fields with `Nothing` values during serialization.
    *   `removeNullFields`: For removing fields with null values during serialization.
    *   `constructorsWithHyphens`: For converting enum constructor names to hyphen-separated strings in JSON.
    *   `constructorsToLowerOptions`: For converting enum constructor names to lowercase in JSON.
    *   `constructorTagModifier`: For custom mapping of enum constructor names to specific JSON string values.
    *   Custom `SumEncoding`: For fine-grained control over how sum types are serialized to JSON (e.g., `tagFieldName`, `contentsFieldName`).
    *   `slashedRecordFields`: For transforming record field names into a "slashed" format in JSON.
*   **Manual `FromJSON` and `ToJSON` Instances:** Often implemented for complex data types or enumerations to provide fine-grained control over JSON representation and validation (e.g., `NewMessageEvent`, `Gps`, various BECKN enums).
*   `Data.OpenApi` and `Kernel.Utils.Schema`: Used for OpenAPI schema generation (`ToSchema`, `declareNamedSchema`, `genericDeclareUnNamedSchema`, `fromAesonOptions`).

### Database Interaction

*   `Database.Beam`: Used as an ORM (Object-Relational Mapper) for interacting with databases.
*   `Database.Postgres`: Used for interacting with PostgreSQL databases.
*   `Kernel.Storage.Esqueleto`: Used for building SQL queries in a type-safe manner, including `from`, `table`, `where_`, `set`, `(=.)`, `(==.)`, `(^.)`, `val`, `Esq.create`, `Esq.update`, `Esq.findById`, `Esq.findAll`, `Esq.findOne`, `Esq.deleteByKey`, `Esq.delete`.
*   `SqlDB` Monad: Database operations are performed within this monad.
*   `Transactionable m`: Type constraint indicating functions operate within a transactional context.
*   `PostgresList`: Used for storing lists of custom types or `Text` directly in PostgreSQL.
*   `derivePersistField`: Used to make custom types directly storable in the database.
*   `ToTType`, `FromTType`, `ToTType'`, `FromTType'`, `TEntityKey`: Custom type classes for defining bidirectional mappings between domain models and database tabular representations, including key conversions.
*   `Sequelize` (custom wrappers): Functions like `createWithKV`, `updateWithKV`, `findOneWithKV`, `findAllWithOptionsKV` provide a Sequelize-like interface for database interactions.

### Concurrency and Asynchronous Programming

*   `Control.Concurrent.Async`: Used for concurrent operations (e.g., `race`).
*   `fork` (from `Kernel.Prelude` or `EulerHS.Prelude`): Used for running computations in separate lightweight threads for asynchronous processing.

### Error Handling

*   `Kernel.Types.Error`: Defines custom error types.
*   `Kernel.Utils.Error.Throwing`: Provides utilities for throwing errors.
*   `fromEitherM`, `fromEitherM'`: For converting `Either` values to monadic errors.
*   `fromMaybeM`: For converting `Maybe` values to monadic errors.
*   `throwError`: For propagating custom errors.
*   `typeMismatch`, `parseFail`: Used in `FromJSON` instances for robust parsing error handling.

### External Service Integration

*   `Amazonka`, `Amazonka.SES`, `Amazonka.SNS.Publish`: Used for interacting with AWS services (SES for email, SNS for messaging).
*   `Servant`: Used for defining REST APIs and client-side API calls.
*   `EulerHS.Types (EulerClient, client)`: Used for making HTTP client calls.
*   `callAPI`: A helper function for making external API calls with error handling.
*   `JSONBS` Content Type: Used for handling raw JSON as `ByteString` in request bodies.
*   `Kernel.External.Encryption`: Provides encryption-related functionalities (`EncryptedField`, `EncryptedHashedField`, `decrypt`, `DbHash`) for handling sensitive data.
*   `Kernel.External.Maps`: For interacting with map services (e.g., Snap-to-road, OSRM).
*   `Kernel.External.Payment.Interface`: For integrating with external payment services.

### Testing

*   `Test.Tasty`, `Test.Tasty.HUnit`, `Test.Tasty.Hspec`: Used for organizing and running unit and integration tests.
*   `describe`, `it`, `before_`, `after_`, `runIO`: Standard Hspec constructs for structuring tests.
*   `Mock Services`: Explicitly started and configured for integration testing to ensure reproducible and isolated tests.
*   `AppEnv` Record: Defines the application environment for tests, managing dependencies and configurations.
*   `wrapTests`: A higher-order function for encapsulating complex test setup.
*   `shouldBe`, `shouldSatisfy`, `equalsEps`: Standard and custom assertions.
*   `LocationUpdates` (nested `NonEmpty` lists): For representing complex test data for geographical routes.

### Utility and Common Modules

*   `Kernel.Prelude`, `EulerHS.Prelude`: Custom preludes providing common utilities and re-exports.
*   `Kernel.Utils.Common`: Contains various general-purpose utility functions.
*   `Kernel.Utils.Dhall`: For Dhall configuration loading (`FromDhall`).
*   `Kernel.Utils.Example`: For providing example values for data types.
*   `Kernel.Utils.GenericPretty`: For pretty-printing data types.
*   `Kernel.Utils.IOLogging`: For logging functionalities.
*   `Kernel.Utils.Servant.SignatureAuth`: For handling signature authentication in Servant.
*   `Data.Time`, `Data.Time.Format.ISO8601`: For handling time and date, including ISO8601 parsing and formatting.
*   `Text.Parsec`: For parser combinators, used in custom JSON parsing (e.g., `Gps`).
*   `Text.Regex.Posix`: For regular expression matching (e.g., in sensitive data masking).
*   `Control.Monad.Extra`: Provides extra monadic functions (e.g., `anyM`).
*   `Data.Set`: Used for efficient categorization and membership testing of enums.
*   `Data.Map`: Used for grouping and aggregating data.

## Architectural Patterns

*   **Environment Separation (`AppEnv` vs. `HandlerEnv`)**: The application strictly separates the environments for different entry points. The main web application runs in a rich `AppEnv` with full access to external clients, caches, and complex configurations. Background workers and scheduler jobs, however, run in a leaner `HandlerEnv`. To use core application logic from a background job, the `HandlerEnv` must be explicitly *enriched* to satisfy the constraints of the function being called (e.g., providing the components required by `SearchRequestFlow`). Direct calls will fail due to this environmental mismatch.

*   **Adapter over Re-implementation**: When adding functionality in a new context (like a scheduler job), the correct pattern is to create a simple "adapter" function. This adapter prepares the necessary data and then calls the existing, core application logic (e.g., `DSearch.search`). Re-implementing core logic from scratch is an anti-pattern that leads to bugs, architectural divergence, and maintenance overhead. The goal is to reuse, not rebuild.
