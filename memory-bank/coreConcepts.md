# Nammayatri Backend - Core Concepts

This document outlines fundamental concepts related to types, flow, Domain-Specific Languages (DSLs), and data handling within the Nammayatri backend, based on the analysis of the codebase.

## 1. Types and Type System

Haskell's strong, static type system is a cornerstone of the Nammayatri backend, ensuring correctness, reliability, and maintainability.

*   **Algebraic Data Types (ADTs) and Newtypes:**
    *   **ADTs:** Used extensively to model domain entities and their variations (e.g., `PaymentOrder`, `Event`, various BECKN message types). They provide a precise way to define data structures and enforce invariants.
    *   **Newtypes:** Frequently employed to introduce type safety and semantic meaning to existing types without runtime overhead (e.g., `Id`, `ShortId`, `LatLong`, `Time`, `Duration`, `Image`, `Tags`). This prevents mixing up different kinds of identifiers or values.
*   **Phantom Types:** Used to add type-level information without affecting runtime representation. Examples include `data Person`, `data Merchant`, `data Ride`, which serve as markers for `Id` newtypes (e.g., `Id Person`). This enhances type safety by distinguishing IDs of different entities.
*   **Generalized Algebraic Data Types (GADTs):** The `PaymentOrderE e` type demonstrates a sophisticated use of GADTs to parameterize data types by an "encryption state" (`e`). This allows the compiler to enforce that sensitive fields (like `clientAuthToken`) are handled appropriately (e.g., encrypted or decrypted) at compile time, providing strong security guarantees.
*   **Type Classes:**
    *   **Standard Type Classes:** `Show`, `Eq`, `Ord`, `Generic`, `ToJSON`, `FromJSON`, `ToSchema` are widely derived or manually implemented for data types, enabling serialization, comparison, and OpenAPI schema generation.
    *   **Custom Type Classes:** The project defines custom type classes like `ToTType`, `FromTType`, `TEntityKey`, `ToTType'`, `FromTType'`, `CompleteTagGroup`, `CompleteTag`. These define interfaces for specific functionalities (e.g., mapping between domain and database types, managing BECKN tags), promoting modularity and extensibility.
    *   **Monadic Type Classes:** `MonadFlow`, `SqlDB`, `Transactionable`, `MonadReader`, `HasFlowEnv`, `HasField`, `MonadGuid`, `MonadTime`, `CoreMetrics`, `Log`, `EncFlow` are used as constraints to define the capabilities required by functions, enabling dependency injection and managing side effects in a structured way.

## 2. Flow and Control Flow

The concept of "flow" in this project refers to the execution context and control flow mechanisms, often managed through monadic stacks.

*   **`FlowR` Monad:** This is a central monadic transformer used throughout the application (e.g., `type TestM = FlowR AppEnv`). It provides an environment (`AppEnv`) and capabilities for logging, error handling, and dependency injection.
*   **Monadic Stacks:** Functions are often defined with polymorphic monadic constraints (e.g., `(MonadFlow m, Log m) => ...`), allowing them to operate in various contexts as long as the required capabilities are provided by the underlying monad transformer stack.
*   **Error Handling:**
    *   **Custom Error Types:** Defined in `Kernel.Types.Error`.
    *   **Monadic Error Propagation:** `throwError` is used to propagate errors within the monadic flow.
    *   **Utility Functions:** `fromMaybeM`, `fromEitherM`, `fromEitherM'` are used to lift `Maybe` or `Either` values into the monadic error context, ensuring that missing data or failed operations result in proper error handling.
*   **Concurrency and Asynchronous Operations:**
    *   `Control.Concurrent.Async`: Used for concurrent execution and managing asynchronous results (e.g., `race`).
    *   `fork`: Used to run computations in separate lightweight threads, enabling non-blocking operations (e.g., event streaming to Kafka/Prometheus).
*   **Middleware:** Functions like `timeoutEvent` (in `Utils.Common.Events.hs`) demonstrate the use of middleware for intercepting and modifying requests/responses, often for cross-cutting concerns like timeouts or logging.

## 3. Domain-Specific Languages (DSLs)

The project leverages several DSL-like approaches to define configurations, schemas, and queries in a more declarative and concise manner.

*   **Template Haskell for Schema/Instance Definition:**
    *   **Database Schema (`mkPersist` and Quasi-Quoters):** The `[defaultQQ| ... |]` syntax provides an embedded DSL for defining database table schemas directly in Haskell, resembling SQL DDL. This allows for type-safe schema definition and automatic generation of corresponding Haskell types and ORM functions.
    *   **HTTP Instances (`mkHttpInstancesForEnum`):** Generates instances for HTTP-related type classes, effectively defining how enums behave in HTTP contexts.
*   **Servant for API Definition:** Servant uses a type-level DSL to define REST APIs. This allows for a declarative specification of endpoints, request/response bodies, query parameters, and headers, which is then used to generate server handlers and client functions.
*   **Esqueleto for Database Queries:** Esqueleto provides a type-safe, embedded DSL for writing SQL queries in Haskell. It allows constructing queries using Haskell operators (`from`, `where_`, `set`, `(=.)`, `(==.)`, `(^.)`, `val`), which are then translated to SQL. This is a powerful alternative to raw SQL strings.
*   **Dhall for Configuration:** Dhall is used as a configuration language (`FromDhall`). It provides a programmable, type-safe configuration language that can be embedded in Haskell, allowing for complex and validated configurations.
*   **Custom JSON Options:** While not a full DSL, the extensive use of custom `Aeson.Options` (e.g., `stripPrefixUnderscoreIfAny`, `constructorsWithHyphens`, `SumEncoding`) acts as a mini-DSL for controlling JSON serialization/deserialization rules declaratively.

## 4. Data Handling and Transformation

Data is central to the application, and various patterns are used for its modeling, serialization, validation, and transformation.

*   **Data Modeling:** Defined using Haskell's rich type system (ADTs, newtypes, GADTs) to accurately represent domain entities and their relationships.
*   **Serialization/Deserialization:**
    *   **JSON:** `Data.Aeson` is the primary library, with extensive use of generic derivation and custom `Options` for flexible mapping between Haskell types and JSON.
    *   **OpenAPI Schema:** `Data.OpenApi` and `Kernel.Utils.Schema` are used to automatically generate OpenAPI schemas from Haskell types, ensuring API documentation is consistent with the code.
*   **Database Persistence:**
    *   **ORM (Beam/Esqueleto):** Used for mapping Haskell data types to relational database tables and performing CRUD operations.
    *   **Custom Mapping Layers:** `ToTType`, `FromTType`, `TEntityKey` provide explicit control over how domain models are converted to and from database representations, handling complex types like `PostgresList` and encrypted fields.
*   **Data Validation:**
    *   **Type System:** Haskell's type system catches many errors at compile time.
    *   **Runtime Validation:** Custom `FromJSON` instances often include validation logic (e.g., `unless`, `fail`, `typeMismatch`) to ensure incoming data conforms to expectations.
    *   **Domain-Specific Validation:** Utility functions (e.g., `validateContext`, `validateDomain`, `validateAction` in BECKN modules) perform business logic validations on data.
*   **Data Transformation:**
    *   **Mapping Functions:** Numerous functions exist to map between different domain-specific enumerations or data structures (e.g., `mapVariantToVehicle`, `tripCategoryToFulfillmentType`).
    *   **Sensitive Data Masking:** Functions like `maskSensitiveData` and `maskNumber` use regular expressions and recursive traversal to sanitize sensitive information within data structures.
    *   **Time/Date Parsing/Formatting:** Utilities for converting between `UTCTime`, `NominalDiffTime`, and ISO8601 string representations.
*   **Encryption:** Sensitive data is encrypted and hashed using `Kernel.External.Encryption` before persistence or transmission, with type-level guarantees.
