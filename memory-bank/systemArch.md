# Nammayatri Backend - System Architecture

The Nammayatri backend is a comprehensive mobility platform built in Haskell, implementing the BECKN/ONDC protocol. It follows a microservices architecture with over 40 services, designed for scalability, high performance, and reliability.

## 1. Overall Architecture

The system is logically divided into several main platforms and service categories:

*   **Rider Platform (Customer-facing):** Handles customer interactions, booking, search, and payment.
*   **Provider Platform (Driver/Fleet-facing):** Manages driver APIs, ride allocation, and business logic.
*   **Shared Services:** Provides common utilities and functionalities used across multiple platforms.
*   **Dashboard Services:** Operational and analytical dashboards for managing rider and provider activities.
*   **Kafka Consumers:** Processes asynchronous events from Kafka topics.
*   **Mock Services:** Used for development and testing, simulating external integrations.
*   **Utility Services:** Supporting functions and tools.

## 2. Core Platforms and Key Services

### Rider Platform
*   `rider-app`: Main customer APIs for booking, search, payment (Port: 8013).
*   `rider-app-scheduler`: Background job processing and notifications.
*   `search-result-aggregator`: Consolidates search results from multiple providers.
*   `public-transport-rider-platform`: Fixed Route Fixed Schedule (FRFS) services.
*   `rider-app-drainer`: Data pipeline processing for analytics.
*   `public-transport-search-consumer`: Asynchronous search processing.

### Provider Platform
*   `dynamic-offer-driver-app`: Driver APIs and business logic (Port: 8016).
*   `driver-offer-allocator`: Core ride allocation engine (Port: 9996).
*   `dynamic-offer-driver-drainer`: Analytics data processing.

### Dashboard Services
*   `rider-dashboard`: Customer operations dashboard.
*   `provider-dashboard`: Driver/fleet operations dashboard.
*   `safety-dashboard`: Safety monitoring and management.

## 3. Technical Stack

*   **Language:** Haskell (with GHC, Cabal, Nix, HLS).
*   **Architecture:** Microservices, Event-Driven.
*   **Protocol:** BECKN/ONDC (V1 and V2) for inter-platform communication.
*   **Databases:**
    *   **PostgreSQL:** Multi-schema setup (e.g., `atlas_driver_offer_bpp`, `atlas_app`) for transactional consistency. Esqueleto and Beam are used for type-safe database interactions.
    *   **ClickHouse:** Analytics database for event tracking and metrics.
    *   **Redis:** Used for caching, session management, and job scheduling (single instance and cluster configurations).
*   **Messaging/Streaming:**
    *   **Kafka:** Event streaming with various topics (e.g., `location-updates`, `broadcast-messages`, `dynamic-offer-driver-events-updates`).
*   **Infrastructure:** Containerized services with Docker.
*   **Mapping/Geospatial:** OSRM (routing engine), Google Maps API, geospatial queries in PostgreSQL.
*   **Feature Management:** CAC (Client Application Configuration), SuperPosition (feature flagging and A/B testing).
*   **External Integrations:**
    *   **Payment:** Juspay payment gateway (webhooks, payouts, mandates).
    *   **Verification:** Idfy, HyperVerge (document verification).
    *   **Communication:** SMS, FCM notifications, Exophone (call status).
    *   **AI/ML:** Azure OpenAI, Gemini (Chat Completion APIs).
    *   **Cloud Services:** AWS S3 (storage), AWS SNS (messaging).
    *   **Logging/Monitoring:** Prometheus (metrics), Grafana (dashboards), centralized logging.

## 4. Communication Patterns

*   **BECKN/ONDC Protocol:** Standardized message format for inter-platform communication, often event-driven and asynchronous.
*   **Internal Communication:**
    *   Direct API calls for synchronous operations (using Servant for API definition and EulerHS for HTTP clients).
    *   Kafka-based messaging for asynchronous operations and event sourcing.
    *   Shared database for transactional consistency.

## 5. Key Design Patterns

*   **Domain-Driven Design:** Clear domain boundaries between rider and provider contexts, with shared kernels for common logic.
*   **Event Sourcing:** State changes captured as events, providing an audit trail and enabling eventual consistency.
*   **CQRS (Command Query Responsibility Segregation):** Separation of read and write models for optimized queries and scalable data access.
*   **Monadic Programming:** Extensive use of monads (e.g., `IO`, `FlowR`, `SqlDB`) for managing side effects, control flow, and dependency injection.
*   **Type-Driven Development:** Strong type system of Haskell is leveraged to ensure correctness and safety, including newtype wrappers for domain-specific types and phantom types for type-level distinctions.
*   **Dependency Injection:** Achieved through `MonadReader` and `HasFlowEnv`/`HasField` type classes, allowing environments and tools to be passed implicitly.

## 6. Data Handling and Persistence

*   **Data Modeling:** Data types are defined with `Generic` derivations for automatic JSON serialization/deserialization (`Data.Aeson`) and OpenAPI schema generation (`Data.OpenApi`).
*   **Custom JSON Serialization:** Extensive use of custom Aeson `Options` (e.g., `stripPrefixUnderscoreIfAny`, `omitNothingFields`, `removeNullFields`, `constructorsWithHyphens`, `constructorTagModifier`, `SumEncoding`, `slashedRecordFields`) for precise control over JSON representation.
*   **Database ORM:**
    *   **Beam:** Used for defining table schemas and basic CRUD operations.
    *   **Esqueleto:** Used for building complex, type-safe SQL queries, including geospatial queries.
    *   **Custom Mapping Layers (`ToTType`, `FromTType`, `TEntityKey`):** Dedicated type classes for mapping between domain-specific data types and database tabular representations, ensuring a clean separation.
    *   **`PostgresList`:** For storing lists directly in PostgreSQL.
*   **Encryption:** Sensitive data (e.g., API keys, client tokens) is encrypted and hashed before storage, with type-level guarantees using GADTs (`EncryptedHashedField`).

## 7. Concurrency and Error Management

*   **Asynchronous Processing:** `Control.Concurrent.Async` and `fork` are used for non-blocking operations and event dispatching.
*   **Robust Error Handling:** A consistent error handling strategy using custom error types (`Kernel.Types.Error`), monadic error propagation (`throwError`), and utilities for converting `Maybe`/`Either` to errors (`fromMaybeM`, `fromEitherM`).

## 8. Testing Strategy

*   **Frameworks:** `Test.Tasty` (test organization), `Test.Tasty.HUnit` (unit tests), `Test.Tasty.Hspec` (BDD-style integration tests).
*   **Test Isolation:** `resetRedis` and other setup/teardown functions ensure clean test environments.
*   **Mock Services:** External dependencies are mocked to enable reproducible and isolated integration tests.
*   **Data-Driven Tests:** Test cases are often defined as structured data, allowing for easy expansion and maintenance.
