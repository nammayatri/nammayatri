# Nammayatri Backend - Data Flow and Handling

This document describes how data flows between different modules and systems (both internal and external) within the Nammayatri backend, based on the analysis of the codebase.

## 1. Data Flow within Microservices

Within the microservices architecture, data primarily flows through:

*   **In-memory Data Structures:** Haskell's strong type system ensures data integrity within individual services. Data is modeled using Algebraic Data Types (ADTs) and Newtypes, providing type-safe representations of domain entities.
*   **Function Parameters and Return Values:** Data is passed between functions as arguments and returned as results, adhering to the functional programming paradigm.
*   **Monadic Contexts (`FlowR`):** Shared data and configurations (e.g., `AppEnv`) are implicitly passed through the `FlowR` monad via `MonadReader` and `HasField` type classes, enabling access to environment-specific data without explicit passing.
*   **Internal API Calls:** Services communicate synchronously via direct HTTP API calls (using Servant for API definition and EulerHS for client calls). Data is serialized to JSON for transmission and deserialized upon receipt.

## 2. Data Persistence and Retrieval

Data is persisted and retrieved from various databases:

*   **PostgreSQL:**
    *   **Storage:** Domain data (e.g., `PaymentOrder`, `SpecialLocation`, `GateInfo`) is stored in PostgreSQL.
    *   **ORM:** `Database.Beam` and `Kernel.Storage.Esqueleto` are used for type-safe object-relational mapping.
    *   **Data Mapping:** Custom type classes (`ToTType`, `FromTType`, `TEntityKey`, `ToTType'`, `FromTType'`) handle the bidirectional mapping between Haskell domain types and database tabular representations. This includes handling `PostgresList` for array-like data and reconstructing/deconstructing encrypted fields (`EncryptedHashedField`).
    *   **Queries:** Data is retrieved using Esqueleto's DSL for complex queries, including geospatial queries.
*   **Redis:**
    *   **Caching:** Used for caching frequently accessed data (e.g., location updates).
    *   **Session Management:** Stores session-related information.
    *   **Job Scheduling:** Manages scheduled tasks.
    *   **Interaction:** `Kernel.Storage.Hedis.Queries` provides functions for interacting with Redis.
*   **ClickHouse:**
    *   **Analytics:** Used as an analytics database for storing and querying event tracking and metrics data.
    *   **Data Ingestion:** Data is likely ingested from Kafka topics into ClickHouse for analytical purposes.

## 3. Inter-Service Communication (Asynchronous)

Asynchronous data flow is primarily handled via Kafka:

*   **Kafka as Event Bus:** Kafka is used as a central event streaming platform.
*   **Event Production:** Services produce events (e.g., `Event` from `lib/sessionizer-metrics/`) to specific Kafka topics (`lib/producer/`).
*   **Event Consumption:** Dedicated Kafka consumer services (`app/kafka-consumers/`) subscribe to topics and process incoming events.
*   **Data Format:** Events are typically serialized to JSON before being sent to Kafka.
*   **Transaction Logs:** Transaction logs are pushed to Kafka topics (`KafkaLogs.TransactionLogs`).

## 4. External System Integrations

Data flows between the Nammayatri backend and various external third-party systems:

*   **BECKN/ONDC Protocol:**
    *   **Request/Response:** Data is exchanged with other BECKN-compliant platforms (BAPs, BPPs) via standardized JSON messages over HTTP.
    *   **API Definitions:** `lib/beckn-spec/` defines the precise structure of these messages and the Servant APIs for interaction.
    *   **Validation:** Incoming BECKN messages undergo rigorous validation (e.g., context, domain, action, version) to ensure protocol compliance.
*   **Payment Gateways (Juspay):**
    *   **Requests:** Payment-related data (e.g., order details, amounts) is sent to Juspay for processing.
    *   **Webhooks:** Juspay sends back payment status updates and other transaction details via webhooks, which are consumed by the backend (`lib/webhook/`).
    *   **Sensitive Data:** Payment data, especially client authentication tokens, is encrypted and hashed before storage or transmission.
*   **Mapping Services (OSRM, Google Maps):**
    *   **Requests:** Location data (e.g., `LatLong` points) is sent to map services for route calculation, snap-to-road functionality, and distance estimation.
    *   **Responses:** Map services return calculated distances, snapped points, and other geospatial information.
*   **Communication Services (AWS SES, AWS SNS, Exophone, FCM):**
    *   **Emails:** Data for email content (e.g., OTPs) is sent to AWS SES.
    *   **Notifications:** Messages are published to AWS SNS for various notifications (e.g., Slack integration).
    *   **SMS/FCM:** Data for SMS and push notifications is sent to respective services.
*   **Verification Services (Idfy, HyperVerge):**
    *   **Requests:** User/document data is sent for verification.
    *   **Webhooks:** Verification results are received via webhooks.
*   **AI/ML Services (Azure OpenAI, Gemini):**
    *   **Requests:** Chat completion requests (e.g., user messages, context) are sent to LLM providers.
    *   **Responses:** LLM providers return generated text responses.
*   **Configuration Management (Dhall):** Configuration data is defined in Dhall files and loaded into the application environment at startup, influencing various data flows and behaviors.

## 5. Data Transformation and Validation Points

Data undergoes various transformations and validations at different stages of its flow:

*   **Serialization/Deserialization:** Data is converted between Haskell types and external formats (JSON, database rows) using Aeson, Beam, and Esqueleto, often with custom rules.
*   **Encryption/Decryption:** Sensitive data is encrypted before storage/transmission and decrypted upon retrieval.
*   **Masking:** Sensitive data (e.g., phone numbers) is masked before logging or display.
*   **Domain Mapping:** Data is mapped between different domain-specific enumerations or data structures (e.g., `VehicleCategory` mappings).
*   **Input Validation:** API endpoints and internal functions validate incoming data against expected formats and business rules.
*   **Error Handling:** Consistent error handling mechanisms (`throwError`, `fromMaybeM`) ensure that data flow issues are properly managed and reported.
