# System Patterns: Namma Yatri

## 1. System Architecture Overview
The Namma Yatri backend is structured as a collection of microservices/packages, primarily written in Haskell. It appears to be event-driven, utilizing Kafka for message consumption. The architecture separates concerns into rider platform, provider platform, dashboards, Kafka consumers, mocks, and utility services.
The frontend consists of UI components for customers (riders) and drivers, likely interacting with the backend via APIs. Mobile applications are native Android and iOS.
Nix is used for overall environment and build management, suggesting a focus on reproducible builds and dependency management.

## 2. Key Technical Decisions
-   **Strong Static Typing & Functional Programming:** Haskell is used for the backend, and PureScript for the frontend, emphasizing type safety and functional paradigms to enhance code reliability and maintainability.
-   **Template Haskell for Metaprogramming:** Heavily used in the Haskell backend (e.g., `mkBeamInstancesForEnumAndList`, `mkHttpInstancesForEnum`, `mkBeamInstancesForJSON`, `encryptItem`, `decryptItem`) to reduce boilerplate for deriving instances for database interaction (Beam), JSON serialization (Aeson), HTTP parameter handling, schema generation, and field-level encryption. This keeps domain type definitions concise.
-   **Nix for Environment and Build Management:** Chosen for reproducible builds and consistent development environments across the team and in CI/CD pipelines.
-   **Microservices/Service-Oriented Architecture (SOA) with Event-Driven Elements:** The backend is divided into multiple packages and executables (e.g., `rider-app`, `dynamic-offer-driver-app`, `kafka-consumers`), indicating a modular architecture. Kafka is used for event-driven communication between services.
-   **Open Standards Integration (Beckn):** Adherence to the Beckn protocol for interoperability in the mobility network is a core design principle, visible in various domain types (e.g., `bapId`, `bapUri` in `SearchRequest`) and ACL modules.
-   **Multi-Database Strategy:** PostgreSQL is the primary OLTP database, likely accessed via the Beam library. Clickhouse is also used, suggested by `ClickhouseValue` instances for certain types (e.g., `DriverMode`), possibly for analytics or high-volume event logging.
-   **Shared Kernel for Core Types:** A `Kernel` module provides common foundational types (e.g., `Id a` (typed identifiers), `HighPrecMoney`, `Currency`, `UTCTime`, `EncryptedHashedField`, `DbHash`) used across various domain modules, promoting consistency.
-   **Phantom Types for Enhanced Safety:** Use of phantom types (e.g., `UsageSafety` in `FarePolicyD`, `EncryptionStatus` (`e`) in `PersonE e`, `TransporterConfigD s`) to enforce domain constraints and differentiate type states (e.g. `'AsEncrypted` vs `'AsUnencrypted`, `'Safe` vs `'Unsafe`) at compile time.
-   **Field-Level Encryption for Sensitive Data:** Critical fields like mobile numbers are stored encrypted using `EncryptedHashedField` and managed via an `EncryptedItem` typeclass.
-   **Declarative Schema & Configuration (YAML):** YAML files in `spec/Storage/` are used to define database table schemas, Haskell type mappings (via Beam), data transformations, default values, and predefined queries. This "schema-as-code" approach likely drives code generation and ensures consistency. Examples: `SearchTry.yaml`, `DriverInformation.yaml`, `Booking.yaml`, `Vehicle.yaml`, `Plan.yaml`, `DriverFee.yaml`, `CallStatus.yaml`, `Estimate.yaml`, `Person.yaml`, `configs.yaml` (for `DriverPoolConfig`, `MerchantState`, `SurgePricing`, `CancellationFarePolicy`, etc.), `SubscriptionConfig.yaml`.
-   **Highly Configurable Business Logic:** Many operational parameters, business rules, and feature flags are managed through database configuration tables (defined in YAML specs like `configs.yaml`, `SubscriptionConfig.yaml`), allowing for dynamic adjustments per merchant, city, or service.

## 3. Core Components & Services
The system is composed of several key services and domain entities, with a strong emphasis on configurability at the merchant and operating city level. A "shared kernel" approach is used to promote code reuse and consistency across services.

-   **Shared Kernel Functions:**
    -   These functions are located in the `Backend/lib/` directory, particularly in modules under `Backend/lib/utils` and `Backend/lib/shared-services`.
    -   They are designed to be generic and reusable across multiple services.
    -   Examples include functions for data validation, error handling, logging, and database interaction.
    -   Naming conventions for shared-kernel functions typically involve a common prefix or suffix (e.g., `validate...`, `handleError...`, `log...`).
    -   The `Kernel` module also provides common foundational types and typeclasses.

-   **Backend Services (Haskell):** (Structure as previously identified)
    -   Rider Platform (`rider-app`): Acts as a Beckn Application Platform (BAP). Manages rider interactions, initiates searches, processes offers, and handles booking state from the rider's perspective.
    -   Provider Platform (`dynamic-offer-driver-app`): Acts as a Beckn Provider Platform (BPP). Manages drivers, vehicles, fare policies, generates quotes/estimates, handles driver allocation, and fulfills ride requests.
    -   Dashboard Services, Kafka Consumers, Mocks, Utility Services.
-   **Core Domain Entities (examples from reviewed `.hs` and `.yaml` files):**
    -   `Person`: Represents users (drivers, admins, fleet owners, operators) with comprehensive details including PII (encrypted), roles, device info, etc. (Defined in `Person.hs` and `Person.yaml`).
    -   `DriverInformation`: Extends `Person` with driver-specific operational data, preferences, status flags, referral info, payment details, blocking/cooldowns. (Defined in `DriverInformation.yaml`).
    -   `Vehicle`: Details about vehicles, including make, model, registration, capacity, features (AC, oxygen), service tiers, registration category. Linked to a driver. (Defined in `Vehicle.hs` and `Vehicle.yaml`).
    -   `SearchRequest`: Captures ride search details (locations, times, user preferences, Beckn context, parcel info). Persisted by the BAP (`rider-app`).
    -   `SearchTry`: Logs each attempt/phase of a search request, linking to `SearchRequest` and `Estimate`(s). Includes `SearchRepeatType` and `SearchTryStatus` enums. (Defined in `SearchTry.hs` and `SearchTry.yaml`).
    -   `Estimate`: Stores fare estimates from BPPs, incorporating dynamic pricing factors. Persisted by the BAP (`rider-app`) upon receiving `on_search` responses. (Defined in `Estimate.yaml`).
    -   `Quote`: Represents a specific, often driver-tied, quote from a BPP in response to a `select` or as part of `on_select`. Persisted by the BAP (`rider-app`). (Defined in `Quote.yaml`).
    -   `DriverQuote`: A specific quote offered by/to a driver on the BPP side (`dynamic-offer-driver-app`). (Defined in `DriverQuote.yaml`).
    -   `Booking`: Represents a confirmed ride or delivery. Both BAP and BPP maintain their own `Booking` records, linked via Beckn transaction IDs and BPP booking IDs. (Defined in `Booking.yaml` in both services).
    -   `Ride`: Represents an active or completed trip, usually created after a booking is fully confirmed and a driver assigned. Persisted by both BAP and BPP.
    -   `FarePolicy`: Defines complex fare structures.
    -   `ConditionalCharges`: Defines specific charges linked to a `FarePolicy`.
    -   `Plan`: Represents subscription or service plans.
    -   `DriverPlan`: Links a `Driver` to a `Plan`.
    -   `DriverFee`: Manages various fees for drivers.
    -   `CallStatus`: Logs details of calls.
    -   `Merchant`: Core entity for providers/fleet owners.
    -   `MerchantOperatingCity`: Defines a merchant's operational presence and settings in a specific city.
    -   **Configuration Entities:** `DriverPoolConfig`, `MerchantState`, `SurgePricing`, `CancellationFarePolicy`, `SubscriptionConfig`, `TransporterConfig`, `MerchantMessage`, `Overlay`, `MerchantServiceUsageConfig`, `DriverIntelligentPoolConfig`, `PayoutConfig`.
    -   `DriverStats`: Aggregated statistics for drivers.
-   **Shared Kernel (`Kernel.Prelude`, `Kernel.Types`, `Kernel.External.*`):** Provides foundational types and typeclasses.
-   **External Service Dependencies:** PostgreSQL, Clickhouse, Redis, Kafka, Paseto, OSRM Server, and various third-party services.
-   **Frontend Components (PureScript, Native Mobile):** `ui-customer`, `ui-driver`, native mobile shells.

## 4. Data Flow & Storage
-   **Primary OLTP Database:** PostgreSQL (via Beam). Schemas defined in YAML.
-   **Analytical/Event Database:** Clickhouse.
-   **Caching:** Redis.
-   **Message Brokering / Event Streaming:** Kafka.
-   **API Communication:**
    -   Internal: RESTful APIs (Servant). Swagger for documentation.
    -   External: Beckn protocol.
-   **Configuration Management:** Dhall and Database-driven Configuration.
-   **Data Persistence & Mapping (Beam):** YAML specs drive Haskell types and Beam table definitions.

## 5. Common Design Patterns
-   **Anti-Corruption Layer (ACL):** Modules named `ACL` (e.g., `Beckn.ACL.OnSearch`) are used to translate between external Beckn protocol types and internal domain types for each Beckn action (`search`, `on_search`, `select`, `on_select`, etc.). This isolates the core domain logic from the specifics of the Beckn protocol.
-   **Asynchronous Processing with Forking:** Many Beckn callback handlers (e.g., for `on_search`, `on_select`, `on_init`, `on_confirm`) perform initial validation and transformation, acknowledge the request (`AckResponse`), and then fork the main domain logic into a background thread (`fork "..." $ ...`). This ensures timely responses to network partners.
-   **Idempotency using Redis Locks:** Redis locks (e.g., `whenWithLockRedis`) are used extensively in Beckn callback handlers, typically keyed by `message_id` or a combination of `message_id` and `subscriber_id`, to prevent duplicate processing of the same incoming message.
-   **Domain-Driven Design (DDD) Influence:** Separation of API handlers, Domain Actions (business logic), and Storage Queries suggests an adherence to DDD principles.
-   **Repository Pattern (Implicit):** Modules in `Storage/Queries/` and `Storage/CachedQueries/` act as repositories, abstracting data access logic.
-   **Event Sourcing (Partial/Potential):** Use of Kafka and event triggering (e.g., `triggerEstimateEvent`, `triggerQuoteEvent`, `triggerBookingCreatedEvent`) suggests elements of event sourcing or at least event-driven updates.
-   **Scheduled Job Automation (NYRegular Feature):**
    *   **Master/Child Scheduler Pattern:** A daily master job (`NyRegularMasterSchedulerJob`) identifies all individual ride instances due for active subscriptions (`NyRegularSubscription` table). It then enqueues child jobs (`ProcessSingleNyRegularInstanceJob`) into a persistent job queue (e.g., `RegularRideInstanceJobs` table) for each instance, with a target execution time.
    *   **Initial Price Establishment for Subscription:** Before automated bookings for a subscription begin, the recurring "fixed price" is established. The BAP (`rider-app`) initiates a `/search` to the target BPP. The BPP calculates a comprehensive estimate, including any conditional charges specific to the regular, scheduled nature of the service. This BPP-originated estimate is returned via `/on_search` and then persisted by the BAP in the `NyRegularSubscription` as the "fixed price" for all subsequent automated instances.
    *   **Automated Beckn Flow Initiation:** The child job, when triggered, constructs a Beckn `/search` message. This message is tagged to indicate it's for an NYRegular instance and includes the pre-agreed fixed price. It's sent to the specific BPP associated with the subscription.
    *   **Context-Aware Callback Handling:** The BAP's (`rider-app`) `on_search` handler is enhanced to detect if an incoming `on_search` response is for an NYRegular instance (e.g., by checking flags on the `SearchRequest` linked via the unique `transaction_id` or by BPP echoing tags). If it is, and the BPP's offer matches the fixed price, the `on_search` handler automatically triggers the subsequent Beckn `/select` call to the BPP, bypassing manual user selection for that instance.
    *   The rest of the Beckn flow (`on_select`, `init`, `on_init`, `confirm`, `on_confirm`) proceeds using existing handlers, driven by this automated initiation.
    *   The outcome of each automated booking attempt is logged in `NyRegularInstanceLog`.
    *   **Scheduled Job Automation & State Management (NYRegular Feature):**
        *   **Master/Child Scheduler Pattern:** A daily master job (`NyRegularMasterSchedulerJob`) runs to identify and schedule ride instances for all active subscriptions. This serves as a reliable fallback and ensures consistency.
        *   **State Hashing for Job Validation:**
            *   A canonical hash (`schedulingHash`) is computed from the scheduling-critical fields of a `NyRegularSubscription` (e.g., status, recurrence rules, times, pause periods). This hash is stored on the subscription record itself.
            *   When a child job (`ProcessSingleNyRegularInstanceJob`) is created (either by the master job or proactively), it stores the parent subscription's current `schedulingHash` in its job data as `expectedSchedulingHash`.
            *   Upon execution, the child job fetches the latest subscription data, re-calculates the hash, and **terminates itself** if the live hash does not match the `expectedSchedulingHash`. This prevents jobs from running with outdated parameters.
        *   **Proactive & Idempotent Job Creation:**
            *   When a `NyRegularSubscription` is updated via its API endpoint (`postNyRegularSubscriptionsUpdate`), the system immediately recalculates and stores the new `schedulingHash`.
            *   If the hash has changed, indicating a significant scheduling modification, the system proactively attempts to schedule the *next single applicable ride instance*.
            *   To prevent race conditions with the master scheduler or rapid updates, this proactive creation is made idempotent using a Redis lock (`setNxExpire`). The lock key is uniquely composed of the subscription ID, the new `schedulingHash`, and the specific timestamp of the instance being created.
        *   **Timezone-Aware Scheduling:** Job instance creation correctly interprets the subscription's `scheduledTimeOfDay` in the merchant's local timezone (retrieved from `RiderConfig`) to calculate the precise UTC execution time.
        *   **Initial Price Establishment & Beckn Flow:** (This part of the pattern remains the same) The recurring "fixed price" is established via an initial BAP-BPP search flow. The automated child job then initiates the Beckn flow for each instance, which is handled by existing callback logic.

## 6. API Design & Communication
-   **API Style:** RESTful (Servant, Swagger).
-   **Inter-service Communication:** Direct API calls, Kafka.
-   **External Communication (Beckn Ride Booking Flow):**
    1.  **`search` (BAP -> BPP):** Rider initiates search via BAP (`rider-app`). BAP sends Beckn `search` to relevant BPPs (`dynamic-offer-driver-app`).
        *   BAP creates `SearchRequest`.
    2.  **`on_search` (BPP -> BAP):** BPPs respond with offers/estimates.
        *   BAP receives `on_search`, validates, transforms (ACL), and stores `Estimate` and `Quote` entities linked to the `SearchRequest`.
    3.  **Rider Fetches Quotes (Client -> BAP):** Rider app polls BAP for available offers.
    4.  **`select` (BAP -> BPP):** Rider selects an offer. BAP updates its state and sends Beckn `select` to the chosen BPP.
        *   BAP's `Domain.Action.UI.Select` prepares for this.
    5.  **`on_select` (BPP -> BAP):** BPP processes the `select` (initiates driver allocation) and responds with `on_select`, often containing driver-specific quotes.
        *   BPP's `Domain.Action.Beckn.Select` handles driver search initiation.
        *   BAP receives `on_select`, validates, transforms (ACL), and stores new driver-specific `Quote` entities. May auto-confirm or notify rider.
    6.  **`init` (BAP -> BPP):** Rider confirms a driver quote (or BAP auto-confirms). BAP creates a local `Booking` and sends Beckn `init` to BPP.
        *   BAP's `Domain.Action.UI.Confirm` handles UI confirmation and triggers `init`.
    7.  **`on_init` (BPP -> BAP):** BPP processes `init`, creates its own `Booking`, and responds with `on_init` (confirming details, payment info).
        *   BPP's `Domain.Action.Beckn.Init` handles BPP-side booking creation.
    8.  **`confirm` (BAP -> BPP):** BAP processes `on_init`, updates its `Booking`, and sends Beckn `confirm` to BPP.
        *   BAP's `Domain.Action.Beckn.OnInit` handles this.
    9.  **`on_confirm` (BPP -> BAP):** BPP processes `confirm`, finalizes its `Booking`, creates `Ride` entity, and responds with `on_confirm` (final ride details).
        *   BPP's `Domain.Action.Beckn.Confirm` handles BPP-side ride creation.
    10. **BAP processes `on_confirm`**: Updates its `Booking` to `TRIP_ASSIGNED`, creates local `Ride` entity, notifies rider. Ride is booked.
        *   BAP's `Domain.Action.Beckn.OnConfirm` handles this.

## 7. Scalability & Performance Considerations
(To be filled: Strategies for ensuring the system can scale and perform under load.)

## 8. Security Patterns
(To be filled: Common security measures and patterns implemented, e.g., Authentication, Authorization, Data Encryption.)
