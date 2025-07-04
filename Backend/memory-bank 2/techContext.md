# Tech Context: Namma Yatri

## 1. Core Technologies
-   **Backend:**
    -   Language: Haskell (GHC, likely with Stack or Cabal for build).
    -   Key Libraries/Frameworks:
        -   Servant (implied by API structure, common for Haskell web services).
        -   Beam (PostgreSQL ORM-like library).
        -   Aeson (JSON handling).
        -   Kafka client libraries.
        -   Redis client libraries.
        -   Paseto (token library).
        -   Template Haskell (for metaprogramming, e.g., deriving instances).
-   **Frontend:**
    -   Language: PureScript (transpiles to JavaScript).
    -   Build Tool: Node.js/npm, Nix.
    -   Mobile (Android): Native Android (Java/Kotlin implied, via Android Studio).
    -   Mobile (iOS): (Details not specified but `npm run start:ios` suggests React Native or similar cross-platform framework).
-   **Databases:**
    -   PostgreSQL (primary OLTP).
    -   Clickhouse (for analytical workloads/event logging).
    -   Redis (for caching, session management).
-   **Infrastructure & DevOps:**
    -   Nix: For build and development environment management.
    -   Direnv: For environment loading.
    -   Docker: For containerization.
    -   Services Flake: For managing external service dependencies (PostgreSQL, Redis, Paseto, OSRM, Kafka).
    -   Monitoring: Prometheus, Grafana.
    -   CI: Implied by `nix build` being run in CI.

## 2. Development Environment & Setup
-   **Common Prerequisite:**
    -   Nix: [Install Nix](https://nixos.asia/en/install)
    -   Direnv: [Install direnv](https://github.com/juspay/nixos-unified-template) (home-manager template recommended)
-   **Backend Setup:**
    1.  `ln -s .envrc.backend .envrc` (from project root, once)
    2.  `direnv allow` (from project root, once)
    3.  This activates a Nix shell with all dependencies.
    4.  Inside Nix shell, `cd ./Backend`
    5.  Build with `cabal build all`.
    6.  Run services with `cabal run <package_path>` or `ghcid` via `, ghcid <package_path>`.
-   **Frontend Setup:**
    1.  Enter dev shell: `nix develop .#frontend`
    2.  Navigate to `Frontend/ui-customer` and `Frontend/ui-driver`.
    3.  Install npm dependencies: `npm i` in each directory.
    4.  Start dev server: `npm start`.
-   **Android Native Setup:**
    1.  Open `android-native` in Android Studio.
    2.  Select build variant.
    3.  Add `google-services.json` to `app` folder.
    4.  Add `MAPS_API_KEY`, `USER_MERCHANT_ID`, `DRIVER_MERCHANT_ID` to `local.properties`.
-   **VS Code:**
    -   Open project as single-folder workspace.
    -   Install recommended extensions (especially Direnv and Haskell Language Server).
    -   Ensure Direnv extension is activated.

## 3. Data Storage, Persistence & Schema Management
-   **Primary OLTP Database:** PostgreSQL.
    -   **Schema Definition:** Database schemas are declaratively defined in YAML files located in `spec/Storage/` directories within backend service packages (e.g., `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/`).
    -   **YAML Spec Content:** These YAML files specify table names, fields, data types (Haskell and SQL), constraints (primary/secondary keys), default values, Beam ORM mappings (`beamType`, `beamFields`), data transformation logic (`fromTType`, `toTType`, `intermediateTransformers`), and predefined queries.
    -   **Haskell Domain Types:** Corresponding Haskell data types are defined in `.hs` files, typically in `Domain/Types/` directories (e.g., `Person.hs`, `FarePolicy.hs`, `SearchTry.hs`, `Overlay.hs`, `Plan.hs`, `SubscriptionConfig.hs`, `TransporterConfig.hs`, `DriverStats.hs`, `Vehicle.hs`, `MerchantMessage.hs`, `ConditionalCharges.hs`). These types often include enums for status, type, category, etc.
        -   Many types are parameterized with a `UsageSafety` phantom type (`'Safe` vs. `'Unsafe`) or an `EncryptionStatus` phantom type (`'AsEncrypted` vs. `'AsUnencrypted`).
        -   `Domain/Types/Extra/` subdirectories often contain supplementary types or enums specific to a domain entity (e.g., `Domain.Types.Extra.Plan` for `PlanBaseAmount`, `ServiceNames`; `Domain.Types.Extra.DriverPlan` for `CommodityData`).
    -   **Code Generation:** YAML specs and Haskell type definitions drive code generation (likely using Template Haskell) for Beam table definitions, JSON instances (Aeson), HTTP parameter instances (Servant), and other boilerplate, ensuring consistency.
    -   **Examples of YAML-defined tables & corresponding Haskell types:** `SearchTry` (`SearchTry.hs`, `SearchTry.yaml`), `DriverInformation` (`DriverInformation.yaml`, `Person.hs` as base), `Booking` (`Booking.yaml`), `Vehicle` (`Vehicle.hs`, `Vehicle.yaml`), `Plan` (`Plan.hs`, `Plan.yaml`), `DriverFee` (`DriverFee.yaml`), `CallStatus` (`CallStatus.yaml`), `Estimate` (`Estimate.yaml`), `Person` (`Person.hs`, `Person.yaml`), `configs.yaml` (defines multiple config entities like `DriverPoolConfig`, `MerchantState`, `SurgePricing`, `CancellationFarePolicy`, `TransporterConfig`, `MerchantMessage`, `LlmPrompt`, `MerchantOperatingCity`, `MerchantPaymentMethod`, `Overlay`, `MerchantServiceUsageConfig`, `DriverIntelligentPoolConfig`, `PayoutConfig`), `SubscriptionConfig` (`SubscriptionConfig.hs`, `SubscriptionConfig.yaml`), `DriverStats` (`DriverStats.hs`, `DriverStats.yaml`), `DriverPlan` (`DriverPlan.yaml`), `ConditionalCharges` (`ConditionalCharges.hs`, `ConditionalCharges.yaml`).
-   **Analytical/Event Database:** Clickhouse is used, indicated by `ClickhouseValue` instances for some domain types (e.g., `DriverMode`). This suggests its use for analytics or high-volume event logging.
-   **Caching:** Redis is employed for caching. Explicitly mentioned for `SurgePricing` and `CancellationFarePolicy` configurations, and likely used for other frequently accessed data or session management.
-   **Data Serialization:**
    -   JSON: Standard for API communication (using Aeson library in Haskell).
    -   JSON in DB: Some complex or variable-structure data is stored as JSON in PostgreSQL text/JSON columns (e.g., `issueBreachCooldownTimes` in `DriverInformation`, `extWebhookConfigs` in `SubscriptionConfig`).
-   **Sensitive Data Handling:**
    -   `EncryptedHashedField` and `DbHash` types are used for fields like mobile numbers and passwords, indicating encrypted and/or hashed storage.
    -   The `EncryptedItem` typeclass and associated functions (`encryptItem`/`decryptItem`) likely manage these cryptographic operations.
-   **Database Migrations:** Presence of `dev/migrations/` and `dev/migrations-read-only/` directories indicates that database schema changes are managed through a migration system.

## 4. API Design & Communication Patterns
-   **Beckn Protocol Integration:** A core aspect, influencing API design and data models for interoperability within the open mobility network. Visible in domain types (e.g., `bapId`, `bapUri`) and ACL (Anti-Corruption Layer) modules.
-   **Internal APIs (Dashboard & Inter-service):**
    -   Likely RESTful, defined using Servant in Haskell.
    -   API contracts are also specified in YAML files (e.g., `Backend/app/dashboard/CommonAPIs/spec/ProviderPlatform/Fleet/API/Driver.yaml`). These YAMLs define endpoints, HTTP methods, authentication requirements (`ApiAuthV2`), request/response types, query parameters, and sometimes links to older/helper APIs or migration metadata.
    -   Swagger is used for API documentation, generated from these specifications.
-   **Event-Driven Architecture:** Kafka is central for asynchronous communication, decoupling services, and processing events (e.g., `kafka-consumers` project).
-   **Authentication:** Paseto tokens are used for secure API authentication (`ApiAuthV2` specified in API YAMLs).

## 5. Code Generation & Metaprogramming (Haskell Backend)
-   **Template Haskell:** Extensively used for:
    -   Deriving instances for Beam (database interaction, e.g., `MakeTableInstances`, `mkBeamInstancesForEnumAndList`).
    -   JSON serialization/deserialization (Aeson).
    -   HTTP parameter handling (Servant, e.g., `mkHttpInstancesForEnum`).
    -   Generating Swagger schemas from Haskell types (`ToSchema`).
    -   Implementing encryption/decryption logic (`encryptItem`, `decryptItem`).
-   **YAML-to-Haskell Code Generation:** The detailed YAML specifications for storage (`spec/Storage/*.yaml`) and APIs (`spec/API/*.yaml`) are a cornerstone of the development process. They serve as a single source of truth and are used to generate:
    -   Haskell data types representing domain entities and API request/response bodies (though primary Haskell types are often manually defined and linked in YAML).
    -   Beam table definitions and ORM mappings.
    -   Data transformation functions between database representations and domain types.
    -   Boilerplate for CRUD operations or specific queries.
    -   Servant API type definitions and potentially server/client stubs.
    -   **NammaDSL**: The custom Domain Specific Language used for defining API and Storage specifications in YAML. It drives the code generation process for Haskell types, Beam mappings, queries, and API definitions. (See `memory-bank/namma_dsl_rules.md` for syntax and usage).

## 6. Configuration Management
-   **Dhall:** Used for static service configuration, providing a typed and programmable configuration language (`dhall-configs/`).
-   **Database-driven Dynamic Configuration:** A significant portion of the system's behavior is controlled by configurations stored in PostgreSQL tables. These are defined via YAML specs (e.g., `configs.yaml`, `SubscriptionConfig.yaml`, `MerchantServiceUsageConfig.yaml`) and their corresponding Haskell types (`TransporterConfig.hs`, `SubscriptionConfig.hs`, etc.). This allows for dynamic updates without redeployment. This includes:
    -   Operational parameters for core logic (e.g., driver dispatch, pricing).
    -   Business rules and policies.
    -   Feature flags.
    -   Third-party service integration settings.
    -   Templates for communications (messages, notifications, overlays defined in `MerchantMessage.hs`, `Overlay.hs`).
    -   These configurations are often scoped by `MerchantOperatingCity`, enabling localized and provider-specific behavior.

## 7. Technical Constraints & Considerations
(To be filled: Any specific technical limitations or important considerations.)

## 8. Tool Usage Patterns
-   **Nix:**
    -   Primary tool for managing development environments and dependencies (`nix develop`, `flake.nix`, `flake.lock`).
    -   Used for CI builds (`nix build`).
    -   Used to run full mobility stack (`run-mobility-stack-nix`) and individual services (e.g., `osrm-server`).
-   **Direnv:**
    -   Used to automatically load the Nix shell environment upon entering project directories.
    -   Essential for VS Code integration.
-   **Cabal:**
    -   Preferred tool for local backend development builds (`cabal build all`) due to speed.
    -   Used to run backend services locally (`cabal run`).
-   **npm:**
    -   Used for managing frontend dependencies and running development scripts (`npm i`, `npm start`).
-   **VS Code:**
    -   Recommended IDE with specific extensions for Haskell (haskell-language-server) and Direnv integration.
-   **Android Studio:**
    -   Used for building and running the native Android application.
-   **Git:** (Implied as version control, specific workflow not detailed in these READMEs but mentioned in root README).
-   **Shell Scripts:** Used for specific tasks like Android bundling (`bundling.sh`) and updating Juspay assets.
-   **Comma Commands (`,`)**: Custom aliases/scripts available within the Nix develop shell for common development tasks (e.g., `, ghcid`, `, run-mobility-stack-dev`).
