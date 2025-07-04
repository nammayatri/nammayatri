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

# Technical Context

## Core Technologies

### Programming Language
- **Haskell**: Functional programming language with strong type system
- **GHC**: Glasgow Haskell Compiler for building applications
- **Cabal**: Package manager and build system

### Build & Development Environment
- **Nix**: Declarative package manager and build system
- **direnv**: Automatic environment management
- **Cabal Project**: Multi-package project management
- **HLS**: Haskell Language Server for IDE support

### Infrastructure & Services
- **PostgreSQL**: Multi-schema setup (atlas_driver_offer_bpp, atlas_app)
- **ClickHouse**: Analytics database for event tracking and metrics
- **Redis**: Both single instance (6379) and cluster (30001) configurations
- **Kafka**: Event streaming with topics (location-updates, broadcast-messages, dynamic-offer-driver-events-updates)
- **Docker**: Containerization for deployment
- **OSRM**: Routing engine with snap-to-road, distance calculations
- **CAC (Client Application Configuration)**: Feature management system
- **SuperPosition**: Advanced feature flagging and A/B testing

## Development Setup

### Prerequisites
1. **Nix Installation**: Required for reproducible builds
2. **direnv**: Automatic shell environment management
3. **VSCode**: Recommended IDE with Haskell extensions

### Development Workflow
```bash
# Environment setup
ln -s .envrc.backend .envrc
direnv allow

# Building
cd Backend
cabal build all

# Running services
, run-mobility-stack-dev

# Testing
cabal test all
```

### Port Configuration & External Integrations
| Service | Port | Key Features |
|---------|------|--------------|
| rider-app | 8013 | Customer APIs, FRFS, IGM, Payment webhooks |
| beckn-gateway | 8015 | BECKN protocol gateway |
| dynamic-offer-driver-app | 8016 | Driver APIs, Multiple webhooks |
| mock-registry | 8020 | Development registry service |
| transporter-scheduler | 8053 | Scheduling services |
| allocation-service | 9996 | Core driver allocation engine |

### External Integration Details
**Payment & Financial:**
- Juspay payment gateway (multiple webhook endpoints)
- Juspay payout system with mandate execution
- Manual payment link generation

**Verification Services:**
- Idfy document verification (multiple webhook configurations)
- HyperVerge verification (result + verification webhooks)
- Multi-merchant, multi-city verification support

**Communication:**
- SMS service with OTP handling (7891 fake SMS for dev)
- FCM notifications with scheduled delivery
- Exophone call status tracking and BAP notifications
- Safety alert system with webhook integration

**Analytics & Monitoring:**
- Event streaming to multiple Kafka topics
- ClickHouse analytics pipeline
- Location tracking service integration
- Vocalytics integration for voice analytics

## Technical Constraints

### Performance Requirements
- **Response Time**: < 500ms for critical APIs
- **Throughput**: Handle 10,000+ concurrent requests
- **Availability**: 99.9% uptime requirement
- **Scalability**: Horizontal scaling capability

### Memory & Resources
- **GHC Settings**: Optimized for production builds
- **Parallel Jobs**: 6 parallel compilation jobs by default
- **Development Flags**: Configurable optimization levels

### Protocol Compliance
- **BECKN/ONDC**: Mandatory protocol adherence
- **REST APIs**: RESTful service interfaces
- **Event Streaming**: Kafka-based async communication

## Dependencies Management

### Flake Inputs
External dependencies managed through `flake.nix`:
- **shared-kernel**: Common Haskell utilities
- **services-flake**: Infrastructure services
- **External repos**: Various third-party integrations

### Package Structure
Multi-package project with 40+ packages:
- **App packages**: Executable applications
- **Lib packages**: Shared libraries
- **Test packages**: Testing utilities

### Update Process
```bash
# Update specific input
nix flake lock --update-input shared-kernel

# Use local development
# Modify flake.nix to point to local path
```

## Tool Usage Patterns

### Development Tools
- **ghcid**: Fast compile feedback during development
- **cabal repl**: Interactive development environment
- **profiling**: Performance analysis tools
- **swagger**: API documentation generation

#### Code Generation & Compilation
- **Generate Haskell from YAML Specs**: `nix develop .#backend -c bash -c "cd Backend && , run-generator --apply-hint"`
- **Haskell Compilation (REPL for rider-app)**: `nix develop .#backend -c bash -c "cd Backend && cabal repl rider-app"`

#### API Specification Conventions
- **Endpoint Paths**: Use camelCase (e.g., `/nyRegular/`) for path segments in API YAML specifications, as hyphens (e.g., `/ny-regular/`) can cause issues with DSL processing and code generation.

#### Code Generation Rules
- **`src-read-only` Files**: Never edit files in `src-read-only` directories directly. All changes to these files must be achieved by modifying the source YAML specification files and then re-running the code generator (`nix develop .#backend -c bash -c "cd Backend && , run-generator --apply-hint"`).
- **Schema for External Types**: If an external type (like `Data.Time.Calendar.DayOfWeek`) lacks a necessary instance (e.g., `ToSchema`), this must be handled by instructing the DSL/code generator via the YAML specification on how to represent this type, or by defining a custom wrapper type in the YAML.
- **File Deletion**: If a Haskell source file (`.hs`) is deleted, ensure the corresponding `.cabal` file (usually by regenerating it via `hpack` from `package.yaml`, or by running the project's specific code generation/build script that includes `hpack`) is updated to remove the reference to the deleted module. Failure to do so will result in "can't find source for" errors during compilation.

### Testing Infrastructure
- **Unit Tests**: Cabal test suites
- **Integration Tests**: Newman-based API testing
- **Load Testing**: Performance testing framework
- **Mock Services**: Local testing with mock external APIs

### Monitoring & Operations
- **Prometheus**: Metrics collection
- **Grafana**: Monitoring dashboards
- **PgAdmin**: Database administration
- **Log aggregation**: Centralized logging

## Security Considerations

### Development Security
- **Local development**: Isolated Nix environments
- **Secrets management**: Environment-based configuration
- **Database access**: Controlled through connection pools

### Production Security
- **Container security**: Docker best practices
- **Network security**: Service mesh configuration
- **Data encryption**: At-rest and in-transit encryption

## Development Optimization

### Fast Development Builds
- Uncomment development flags in `cabal.project`
- Use `optimization: 0` for faster compilation
- Local flag configuration for development

### Resource Management
- Stack size limit adjustments for linking issues
- Parallel compilation configuration
- Memory usage optimization

## Key Learnings & Best Practices (from NYRegular API Setup)

1.  **DSL and Code Generation Workflow:**
    *   **Source of Truth**: YAML specification files (`spec/API/`, `spec/Storage/`) are the primary source for API and data structure definitions.
    *   **`src-read-only` Directories**: Files within these directories are automatically generated and **must not be edited directly**. Changes require modifying the source YAML and re-running the generator.
    *   **Code Generator Command**: Use `nix develop .#backend -c bash -c "cd Backend && , run-generator --apply-hint"` to generate Haskell code, format it (`treefmt`), and update Cabal files (`hpack`).
    *   **Manual Implementation Files**: The generator creates stubs in corresponding `src/` directories (e.g., `src/Domain/Action/UI/`) which need manual implementation of business logic.

2.  **Haskell Compilation & Debugging in this Project:**
    *   **Iterative Debugging**: Address compilation errors one by one, as fixing one can often lead to new, more specific errors.
    *   **`-Werror` Impact**: Project uses `-Werror`, meaning all GHC warnings are treated as compilation errors. This includes unused imports, dodgy imports, and potentially ambiguous fields if those specific warnings are enabled as errors.
    *   **Orphan Instances for External Types**:
        *   Place orphan instances (e.g., `ToSchema DayOfWeek`) in designated "Extra" files. For domain-related types, prefer `src/Domain/Types/Extra/*.hs`.
        *   The `EXTRA_API_TYPES_FILE` directive in API YAMLs creates `src/API/Types/UI/.../Extra.hs`, suitable for API-specific helper types or instances not tied to a domain model.
    *   **`ToSchema` for `DayOfWeek`**: The type `Data.Time.Calendar.DayOfWeek` lacks a `Generic` instance, so `genericDeclareUnNamedSchema` is unsuitable. A manual instance defining it as a string enum is required:
        ```haskell
        -- In an 'Extra.hs' file with appropriate imports:
        -- instance ToSchema DayOfWeek where
        --   declareNamedSchema _ =
        --     pure $
        --       NamedSchema (Just "DayOfWeek") $
        --         mempty
        --           & type_ ?~ OpenApiString
        --           & enum_ ?~ (map (Data.Aeson.String . T.pack . show) [Monday .. Sunday])
        ```
    *   **Cabal File Management**:
        *   The `run-generator` script typically handles `hpack` calls, updating `.cabal` files.
        *   If a module is deleted manually or if `hpack` doesn't correctly remove a reference, the `.cabal` file might need manual editing to remove the orphaned module from `exposed-modules` or `other-modules`.

3.  **API and Code Naming Conventions (for Namma DSL):**
    *   **API Endpoint Paths (YAML)**: Use camelCase for path segments (e.g., `/nyRegular/subscriptionDetails/`) rather than hyphens, as hyphens can interfere with DSL processing or function name generation.
    *   **Generated Haskell Function Names**: The DSL derives Haskell function names from endpoint paths. Ensure paths are structured to produce valid and unique Haskell identifiers. For example, `GET /module/entity/{id}` might generate `getModuleEntityById`.
    *   **Function Name Uniqueness**: If multiple GET (or other HTTP methods) endpoints target similar base paths (e.g., `/entities` and `/entities/{id}`), ensure the generator can distinguish them to create unique function names. Modifying paths (e.g., `/entityDetails/{id}`) or using `operationId` in the YAML (if supported by the DSL) can help.

4.  **General Workflow & Tool Interaction:**
    *   **Memory Bank Accuracy**: Strive to keep project-specific commands, conventions, and troubleshooting steps accurately documented in the memory bank.
    *   **File System State**: Be mindful that auto-formatters or other tools might change file contents (e.g., import order) between `read_file` and `replace_in_file` operations. Always use the latest known state for `SEARCH` blocks.
    *   **Verify Compilation Thoroughly**: A silent return from `cabal repl` doesn't guarantee full compilability. Use `cabal build <target>` for a more comprehensive check.
    *   **Consult Existing Code**: When unsure about a pattern (e.g., where to put orphan instances), look for similar existing code or "Extra" files in the project.

### Common Function Usage Patterns

*   **ID Generation**: Use `generateGUID` from `Kernel.Utils.Common`. It returns a `Flow (Id a)`, so it can be used directly as `newId <- generateGUID`.
*   **Database Inserts**: For creating a single new record, call the `create` function from the relevant `Storage.Queries` module directly (e.g., `void $ QNyRegularSubscription.create newSubscription`). Do not wrap single creations in `runInTransaction`.
*   **Error Handling**: Use specific error types from `Kernel.Types.Error` (e.g., `PersonNotFound`) with `fromMaybeM`. Example: `person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)`.
*   **Logging**: Use `logInfo` from `Kernel.Utils.Logging` for general logging.
*   **Beckn Tags**: Custom tags must be defined in `Backend/lib/beckn-spec/src/BecknV2/OnDemand/Tags.hs` before use.
*   **YAML Imports**: When importing types in YAML files, use the full module path instead of short names. For example, use `PlatformType: Domain.Types.IntegratedBPPConfig` instead of a short name. Always verify that the type exists in the specified module before adding an import.
*   **Compilation after Code Generation**: Always compile the code after running the code generator to ensure that the generated code is correct before proceeding with further changes.

---
## Detailed Tech Context from Memory Bank 2

### 1. Core Technologies
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

### 2. Development Environment & Setup
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

### 3. Data Storage, Persistence & Schema Management
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

### 4. API Design & Communication Patterns
-   **Beckn Protocol Integration:** A core aspect, influencing API design and data models for interoperability within the open mobility network. Visible in domain types (e.g., `bapId`, `bapUri`) and ACL (Anti-Corruption Layer) modules.
-   **Internal APIs (Dashboard & Inter-service):**
    -   Likely RESTful, defined using Servant in Haskell.
    -   API contracts are also specified in YAML files (e.g., `Backend/app/dashboard/CommonAPIs/spec/ProviderPlatform/Fleet/API/Driver.yaml`). These YAMLs define endpoints, HTTP methods, authentication requirements (`ApiAuthV2`), request/response types, query parameters, and sometimes links to older/helper APIs or migration metadata.
    -   Swagger is used for API documentation, generated from these specifications.
-   **Event-Driven Architecture:** Kafka is central for asynchronous communication, decoupling services, and processing events (e.g., `kafka-consumers` project).
-   **Authentication:** Paseto tokens are used for secure API authentication (`ApiAuthV2` specified in API YAMLs).

### 5. Code Generation & Metaprogramming (Haskell Backend)
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

### 6. Configuration Management
-   **Dhall:** Used for static service configuration, providing a typed and programmable configuration language (`dhall-configs/`).
-   **Database-driven Dynamic Configuration:** A significant portion of the system's behavior is controlled by configurations stored in PostgreSQL tables. These are defined via YAML specs (e.g., `configs.yaml`, `SubscriptionConfig.yaml`, `MerchantServiceUsageConfig.yaml`) and their corresponding Haskell types (`TransporterConfig.hs`, `SubscriptionConfig.hs`, etc.). This allows for dynamic updates without redeployment. This includes:
    -   Operational parameters for core logic (e.g., driver dispatch, pricing).
    -   Business rules and policies.
    -   Feature flags.
    -   Third-party service integration settings.
    -   Templates for communications (messages, notifications, overlays defined in `MerchantMessage.hs`, `Overlay.hs`).
    -   These configurations are often scoped by `MerchantOperatingCity`, enabling localized and provider-specific behavior.

### 7. Technical Constraints & Considerations
(To be filled: Any specific technical limitations or important considerations.)

### 8. Tool Usage Patterns
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
