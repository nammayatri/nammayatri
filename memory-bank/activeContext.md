# Active Context: Namma Yatri Memory Bank Population

## Current Work Focus
The primary focus has been on understanding the Namma Yatri project structure, key entities, and technical architecture by analyzing various source code files (`.yaml` specifications, Haskell domain types, shared logic, and query modules). This understanding is being used to populate the core Memory Bank documents (`projectbrief.md`, `productContext.md`, `systemPatterns.md`, `techContext.md`).

## Recent Changes & Learnings
-   **Directory Structure Analysis:** The provided comprehensive directory structure has been analyzed to understand the overall project layout, identify key components (Backend, Frontend, docs, dev, lib, etc.), and locate relevant files for deeper inspection. This initial structural overview was crucial for navigating the codebase.
-   **YAML Specifications:** Reviewed several `spec/Storage/*.yaml` files, revealing declarative schemas for database tables, Haskell type mappings (Beam), data transformations, and default values. Key configuration entities like `DriverPoolConfig`, `SubscriptionConfig`, `TransporterConfig`, `FarePolicy`, `MerchantMessage`, `Overlay` are defined here.
-   **Haskell Domain Types:** Analyzed corresponding `.hs` files in `Domain/Types/` and `Domain/Types/Extra` for entities like `Person`, `SearchRequest`, `FarePolicy`, `Plan`, `DriverPlan`, `DriverFee`, `Vehicle`, `SubscriptionConfig`, `TransporterConfig`, `Overlay`, `MerchantMessage`, `ConditionalCharges`, `DriverStats`, `SearchTry`. This provided insights into data structures, enums, and relationships.
-   **Shared Logic Modules:** Examined modules in `SharedLogic/` such as `DriverFee.hs` (and its sub-modules for job processing), `FarePolicy.hs`, `Payment.hs`, and `Allocator.hs` (job type definitions). These files revealed core business logic for fee calculation, payment processing, fare policy selection (including dynamic pricing via Yudhishthira), and background job orchestration.
-   **Storage & Query Modules:** Reviewed files in `Storage/Queries/` and `Storage/Queries/OrphanInstances/`, which define database interaction patterns (CRUD, specific finders) and `FromTType'/ToTType'` instances for mapping between Beam table types and domain model types. This highlighted data transformation and backfilling logic.
-   **Key Architectural Patterns:**
    -   Declarative schema definition via YAML.
    -   Extensive use of Template Haskell for boilerplate reduction.
    -   Database-driven dynamic configuration.
    -   Phantom types for enhanced type safety.
    -   Modular design with shared kernel types.
    -   Event-driven elements (Kafka) and scheduled jobs.
    -   Integration with external services (Payment, Maps, Yudhishthira for dynamic logic).
-   **Shared Kernel Memory Map:** Added `memory-bank/shared_kernel_memory_map.md` which contains context of all functions coming from shared-kernel. This file can be used as a lookup for shared-kernel functions.

## Currently Open Files
1.  Finalize the content for `activeContext.md` and `progress.md` to reflect the memory bank population task.
2.  Attempt completion of the overall task.
    - Added parsing of `Backend/app/provider-platform/dynamic-offer-driver-app/Main/spec/Storage/AlertRequest.yaml`
    - Added parsing of `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/SharedLogic/Allocator/Jobs/SendSearchRequestToDrivers/Handle/Internal/DriverPoolUnified.hs`
    - Added parsing of `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/UI/Driver.hs`
    - Added parsing of `Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/API/UI/Driver.hs`

## Active Decisions & Considerations
-   The Memory Bank population is based on the subset of files reviewed. A more comprehensive review of the entire codebase would yield even richer documentation.
-   The focus has been on capturing structural and high-level functional aspects rather than deep implementation details of every function.
-   Persistent "File not found" errors for some files listed in VS Code tabs were encountered, limiting the analysis for those specific files. Fallback to `write_to_file` was used when `replace_in_file` failed repeatedly.

---
## Current Work Focus (Ride Booking Flow Analysis - June 20, 2025)
Detailed analysis of the Beckn-based ride booking flow, tracing interactions between the `rider-app` (acting as BAP) and the `dynamic-offer-driver-app` (acting as BPP). The goal is to document the sequence of API calls, domain logic, and key data transformations involved in a successful ride booking.

## Key Learnings from Ride Booking Flow Analysis (June 20, 2025)
-   **Choreographed Beckn Interactions:** The booking process involves a strict sequence of paired Beckn calls (search/on_search, select/on_select, init/on_init, confirm/on_confirm) between the BAP and BPP.
-   **State Management:** Both BAP and BPP maintain their own state for entities like `SearchRequest`, `Estimate`, `Quote`, and `Booking`, synchronizing them through Beckn messages.
-   **Asynchronous Operations:** Many Beckn handlers fork core processing to background threads to ensure quick `AckResponse` to network partners.
-   **Idempotency:** Redis locks are used extensively to prevent duplicate processing of Beckn messages.
-   **ACL Layer:** Anti-Corruption Layers (`Beckn/ACL/`) are crucial for translating between external Beckn types and internal domain model types.
-   **Driver Allocation:** The BPP (`dynamic-offer-driver-app`) handles the complex logic of driver searching and allocation, primarily triggered after receiving a `select` message.

## Next Steps (Post Ride Booking Analysis)
-   Conclude the current analysis of the ride booking flow.
-   Updated `systemPatterns.md` to include information about shared-kernel functions.
-   Await further instructions for analyzing other system aspects (e.g., payment processing, post-booking flows like tracking/cancellation, or specific feature implementations).

---
## Current Work Focus (NammaDSL Documentation Processing - June 20, 2025)
Processing and summarizing the provided NammaDSL documentation to create a dedicated ruleset in the memory bank. This will aid in understanding and working with the YAML-based API and Storage specifications.

## Recent Changes & Learnings (NammaDSL Documentation Processing - June 20, 2025)
-   Received NammaDSL documentation detailing syntax for API and Storage specifications.
-   Key aspects understood:
    -   General workflow: YAML creation in `spec/` -> Code Generation (`run-generator`) -> Compilation.
    -   API DSL: Structure for defining modules, types (records, enums, newtypes), API endpoints (path, method, auth, request/response, params, headers), and migration notes.
    -   Storage DSL: Structure for defining modules, types (records, enums, newtypes), API endpoints (path, method, auth, request/response, params, headers), and migration notes.
    -   Common elements: `imports`, `importPackageOverrides`, `types`.
    -   Specialized features: `WithId` extensions for linked domain types, constant type suffixes for queries, various `extraOperations` for generator control.
-   Created `memory-bank/namma_dsl_rules.md` to store a summarized version of these rules.
-   Updated `memory-bank/techContext.md` to reference NammaDSL and the new rules file.

## Next Steps (Post NammaDSL Documentation)
-   Finalize memory bank updates related to NammaDSL.
-   Await further instructions.
-   Added `memory-bank/shared_kernel_memory_map.md` which contains context of all functions coming from shared-kernel. This file can be used as a lookup for shared-kernel functions.

---
## Current Work Focus (NYRegular Feature Implementation - July 2, 2025)
Implementing the "NYRegular" (recurring/scheduled rides) feature. This involves defining new data models, API endpoints, scheduler logic, and modifications to existing Beckn flows to support automated booking of pre-scheduled rides at a fixed price.

**Latest Update (Commit: b3cdd872997abf40f5e9e457bb6d02136d6fff5c)**: Added backend internal API for estimates and enhanced search functionality with reserve ride support.

## Recent Changes & Learnings (NYRegular Feature Implementation - July 2, 2025)

### Latest Implementation (Commit: b3cdd872997abf40f5e9e457bb6d02136d6fff5c)
-   **Backend Internal API for Estimates**: Added new `/internal/estimates/{estimateId}` endpoint in BPP to allow direct access to estimate details
-   **Reserve Ride Support**: Enhanced search functionality throughout the Beckn pipeline to support `isReserveRide` flag
-   **Schema Updates**: Added `ToSchema` derivations to various FarePolicy types for better API documentation
-   **Cross-Platform Integration**: Rider app can now call BPP internal APIs to retrieve estimate details for NYRegular subscriptions

### Previous Implementation
-   **Feature Goal:** Allow users to subscribe to recurring rides with fixed pricing, automated by the system.
-   **Core Principle:** Minimize changes to existing Beckn flows; leverage them via automation.
-   **Proposed Data Models (`rider-app`):**
    *   `NyRegularSubscription`: Stores subscription terms (locations, schedule, preferences, fixed price, target BPP).
    *   `NyRegularInstanceLog`: Logs each automated booking attempt for a subscription instance.
-   **Proposed API Endpoints (`rider-app`):**
    *   To initiate a fixed price quote for an NYRegular subscription (BAP sends special `/search` to BPPs).
    *   CRUD APIs for users to manage their `NyRegularSubscription`s.
    *   Endpoint to skip the next scheduled instance.
-   **Scheduler Design (`rider-app-scheduler` - Master/Child Pattern):**
    *   **Master Job (`NyRegularMasterSchedulerJob`):** Runs daily. Identifies all ride instances due for active subscriptions and enqueues individual "Child Jobs" into a persistent queue (e.g., `RegularRideInstanceJobs` table).
    *   **Child Job (`ProcessSingleNyRegularInstanceJob`):** Triggered for each specific ride instance before the scheduled pickup time.
        *   Generates a unique `instance_transaction_id`.
        *   Logs to `ny_regular_instance_log`.
        *   **Sends a tagged Beckn `/search` to the specific BPP.** This `/search` includes the `instance_transaction_id`, fixed price details, and `NYREGULAR_INSTANCE` tags.
-   **Automated Beckn Flow (Post Child Job's `/search`):**
    *   **BPP Modification:** BPP's `/search` handler recognizes NYRegular tags and fixed price, responds in `/on_search` with an offer matching the fixed price.
    *   **BAP `on_search` Handler Enhancement (`rider-app`):**
        *   Identifies the `on_search` response as belonging to an NYRegular instance.
        *   If the BPP's offer matches the fixed price, it **automatically triggers the Beckn `/select` flow** programmatically (calling internal select logic).
    *   The standard Beckn sequence (`select` -> `on_select` -> `init` -> `on_init` -> `confirm` -> `on_confirm`) then proceeds using existing BAP and BPP handlers.
    *   The Child Job (or a monitoring mechanism) updates `ny_regular_instance_log` with the final booking outcome.
-   **Key Refinement:** The BAP's `on_search` handler, rather than a separate polling mechanism in the Child Job, will drive the automated `select` action. This makes the automation more integrated with the reactive Beckn flow.
-   **Clarified Pricing Mechanism for Fixed Price:** The "fixed price" stored in an `NyRegularSubscription` is established based on an initial, comprehensive estimate from the target BPP. This BPP estimate explicitly includes any conditional charges the BPP applies for regular, scheduled services. This BPP-originated total price is then persisted by the BAP and used as the benchmark for automated booking of all subsequent instances of that subscription.

## Next Steps (NYRegular Feature)
-   ✅ **Backend Internal API for Estimates** - Completed with commit b3cdd872997abf40f5e9e457bb6d02136d6fff5c
-   Finalize the NammaDSL schemas for `NyRegularSubscription` and `NyRegularInstanceLog` based on the peer's provided context.
-   Detail the API contracts for NYRegular management.
-   Elaborate on the error handling and notification strategy for the scheduler jobs.
-   Consider edge cases (e.g., payment failure for automated bookings, user pausing a subscription close to trigger time).
-   Test the enhanced search functionality with reserve ride support.
-   Validate the internal estimate API integration for NYRegular subscriptions.

# Active Context

## Current Work Focus

### Memory Bank Initialization & Merging
**Status**: Merging two memory banks to enable consistent AI assistance.
**Goal**: Consolidate all project knowledge into a single, coherent memory bank without losing information.
**Progress**: Merging unique and common files. Key files like `productContext.md` and `activeContext.md` are undergoing a structured merge.

### Development Environment
**Current State**: Project uses Nix-based development with Cabal
**Active Setup**:
- Nix development shell with direnv
- Multi-package Cabal project (40+ packages)
- VSCode with Haskell Language Server
## Recent Changes

### Memory Bank Enhancement
- **Enhanced `projectbrief.md`** - Added specific service details, port configurations
- **Enhanced `productContext.md`** - Business context and user journeys
- **Enhanced `systemPatterns.md`** - Detailed 40+ microservices, 24+ allocation jobs
- **Enhanced `techContext.md`** - Comprehensive external integrations, webhooks
- **Status**: Completed detailed codebase analysis and memory bank enrichment
