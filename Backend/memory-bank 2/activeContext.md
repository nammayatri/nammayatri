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

## Currently Open Files
1.  Finalize the content for `activeContext.md` and `progress.md` to reflect the memory bank population task.
2.  Attempt completion of the overall task.
3.  Created `memory-bank/backend_parsing.md` to store detailed parsing of the Backend directory.
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

## Recent Changes & Learnings (Ride Booking Flow Analysis - June 20, 2025)

The following modules and their roles in the ride booking flow have been analyzed:

**1. Search Initiation (BAP - `rider-app`):**
    *   `API/UI/Search.hs`: Handles incoming UI search requests, calls domain logic.
    *   `Domain/Action/UI/Search.hs`: Core logic for creating `SearchRequest` entity, interacting with map services, and preparing for Beckn `search` calls to BPPs.
    *   Key Data: `SearchReq` (input), `SearchRes` (output), `SearchRequest` (persisted entity).

**2. Search Response Processing (BAP - `rider-app`):**
    *   `API/Beckn/OnSearch.hs`: Handles incoming Beckn `on_search` callbacks from BPPs.
    *   `Domain/Action/Beckn/OnSearch.hs`: Parses `on_search` payload, validates offers, transforms them into `Estimate` and `Quote` domain entities, and persists them.
    *   Key Data: `DOnSearchReq` (transformed BPP response), `EstimateInfo`, `QuoteInfo`, persisted `Estimate` and `Quote` entities.

**3. Quote Retrieval for Rider (BAP - `rider-app`):**
    *   `API/UI/Quote.hs`: Endpoint for rider app to fetch available quotes/estimates for a `searchId`.
    *   `Domain/Action/UI/Quote.hs`: Retrieves stored `Estimate` and `Quote` entities, filters/sorts them, and prepares the `GetQuotesRes` response.
    *   Key Data: `GetQuotesRes`, `OfferRes` (sum type for different offer types like `OnDemandCab`, `MetroOffer`).

**4. Rider Selects Offer (BAP - `rider-app`):**
    *   `API/UI/Select.hs`: Handles UI request when rider selects an offer (identified by `estimateId`).
    *   `Domain/Action/UI/Select.hs`: Updates internal state (`SearchRequest`, `PersonFlowStatus`), prepares data for Beckn `select` call to BPP.
    *   Key Data: `DSelectReq` (input), `DSelectRes` (internal result for BPP call).

**5. BPP Processes Select (BPP - `dynamic-offer-driver-app`):**
    *   `API/Beckn/Select.hs`: Receives Beckn `select` from BAP.
    *   `Domain/Action/Beckn/Select.hs`: Processes selection, updates its `SearchRequest`, and **initiates driver allocation** (`initiateDriverSearchBatch`). Prepares to send `on_select` back to BAP.

**6. BAP Processes OnSelect (BAP - `rider-app`):**
    *   `API/Beckn/OnSelect.hs`: Receives Beckn `on_select` from BPP (contains driver-specific quotes).
    *   `Domain/Action/Beckn/OnSelect.hs`: Stores new driver-specific `Quote` entities. If auto-confirm is enabled, triggers Beckn `init`; otherwise, notifies rider app.
    *   Key Data: `DOnSelectReq` (BPP's response), new `Quote` entities.

**7. Rider Confirms Driver Quote (BAP - `rider-app`):**
    *   `API/UI/Confirm.hs`: Handles UI request when rider confirms a specific driver quote.
    *   `Domain/Action/UI/Confirm.hs`: Creates local `Booking` entity, prepares for Beckn `init` call.
    *   Key Data: `ConfirmRes` (output with `bookingId`).

**8. BPP Processes Init (BPP - `dynamic-offer-driver-app`):**
    *   `API/Beckn/Init.hs`: Receives Beckn `init` from BAP.
    *   `Domain/Action/Beckn/Init.hs`: Creates BPP-side `Booking` entity, prepares data for `on_init` response.
    *   Key Data: `InitReq` (input), `InitRes` (output for `on_init`).

**9. BAP Processes OnInit (BAP - `rider-app`):**
    *   `API/Beckn/OnInit.hs`: Receives Beckn `on_init` from BPP.
    *   `Domain/Action/Beckn/OnInit.hs`: Updates local `Booking` with confirmed details from BPP, prepares for Beckn `confirm` call.
    *   Key Data: `OnInitReq` (input), `OnInitRes` (output for `confirm` call).

**10. BPP Processes Confirm (BPP - `dynamic-offer-driver-app`):**
    *   `API/Beckn/Confirm.hs`: Receives Beckn `confirm` from BAP.
    *   `Domain/Action/Beckn/Confirm.hs`: Finalizes BPP-side `Booking`, creates `Ride` entity if driver assigned, prepares for `on_confirm` response.
    *   Key Data: `DConfirmReq` (input), `DConfirmRes` (output for `on_confirm`).

**11. BAP Processes OnConfirm (BAP - `rider-app`):**
    *   `API/Beckn/OnConfirm.hs`: Receives Beckn `on_confirm` from BPP.
    *   `Domain/Action/Beckn/OnConfirm.hs`: Updates local `Booking` to `TRIP_ASSIGNED` or `CONFIRMED`. If driver details provided, creates local `Ride` entity. Notifies rider app. Ride is booked.
    *   Key Data: `OnConfirmReq` (input), final `Booking` and `Ride` states.

## Key Learnings from Ride Booking Flow Analysis (June 20, 2025)
-   **Choreographed Beckn Interactions:** The booking process involves a strict sequence of paired Beckn calls (search/on_search, select/on_select, init/on_init, confirm/on_confirm) between the BAP and BPP.
-   **State Management:** Both BAP and BPP maintain their own state for entities like `SearchRequest`, `Estimate`, `Quote`, and `Booking`, synchronizing them through Beckn messages.
-   **Asynchronous Operations:** Many Beckn handlers fork core processing to background threads to ensure quick `AckResponse` to network partners.
-   **Idempotency:** Redis locks are used extensively to prevent duplicate processing of Beckn messages.
-   **ACL Layer:** Anti-Corruption Layers (`Beckn/ACL/`) are crucial for translating between external Beckn types and internal domain model types.
-   **Driver Allocation:** The BPP (`dynamic-offer-driver-app`) handles the complex logic of driver searching and allocation, primarily triggered after receiving a `select` message.

## Next Steps (Post Ride Booking Analysis)
-   Conclude the current analysis of the ride booking flow.
-   Await further instructions for analyzing other system aspects (e.g., payment processing, post-booking flows like tracking/cancellation, or specific feature implementations).

---
## Current Work Focus (NammaDSL Documentation Processing - June 20, 2025)
Processing and summarizing the provided NammaDSL documentation to create a dedicated ruleset in the memory bank. This will aid in understanding and working with the YAML-based API and Storage specifications.

## Recent Changes & Learnings (NammaDSL Documentation Processing - June 20, 2025)
-   Received NammaDSL documentation detailing syntax for API and Storage specifications.
-   Key aspects understood:
    -   General workflow: YAML creation in `spec/` -> Code Generation (`run-generator`) -> Compilation.
    -   API DSL: Structure for defining modules, types (records, enums, newtypes), API endpoints (path, method, auth, request/response, params, headers), and migration notes.
    -   Storage DSL: Structure for defining data types (tables), fields, constraints, Beam types/fields, SQL types, default values, queries (Beam and cached), and data transformers (`fromTType`/`toTType`).
    -   Common elements: `imports`, `importPackageOverrides`, `types`.
    -   Specialized features: `WithId` extensions for linked domain types, constant type suffixes for queries, various `extraOperations` for generator control.
-   Created `memory-bank/namma_dsl_rules.md` to store a summarized version of these rules.
-   Updated `memory-bank/techContext.md` to reference NammaDSL and the new rules file.

## Next Steps (Post NammaDSL Documentation)
-   Finalize memory bank updates related to NammaDSL.
-   Await further instructions.

---
## Current Work Focus (NYRegular Feature Planning - June 20, 2025)
Planning the implementation of the "NYRegular" (recurring/scheduled rides) feature. This involves defining new data models, API endpoints, scheduler logic, and modifications to existing Beckn flows to support automated booking of pre-scheduled rides at a fixed price.

## Recent Changes & Learnings (NYRegular Feature Planning - June 20, 2025)
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

## Next Steps (NYRegular Feature)
-   Finalize the NammaDSL schemas for `NyRegularSubscription` and `NyRegularInstanceLog` based on the peer's provided context.
-   Detail the API contracts for NYRegular management.
-   Elaborate on the error handling and notification strategy for the scheduler jobs.
-   Consider edge cases (e.g., payment failure for automated bookings, user pausing a subscription close to trigger time).
