# Progress Log: Namma Yatri

## Session: Enhanced `NyRegularSubscription` Job Handling (Date: 2024-05-24)

### Completed Tasks:

1.  **Implemented Hash-Based Job Validation:**
    *   **Goal:** Prevent scheduled ride jobs (`NyRegularInstanceJob`) from running if the parent subscription's scheduling rules have changed since the job was created.
    *   **Implementation:**
        *   Created a `SharedLogic.NyRegularSubscriptionHasher` module to compute a canonical `DbHash` of a subscription's scheduling-critical state.
        *   Added a `schedulingHash` field to the `NyRegularSubscription` domain type and database table. This hash is now calculated and stored on every creation and update.
        *   Added an `expectedSchedulingHash` field to the `NyRegularInstanceJobData` record.
        *   The master scheduler job now populates this `expectedSchedulingHash` when creating jobs.
        *   The instance job now validates at runtime by comparing its stored hash against the live hash of the subscription, terminating itself if they do not match.

2.  **Implemented Proactive & Idempotent Job Creation:**
    *   **Goal:** Make the system more responsive by scheduling the next applicable ride immediately after a relevant subscription change.
    *   **Implementation:**
        *   The `postNyRegularSubscriptionsUpdate` API endpoint now detects if a significant scheduling change has occurred by comparing the old and new `schedulingHash`.
        *   If a change is detected, it calculates the *next single applicable* ride instance time.
        *   This creation is made idempotent using a Redis lock (`setNxExpire`) with a key composed of the subscription ID, the new scheduling hash, and the instance timestamp.

3.  **Implemented Timezone-Aware Scheduling:**
    *   **Goal:** Ensure scheduled ride times are interpreted in the merchant's local timezone.
    *   **Implementation:**
        *   The proactive job creation logic now fetches the `RiderConfig` to get the `timeDiffFromUtc`.
        *   The time calculation helper now uses this offset to correctly convert the local `scheduledTimeOfDay` to a precise UTC timestamp.

### Overall Outcome:
The `NyRegularSubscription` feature is now significantly more robust, responsive, and reliable. The system correctly handles updates to subscriptions, prevents outdated jobs from running, and ensures accurate, timezone-aware scheduling.

---
<br>

# Progress: Namma Yatri Memory Bank Population

## What Works (Completed Steps)
1.  **Initial Setup:** Memory Bank file structure created.
2.  Created `memory-bank/ny_regular_feature.md` to track NYRegular feature implementation.
3.  **`productContext.md` Population:**
    -   Initial population based on high-level project understanding.
4.  **`systemPatterns.md` Population:**
    -   Initial population based on READMEs and file structure.
5.  **`techContext.md` Population:**
    -   Initial population based on READMEs and file structure.
6.  **Analysis of Haskell Domain Types:**
    -   Reviewed several key domain type files to understand data structures, enums, and relationships.
7.  **Analysis of Shared Logic Modules:**
    -   Reviewed key shared logic files to understand core business processes.
8.  **Analysis of Storage Query Modules:**
    -   Reviewed several query modules to understand database interaction patterns and data transformation logic.
9.  **`activeContext.md` Population:** Updated to reflect the current task.
10. **Directory Structure Enrichment:** The provided comprehensive directory structure has been incorporated into the memory bank's understanding.
11. **Analyzed Core Ride Booking Flow (Beckn Protocol):**
    *   Traced the sequence of Beckn API calls and corresponding domain logic.
12. **Processed NammaDSL Documentation:**
    *   Received and analyzed the NammaDSL documentation.
    *   Summarized syntax for API DSL and Storage DSL.
    *   Created `memory-bank/namma_dsl_rules.md` to store these rules.
    *   Updated `techContext.md` to reference NammaDSL.
13. **Added Shared Kernel Memory Map:**
    * Added `memory-bank/shared_kernel_memory_map.md` which contains context of all functions coming from shared-kernel.
    * Updated `activeContext.md` to reflect the addition of the new file.
14. **Memory Bank Updated:**
    * Updated `activeContext.md` and `progress.md` to reflect the creation of `memory-bank/ny_regular_feature.md`.

## Current Status
-   Core Memory Bank files have been updated with a foundational understanding of the Namma Yatri project.
-   A detailed understanding of the Beckn-based ride booking choreography between the `rider-app` (BAP) and `dynamic-offer-driver-app` (BPP) has been achieved and documented in `systemPatterns.md` and `activeContext.md`.
-   The system's reliance on ACLs, asynchronous processing, Redis for idempotency, and distinct domain logic for BAP and BPP roles in the Beckn flow is now clearer.
-   NammaDSL documentation has been processed and summarized in `memory-bank/namma_dsl_rules.md`, enhancing understanding of the YAML-to-Haskell code generation pipeline.
-   A detailed plan for the "NYRegular" (recurring rides) feature has been formulated, incorporating peer context and refining the automation strategy. This plan is documented across relevant memory bank files.
- Created `memory-bank/ny_regular_feature.md` to track NYRegular feature implementation.

## In Progress
-   Implementing the scheduler job for `NyRegularSubscription`.

## Next Up
-   ✅ **Backend Internal API for Estimates** (Commit: b3cdd872997abf40f5e9e457bb6d02136d6fff5c)
    - Added `/internal/estimates/{estimateId}` endpoint in BPP
    - Enhanced search functionality with `isReserveRide` support
    - Updated schema and type definitions for API documentation
-   Modifying the BPP to handle the tagged `/search` requests from the new scheduler.
-   Enhancing the BAP's `on_search` handler to automatically trigger the `/select` flow.

### Completed
-   **`NyRegularSubscription` Scheduler:**
    -   Implemented the `NyRegularInstance` job to handle triggering an automated search for a `NyRegularSubscription`.
    -   Successfully refactored the search-triggering logic in `SharedLogic/NySubscription.hs` to reuse the application's core `DSearch.search` function.
    -   Enriched the scheduler's `HandlerEnv` to satisfy the `SearchRequestFlow` constraints required by the core search logic, resolving a critical environment mismatch.
-   **`NyRegularSubscription` Scheduler:**
    -   Implemented the `NyRegularMaster` job to run daily and create child jobs for valid subscriptions.
    -   Implemented the `NyRegularInstance` job to initiate the Beckn `/search` for each ride.
-   Defined the data models (`NyRegularSubscription`, `NyRegularInstanceLog`).
-   Created the initial API endpoints for managing subscriptions.
-   **Backend Internal API for Estimates** (Commit: b3cdd872997abf40f5e9e457bb6d02136d6fff5c):
    -   Added new internal API endpoint `/internal/estimates/{estimateId}` in BPP for retrieving estimate details
    -   Enhanced search functionality with `isReserveRide` flag support throughout the Beckn pipeline
    -   Updated domain types and schemas for better API documentation
    -   Added cross-platform integration capabilities for NYRegular feature
