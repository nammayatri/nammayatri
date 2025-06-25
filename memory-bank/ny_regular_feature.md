# NYRegular Feature Tracker

## Overview

This document tracks the progress and context of the NYRegular (recurring rides) feature implementation.

## Goal

Implement a scheduler job that automatically creates child jobs for valid NYRegularSubscription entries, triggering Beckn searches at the scheduled time.

## Data Models

-   `NyRegularSubscription`: Stores subscription terms (locations, schedule, preferences, target BPP, and the fixed price. The fixed price is established from an initial comprehensive estimate provided by the target BPP, which includes any conditional charges specific to NYRegular recurring services).
-   `NyRegularInstanceLog`: Logs each automated booking attempt for a subscription instance.

## API Endpoints

-   To initiate a fixed price quote for an NYRegular subscription (BAP sends special `/search` to BPPs).
-   CRUD APIs for users to manage their `NyRegularSubscription`s.
-   Endpoint to skip the next scheduled instance.

## Scheduler Design (rider-app-scheduler)

-   **Master Job (`NyRegularMasterSchedulerJob`):** Runs daily. Identifies all ride instances due for active subscriptions and enqueues individual "Child Jobs" into a persistent queue (e.g., `RegularRideInstanceJobs` table).
-   **Child Job (`ProcessSingleNyRegularInstanceJob`):** Triggered for each specific ride instance before the scheduled pickup time.
    -   Generates a unique `instance_transaction_id`.
    -   Logs to `ny_regular_instance_log`.
    -   Sends a tagged Beckn `/search` to the specific BPP. This `/search` includes the `instance_transaction_id`, the pre-established fixed price details (derived from the BPP's initial estimate inclusive of conditional charges), and `NYREGULAR_INSTANCE` tags.

## Initial Price Establishment for NYRegular Subscription

Before regular automated bookings commence for a `NyRegularSubscription`, or as part of its setup, the recurring "fixed price" is established:
1.  **BAP Initiates Price Discovery:** The `rider-app` (BAP) sends a special `/search` request to the target BPP associated with the (potential) subscription. This search may be flagged to indicate it's for establishing a regular service price.
2.  **BPP Calculates Comprehensive Estimate:** The BPP processes this `/search`. It calculates its standard estimate for the ride and adds any applicable conditional charges specifically for the ongoing, regular nature of the service (e.g., a "regular estimate surcharge").
3.  **BPP Responds with Full Estimate:** The BPP returns this comprehensive estimate (base fare + NYRegular conditional charges) in its `/on_search` response to the BAP.
4.  **BAP Persists Fixed Price:** The `rider-app` (BAP) receives this estimate. This total amount is then stored within the `NyRegularSubscription` record as its "fixed price". This price will be used for all subsequent automated booking instances for this subscription.

## Automated Beckn Flow (Post Child Job's `/search`)

-   **BPP Modification:** BPP's `/search` handler recognizes NYRegular tags and the provided fixed price (which originated from its own initial comprehensive estimate), responds in `/on_search` with an offer matching this fixed price.
-   **BAP `on_search` Handler Enhancement (`rider-app`):**
    -   Identifies the `on_search` response as belonging to an NYRegular instance.
    -   If the BPP's offer matches the stored fixed price, it automatically triggers the Beckn `/select` flow programmatically (calling internal select logic).
-   The standard Beckn sequence (`select` -> `on_select` -> `init` -> `on_init` -> `confirm` -> `on_confirm`) then proceeds using existing BAP and BPP handlers.
-   The Child Job (or a monitoring mechanism) updates `ny_regular_instance_log` with the final booking outcome.

## Validation Logic

```
flowchart TD
    subgraph NyRegularSubscription Validation
        A[Start] --> B{Is recurrenceEndDate in the future?}
        B -- Yes --> C{Is today in recurrenceRuleDays?}
        C -- Yes --> D[Entry is valid]
        C -- No --> E[Entry is invalid]
        B -- No --> E
    end
```

## Current Tasks

-   Implement the Scheduler Job
-   Implement Child Job
-   Schedule the Main Job
-   Modify BPP's `/search` handler
-   Enhance BAP `on_search` Handler

## Completed Tasks

-   Data Models Created
-   API Endpoints Created
-   Scheduler Job Implemented (`NyRegularMaster` and `NyRegularInstance`)
-   Main Job Scheduled

## Notes

-   Use camelCase for API endpoint paths in YAML specifications.
-   Never edit files in `src-read-only` directories directly.
-   Always compile the code after running the code generator.
