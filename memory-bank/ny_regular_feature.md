# NYRegular Feature Tracker

## Overview

This document tracks the progress and context of the NYRegular (recurring rides) feature implementation.

## Goal

Implement a scheduler job that automatically creates child jobs for valid NYRegularSubscription entries, triggering Beckn searches at the scheduled time, and automatically selecting estimates when the `isReserve` flag is set to true.

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

## Automatic Selection Implementation

### Enhanced OnSearch Handler

The `Domain.Action.Beckn.OnSearch` module now includes automatic selection logic for reserved rides:

1. **Detection Logic**: The `isReservedRideSearch` function checks if `searchRequest.searchMode == Just RESERVE`
2. **Automatic Selection**: When a reserved ride search receives estimates, it automatically triggers the select flow
3. **Status Logging**: Updates `NyRegularInstanceLog` with `AUTO_SELECTED` status when automatic selection occurs

### Search Mode Enhancement

The `Domain.Action.UI.Search` module now properly converts `isReserveRide` from `OneWaySearchReq` to `searchMode` in `SearchRequest`:

1. **extractSearchMode Function**: Converts `isReserveRide = Just True` to `searchMode = Just SearchRequest.RESERVE`
2. **buildSearchRequest Enhancement**: Now accepts and stores the `searchMode` parameter

### NYRegular Integration

The `SharedLogic.NySubscription.triggerSubscriptionSearch` sets `isReserveRide = Just True` in the search request, which flows through to create a `SearchRequest` with `searchMode = RESERVE`.

## Automation Status Flow

1. **SEARCH_SENT**: When the scheduled job triggers a search
2. **AUTO_SELECTED**: ✅ **NEW** - When the OnSearch handler automatically selects an estimate for reserved rides
3. **BOOKING_INITIATED**: When the booking process begins
4. **CONFIRMED**: When the booking is successfully confirmed

## Initial Price Establishment for NYRegular Subscription

Before regular automated bookings commence for a `NyRegularSubscription`, or as part of its setup, the recurring "fixed price" is established:
1.  **BAP Initiates Price Discovery:** The `rider-app` (BAP) sends a special `/search` request to the target BPP associated with the (potential) subscription. This search may be flagged to indicate it's for establishing a regular service price.
2.  **BPP Calculates Comprehensive Estimate:** The BPP processes this `/search`. It calculates its standard estimate for the ride and adds any applicable conditional charges specifically for the ongoing, regular nature of the service (e.g., a "regular estimate surcharge").
3.  **BPP Responds with Full Estimate:** The BPP returns this comprehensive estimate (base fare + NYRegular conditional charges) in its `/on_search` response to the BAP.
4.  **BAP Persists Fixed Price:** The `rider-app` (BAP) receives this estimate. This total amount is then stored within the `NyRegularSubscription` record as its "fixed price". This price will be used for all subsequent automated booking instances for this subscription.

## Automated Beckn Flow (Post Child Job's `/search`)

-   **BPP Modification:** BPP's `/search` handler recognizes NYRegular tags and the provided fixed price (which originated from its own initial comprehensive estimate), responds in `/on_search` with an offer matching this fixed price.
-   **BAP `on_search` Handler Enhancement (`rider-app`):** ✅ **IMPLEMENTED**
    -   Identifies the `on_search` response as belonging to an NYRegular instance using `searchMode = RESERVE`.
    -   Automatically triggers the Beckn `/select` flow programmatically (calling internal select logic).
    -   Updates `NyRegularInstanceLog` status to `AUTO_SELECTED`.
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

-   ✅ Implement the Scheduler Job
-   ✅ Implement Child Job
-   ✅ Schedule the Main Job
-   ✅ Enhance BAP `on_search` Handler for Automatic Selection
-   ✅ **Backend Internal API for Estimates** - Added `/internal/estimates/{estimateId}` endpoint
-   ✅ **Reserve Ride Support** - Enhanced search with `isReserveRide` flag
-   ✅ **Schema Updates** - Added `ToSchema` derivations for API documentation
-   🔄 Modify BPP's `/search` handler (if needed for fixed price validation)

## Completed Tasks

-   ✅ Data Models Created
-   ✅ API Endpoints Created
-   ✅ Scheduler Job Implemented (`NyRegularMaster` and `NyRegularInstance`)
-   ✅ Main Job Scheduled
-   ✅ **Automatic Selection for Reserved Rides Implemented**
    -   ✅ Enhanced OnSearch handler to detect reserved rides
    -   ✅ Added automatic estimate selection logic
    -   ✅ Added AUTO_SELECTED status to NyRegularInstanceLog
    -   ✅ Updated search logic to properly set searchMode
-   ✅ **Backend Internal API for Estimates** (Commit: b3cdd872997abf40f5e9e457bb6d02136d6fff5c)
    -   ✅ Added `API.Internal.Estimate` module with GET `/estimates/{estimateId}` endpoint
    -   ✅ Added `Domain.Action.Internal.Estimate` module with `getEstimateDetails` function
    -   ✅ Added `BppEstimate` data type for API responses
    -   ✅ Updated `Estimate` domain type with `ToJSON, FromJSON, ToSchema` derivations
    -   ✅ Enhanced search transformer to support `isReserveRide` flag
    -   ✅ Updated search request data structures to include `isReserveRide`
    -   ✅ Added `ToSchema` derivations to FarePolicy types for API documentation
    -   ✅ Updated database schema for estimate table

## Implementation Summary

### Files Modified

1. **Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/OnSearch.hs**
   - Added detection for reserved rides (`searchMode = RESERVE`)
   - Implemented automatic selection logic
   - Added logging and status updates to `NyRegularInstanceLog`

2. **Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs**
   - Added `extractSearchMode` function to convert `isReserveRide` to `searchMode`
   - Enhanced `buildSearchRequest` to accept and store `searchMode`

3. **Backend/app/rider-platform/rider-app/Main/spec/Storage/ny_regular_instance_log.yaml**
   - Added `AUTO_SELECTED` status to the automation status enum

### Files Modified (Commit: b3cdd872997abf40f5e9e457bb6d02136d6fff5c)

4. **Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/API/Internal/Estimate.hs** (NEW)
   - Added GET `/estimates/{estimateId}` endpoint for retrieving estimate details

5. **Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Internal/Estimate.hs** (NEW)
   - Added `getEstimateDetails` function to fetch and transform estimate data
   - Added `BppEstimate` data type for API responses

6. **Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Beckn/OnDemand/Transformer/Search.hs**
   - Added `getIsReserveRide` function to extract reserve ride flag from Beckn search requests
   - Enhanced `buildSearchReq` to include `isReserveRide` in search request

7. **Backend/app/provider-platform/dynamic-offer-driver-app/Main/src/Domain/Action/Beckn/Search.hs**
   - Added `isReserveRide` field to `DSearchReq` and `ValidatedDSearchReq` data types
   - Updated validation logic to handle reserve ride requests

8. **Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs**
   - Enhanced search logic to handle `isReserveRide` flag
   - Updated Beckn fulfillment tags to include reserve ride information
   - Modified `buildSearchRequest` to accept and process `isReservedRideSearch` parameter

9. **Backend/app/rider-platform/rider-app/Main/src/SharedLogic/CallBPPInternal.hs**
   - Added `getEstimateDetails` function for calling BPP internal estimate API
   - Added `BppEstimate` data type for internal API responses

10. **Schema and Type Updates**
    - Updated `Estimate` domain type with `ToJSON, FromJSON, ToSchema` derivations
    - Added `ToSchema` derivations to various FarePolicy types for API documentation
    - Updated database schema for estimate table (congestion_multiplier type change)

### How It Works

1. **NYRegular Scheduler** creates a search with `isReserveRide = Just True`
2. **Search Logic** converts this to `searchMode = Just SearchRequest.RESERVE` 
3. **OnSearch Handler** detects reserved rides and automatically selects the first available estimate
4. **Status Logging** updates the instance log to track automatic selection
5. **Existing Flow** continues with the standard select/init/confirm sequence

### Enhanced Flow with Internal APIs (Commit: b3cdd872997abf40f5e9e457bb6d02136d6fff5c)

1. **BPP Internal Estimate API**: New `/internal/estimates/{estimateId}` endpoint allows direct access to estimate details
2. **Reserve Ride Support**: Search requests now include `isReserveRide` flag that flows through the entire Beckn pipeline
3. **Enhanced Search Transformer**: Beckn search requests are parsed to extract reserve ride information from fulfillment tags
4. **Cross-Platform Integration**: Rider app can now call BPP internal APIs to retrieve estimate details for NYRegular subscriptions

## Notes

-   Use camelCase for API endpoint paths in YAML specifications.
-   Never edit files in `src-read-only` directories directly.
-   Always compile the code after running the code generator.
-   **No price validation implemented** - automatic selection occurs regardless of price differences (as per simplified requirements).

## Next Steps

1. Run the code generator to regenerate domain types: `cd Backend && ,run-generator --apply-hint`
2. Compile and test the implementation: `cd Backend/app/rider-platform/rider-app && cabal build`
3. Test end-to-end NYRegular flow with automatic selection
