# Backend Parsing: Ride Booking Flow Analysis (June 20, 2025)

This document details the modules and logic involved in the Beckn-based ride booking flow, primarily focusing on the interactions between the `rider-app` (BAP) and the `dynamic-offer-driver-app` (BPP).

## 1. Search Initiation (Rider App - BAP)

*   **UI Request:** Rider initiates a search via the mobile app.
*   **API Layer (`rider-app/.../API/UI/Search.hs`):**
    *   Receives the search request (type `SearchReq`).
    *   Handles authentication, rate limiting, duplicate search checks.
    *   Calls the domain action `DSearch.search`.
*   **Domain Layer (`rider-app/.../Domain/Action/UI/Search.hs` - `search` function):**
    *   Validates input (`SearchReq` containing `OneWaySearchReq`, `RentalSearchReq`, etc.).
    *   Fetches `Person` (rider), `Merchant`, `MerchantOperatingCity`, `RiderConfig`.
    *   Uses map services (`Kernel.External.Maps`) to calculate route details (distance, duration, polyline).
    *   Constructs a `SearchRequest` domain entity (defined in `Domain.Types.SearchRequest.hs`, likely in `dynamic-offer-driver-app` or shared lib) with all details.
    *   Persists the `SearchRequest` entity.
    *   Logs events and updates hotspot data.
    *   Returns `SearchRes` (containing `searchId`, `searchRequestExpiry`, route info) to the API layer.
*   **Asynchronous BPP Calls (from API Layer):**
    *   After `DSearch.search` returns, the API handler forks asynchronous calls to `SharedLogic.CallBPP.searchV2` to query relevant BPPs.

## 2. BPP Processes Search & Responds with Offers (BPP - `dynamic-offer-driver-app`)

*   **API Layer (`dynamic-offer-driver-app/.../API/Beckn/Search.hs`):**
    *   Receives Beckn `search` request from BAP.
    *   Validates signature, transforms request via ACL (`Beckn.ACL.Search.buildSearchReqV2`).
    *   Calls domain action `Dsearch.handler` (BPP's `Domain.Action.Beckn.Search`).
    *   Sends `AckResponse` to BAP.
*   **Domain Layer (`dynamic-offer-driver-app/.../Domain/Action/Beckn/Search.hs` - `handler` function):**
    *   Processes the search request based on its internal logic (driver availability, fare policies, surge pricing).
    *   Generates `EstimateInfo` and `QuoteInfo` for available services/drivers.
    *   Constructs a Beckn `on_search` message payload.
    *   Uses `SharedLogic.CallBAP.onSearchV2` (or similar) to send the `on_search` response back to the BAP's callback URL.

## 3. BAP Processes `on_search` Responses (BAP - `rider-app`)

*   **API Layer (`rider-app/.../API/Beckn/OnSearch.hs`):**
    *   Receives Beckn `on_search` callback from BPP.
    *   Validates signature, transforms request via ACL (`Beckn.ACL.OnSearch.buildOnSearchReqV2`).
    *   Calls domain action `DOnSearch.onSearch`.
    *   Sends `AckResponse` to BPP.
*   **Domain Layer (`rider-app/.../Domain/Action/Beckn/OnSearch.hs` - `onSearch` function):**
    *   Takes `ValidatedOnSearchReq` (containing `providerInfo`, `estimatesInfo`, `quotesInfo`).
    *   Filters estimates/quotes based on rider preference and blacklisted vehicles.
    *   Transforms `EstimateInfo` into `DEstimate.Estimate` entities using `buildEstimate`.
    *   Transforms `QuoteInfo` into `DQuote.Quote` entities using `buildQuote`.
    *   Persists these `Estimate` and `Quote` entities, linking them to the original `SearchRequest`.
    *   Handles auto-select/auto-confirm for meter rides or multimodal scenarios if applicable.

## 4. Rider Fetches Quotes (Rider App -> BAP)

*   **API Layer (`rider-app/.../API/UI/Quote.hs`):**
    *   Endpoint `/rideSearch/{searchId}/results`.
    *   Calls domain action `DQuote.getQuotes`.
*   **Domain Layer (`rider-app/.../Domain/Action/UI/Quote.hs` - `getQuotes` function):**
    *   Retrieves `SearchRequest`, `Estimate`s, and `Quote`s for the `searchId`.
    *   Handles active booking conflicts.
    *   Transforms `Estimate`s to `UEstimate.EstimateAPIEntity` and `Quote`s to `UQuote.QuoteAPIEntity` (wrapped in `OfferRes`).
    *   Sorts and returns them in `GetQuotesRes`.

## 5. Rider Selects an Offer (Rider App -> BAP)

*   **API Layer (`rider-app/.../API/UI/Select.hs`):**
    *   Endpoint `/estimate/{estimateId}/select` or `/selectV2`.
    *   Takes `estimateId` and `DSelect.DSelectReq` (customer fee, payment method, etc.).
    *   Calls domain action `DSelect.select2`.
*   **Domain Layer (`rider-app/.../Domain/Action/UI/Select.hs` - `select2` function):**
    *   Validates request and estimate expiry.
    *   Updates `SearchRequest` (e.g., with payment method, delivery details).
    *   Updates `PersonFlowStatus` to `WAITING_FOR_DRIVER_OFFERS`.
    *   Updates selected `Estimate` status to `DRIVER_QUOTE_REQUESTED`.
    *   Prepares `DSelectRes` for the Beckn `select` call to BPP.
    *   **Triggers Beckn `select` call to the BPP** (via `SharedLogic.CallBPP.selectV2` or similar).

## 6. BPP Processes `select` Request (BPP - `dynamic-offer-driver-app`)

*   **API Layer (`dynamic-offer-driver-app/.../API/Beckn/Select.hs`):**
    *   Receives Beckn `select` from BAP.
    *   Validates signature, transforms via ACL (`Beckn.ACL.Select.buildSelectReqV2`).
    *   Calls domain action `DSelect.select` (BPP's version).
    *   Sends `AckResponse`.
*   **Domain Layer (`dynamic-offer-driver-app/.../Domain/Action/Beckn/Select.hs` - `select`/`handler` function):**
    *   Validates request, fetches `SearchRequest` and selected `Estimate`(s).
    *   Updates its local `SearchRequest`.
    *   Constructs `TripQuoteDetail`(s).
    *   **Calls `initiateDriverSearchBatch`**: This is the core of driver allocation. It finds suitable drivers and sends them requests.
    *   Does *not* immediately send `on_select`. This happens after driver allocation outcome.

## 7. BPP Sends `on_select` (BPP -> BAP)

*   **(Logic likely within driver allocation/`SharedLogic.Allocator` or a dedicated `OnSelect` builder in BPP):**
    *   After driver allocation (driver accepts or timeout), BPP constructs an `on_select` message.
    *   This message contains driver-specific quotes (`QuoteInfo` with `DriverOfferQuoteDetails`).
    *   Sends `on_select` to BAP's callback URL (via `SharedLogic.CallBAP.onSelectV2` or similar).

## 8. BAP Processes `on_select` (BAP - `rider-app`)

*   **API Layer (`rider-app/.../API/Beckn/OnSelect.hs`):**
    *   Receives Beckn `on_select` from BPP.
    *   Validates signature, transforms via ACL (`Beckn.ACL.OnSelect.buildOnSelectReqV2`).
    *   Calls domain action `DOnSelect.onSelect`.
    *   Sends `AckResponse`.
*   **Domain Layer (`rider-app/.../Domain/Action/Beckn/OnSelect.hs` - `onSelect` function):**
    *   Takes `ValidatedOnSelectReq`.
    *   Transforms incoming `QuoteInfo` (driver offers) into new `DQuote.Quote` entities (using `buildSelectedQuote` which creates `DDriverOffer.DriverOffer`).
    *   Persists these new `Quote`s.
    *   Updates original `Estimate` status to `GOT_DRIVER_QUOTE`.
    *   If `autoAssignEnabledV2` is true and a suitable quote exists:
        *   Attempts to auto-confirm: calls `SConfirm.confirm` (internal confirm logic), then builds and **sends Beckn `init` to BPP**.
    *   Else (no auto-assign): Notifies rider app (`Notify.notifyOnDriverOfferIncoming`) that driver offers are available.

## 9. Rider Confirms Driver Quote / Auto-Confirm (Rider App -> BAP)

*   **API Layer (`rider-app/.../API/UI/Confirm.hs`):**
    *   Endpoint `/rideSearch/quotes/{quoteId}/confirm`.
    *   Takes `quoteId` (the new driver-specific quote).
    *   Calls domain action `DConfirm.confirm'`.
*   **Domain Layer (`rider-app/.../Domain/Action/UI/Confirm.hs` - `confirm'` function):**
    *   Calls `DConfirm.confirm` which validates the quote and **creates a local `DRB.Booking` entity**.
    *   Prepares `DConfirmRes`.
    *   The API layer then uses `ACL.buildInitReqV2` and **sends a Beckn `init` message to the BPP** (`CallBPP.initV2`).
    *   Returns `ConfirmRes` (with `bookingId`) to the app.

## 10. BPP Processes `init` (BPP - `dynamic-offer-driver-app`)

*   **API Layer (`dynamic-offer-driver-app/.../API/Beckn/Init.hs`):**
    *   Receives Beckn `init` from BAP.
    *   Validates signature, transforms via ACL (`Beckn.ACL.Init.buildInitReqV2`).
    *   Calls domain action `DInit.handler`.
    *   Sends `AckResponse`.
*   **Domain Layer (`dynamic-offer-driver-app/.../Domain/Action/Beckn/Init.hs` - `handler` function):**
    *   Validates request against its `Quote` or `DriverQuote`.
    *   **Creates its own `DRB.Booking` entity**, linking it to the BAP's transaction.
    *   Handles delivery details if applicable.
    *   Prepares `InitRes` containing BPP's booking details, payment info, etc.
    *   The API layer then uses this to **send Beckn `on_init` back to BAP**.

## 11. BAP Processes `on_init` (BAP - `rider-app`)

*   **API Layer (`rider-app/.../API/Beckn/OnInit.hs`):**
    *   Receives Beckn `on_init` from BPP.
    *   Validates signature, transforms via ACL (`Beckn.ACL.OnInit.buildOnInitReqV2`).
    *   Calls domain action `DOnInit.onInit`.
    *   Sends `AckResponse`.
*   **Domain Layer (`rider-app/.../Domain/Action/Beckn/OnInit.hs` - `onInit` function):**
    *   Updates its local `Booking` with confirmed details from BPP (e.g., `bppBookingId`, final fares, payment URL).
    *   Stores fare breakup.
    *   Prepares `OnInitRes`.
    *   The API layer then uses this to **send Beckn `confirm` to BPP**.

## 12. BPP Processes `confirm` (BPP - `dynamic-offer-driver-app`)

*   **API Layer (`dynamic-offer-driver-app/.../API/Beckn/Confirm.hs`):**
    *   Receives Beckn `confirm` from BAP.
    *   Validates signature, transforms via ACL (`Beckn.ACL.Confirm.buildConfirmReqV2`).
    *   Calls domain action `DConfirm.handler`.
    *   Sends `AckResponse`.
*   **Domain Layer (`dynamic-offer-driver-app/.../Domain/Action/Beckn/Confirm.hs` - `handler` function):**
    *   Validates the BPP's `Booking` status.
    *   If confirming a dynamic/driver offer: **Creates the `DRide.Ride` entity**, linking booking to driver/vehicle.
    *   If confirming a static offer: May re-initiate driver search if needed.
    *   Prepares `DConfirmRes` with final ride details (if available).
    *   The API layer then uses this to **send Beckn `on_confirm` back to BAP**.

## 13. BAP Processes `on_confirm` (BAP - `rider-app`) - Ride Booked!

*   **API Layer (`rider-app/.../API/Beckn/OnConfirm.hs`):**
    *   Receives Beckn `on_confirm` from BPP.
    *   Validates signature, transforms via ACL (`Beckn.ACL.OnConfirm.buildOnConfirmReqV2`).
    *   Calls domain action `DOnConfirm.onConfirm`.
    *   Sends `AckResponse`.
*   **Domain Layer (`rider-app/.../Domain/Action/Beckn/OnConfirm.hs` - `onConfirm` function):**
    *   Updates its local `Booking` status to `TRIP_ASSIGNED` or `CONFIRMED`.
    *   If driver and ride details are in the `on_confirm` message (typically `RideAssignedInfo`):
        *   **Creates a local `DRide.Ride` entity**, storing driver name, vehicle info, OTP, tracking URL, etc.
    *   Notifies the rider app that the booking is confirmed and ride is assigned.
