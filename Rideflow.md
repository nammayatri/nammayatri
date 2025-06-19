# NammaYatri Backend Ride Flow Overview

This document provides an in-depth explanation of the ride booking and dynamic pricing flow within the NammaYatri backend, focusing on the interactions between the Rider Application (BAP) and the Driver Application (BPP) via the Beckn Protocol.

## 1. Core Technologies and Architecture

The NammaYatri backend is built primarily with **Haskell**, ensuring a strongly-typed and robust system. Key components include:

*   **Haskell**: The primary language for backend services.
*   **PostgreSQL**: Used for persistent data storage, managed via the Beam ORM.
*   **Redis**: Employed for caching, transient data (like OTPs, rate limiting, distributed locks), and potentially for real-time geospatial queries for nearby drivers.
*   **Apache Kafka**: The backbone for asynchronous event streaming between services.
*   **Nix Flakes**: Manages reproducible builds and dependency management across the project.
*   **Namma DSL (`alchemist`)**: A YAML-based Domain Specific Language used to define API endpoints, data models (request/response), and database schemas. `alchemist` generates boilerplate Haskell code (Servant APIs, Aeson instances, Beam tables), ensuring consistency and accelerating development.
*   **Beckn Protocol**: The open-source protocol facilitating seamless communication and transactions between different network participants (e.g., Buyer Applications/BAP and Buyer Participant Platforms/BPP).
*   **`yudhishthira`**: A central rule engine library used for evaluating complex business rules and policies, crucial for dynamic pricing and offer generation.
*   **`external` library**: Manages integrations with various third-party services (e.g., SMS gateways, Firebase Cloud Messaging for push notifications, mapping services, payment gateways, etc.).
*   **`location-updates` library**: Handles the processing of real-time driver location data.
*   **`special-zone-a` library**: Utilized for managing geographic zone configurations, impacting pricing and service availability.

## 2. Backend Ride Flow (Rider App <-> Driver App via Beckn)

The ride booking process in NammaYatri is orchestrated through a series of interactions following the Beckn Protocol. The `rider-app` acts as the **Buyer Application (BAP)**, and the `dynamic-offer-driver-app` acts as the **Buyer Participant Platform (BPP)**.

Here's a step-by-step breakdown of both the manual acceptance and auto-assignment flows:

### 2.1. Rider Initiates Search (`rider-app` - BAP)

1.  **User Action**: A rider opens the `rider-app`, inputs their origin and destination, and optionally sets preferences (e.g., time, vehicle type). This triggers a ride search request from the rider's device.
2.  **API Call**: The `rider-app` sends an authenticated `POST /v2/rideSearch` (for standard taxi rides) or `POST /v2/multimodalSearch` (for public transport options) request to its backend service (`rider-app`).
3.  **BAP Processing (`API.UI.Search.hs`)**:
    *   The `API.UI.Search` handler in the `rider-app` backend processes the incoming request.
    *   It uses `TaxiACL.buildSearchReqV2` (from the `beckn-spec` library) to construct a Beckn `search` request payload, adhering to the Beckn protocol specifications.
    *   The BAP persists the `SearchRequest` entity in its database to track the search context.
    *   **Inter-service Communication**: The `rider-app` then asynchronously sends this Beckn `search` request to various relevant BPPs (including the `dynamic-offer-driver-app`) via the `CallBPP.searchV2` function (from `beckn-services`).

### 2.2. Driver App Responds with Dynamic Quotes (`dynamic-offer-driver-app` - BPP)

1.  **BPP Receives Search (`API.Beckn.Search.hs`)**:
    *   The `dynamic-offer-driver-app` (BPP) receives the Beckn `search` request at its `POST /v2/beckn/{merchantId}/search` endpoint.
    *   **Authentication**: The request is authenticated using Beckn `SignatureAuth`, verifying the digital signature of the incoming message.
2.  **Dynamic Offer Generation (`Domain.Action.Beckn.Search.handler`)**:
    *   The `Domain.Action.Beckn.Search.handler` is the core logic that processes the search request.
    *   It utilizes internal services and data (e.g., driver availability, location, traffic conditions) to identify potential drivers.
    *   Crucially, it integrates with the **`yudhishthira` rule engine** and `special-zone-a` configurations to apply dynamic pricing rules and generate multiple, varied quotes/offers based on real-time factors like demand, supply, time of day, and special zone policies.
    *   **Idempotency**: Redis locks (`searchLockKey`, `searchProcessingLockKey`) are used to ensure idempotency and prevent duplicate processing of `search` requests.
3.  **BPP Sends `on_search` Callback**:
    *   The BPP constructs a Beckn `on_search` callback message (containing all the generated dynamic `quotes/estimate`) using `Beckn.ACL.OnSearch.mkOnSearchRequest`.
    *   This `on_search` response is then asynchronously sent back to the `rider-app`'s (BAP's) designated callback URI using `Callback.withCallback`.

### 2.3. Rider App Receives and Displays Quotes (`rider-app` - BAP)

1.  **BAP Receives `on_search` (`API.Beckn.OnSearch.hs`)**:
    *   The `rider-app` (BAP) receives the asynchronous Beckn `on_search` callback at its `API.Beckn.OnSearch` handler.
    *   **Validation**: It validates the incoming context against the original `SearchRequest` to ensure message integrity and correlation.
2.  **Quote Persistence & Retrieval (`Domain.Action.Beckn.OnSearch.onSearch`, `API.UI.Quote.hs`)**:
    *   `Domain.Action.Beckn.OnSearch.onSearch` asynchronously persists the validated offers as `Estimate` entities in the `rider-app`'s database.
    *   The `rider-app` UI periodically or upon receiving `on_search` fetches these aggregated ride quotes from its backend via `GET /v2/rideSearch/{searchId}/results`.
    *   **User Selection**: The `rider-app` UI then presents these multiple dynamic quotes to the rider, allowing them to compare and select their preferred option (e.g., based on price, ETA, vehicle type). This is the "user selects the quotes given by driver" part of the dynamic flow.

### 2.4. User Selects a Quote (`rider-app` - BAP)

1.  **User Action**: The rider taps on one of the displayed quotes in the `rider-app` UI.
2.  **API Call**: The `rider-app` sends an authenticated request to its `API.UI.Select` endpoint, initiating the Beckn `select` phase to the chosen BPP.

### 2.5. Driver App Processes Selection (`dynamic-offer-driver-app` - BPP)

1.  **BPP Receives `select` (`API.Beckn.Select.hs`)**:
    *   The `dynamic-offer-driver-app` (BPP) receives the Beckn `select` request at its `POST /v2/beckn/{merchantId}/select` endpoint.
2.  **Validation & Resource Locking (`Domain.Action.Beckn.Select.validateRequest`, `Domain.Action.Beckn.Select.handler`)**:
    *   `Domain.Action.Beckn.Select.validateRequest` validates the selected offer against the previously sent `SearchRequest` and `Estimate` entities to confirm its validity and availability.
    *   `Domain.Action.Beckn.Select.handler` confirms the availability of the selected ride/driver and may lock necessary resources to prevent double-booking.
3.  **BPP Sends `on_select` Callback**: After pooling and responding with a quote from the driver, the BPP constructs and sends an `on_select` callback message back to the `rider-app` (BAP), confirming the selection and providing finalized details.

### 2.6. Rider App Confirms Selection (Pre-Booking - `rider-app` - BAP)

1.  **BAP Receives `on_select` (`API.Beckn.OnSelect.hs`)**:
    *   The `rider-app` (BAP) receives the `on_select` callback at its `API.Beckn.OnSelect` handler.
    *   `Domain.Action.Beckn.OnSelect.onSelect` updates the `Estimate` entity with the confirmed details from the BPP.
    *   We create `driver offer` data, and here we either show the driver accepted quotes by `estimate/<estimateId>/results` api if the user has opted for choosing between multiple drivers, or else, in case of auto-assign, we take the first offer, create a `Booking Entry`, and then call `init`.
3.  **API Call**: Upon user confirmation in case of `choose between drivers`, the `rider-app` calls its `API.UI.Confirm` endpoint, initiating the Beckn `init` phase. This request is sent to the BPP. (`Booking already created`)

### 2.7. Driver App Initializes Booking (`dynamic-offer-driver-app` - BPP)

1.  **BPP Receives `init` (`API.Beckn.Init.hs`)**:
    *   The `dynamic-offer-driver-app` (BPP) receives the Beckn `init` request at `POST /v2/beckn/{merchantId}/init`.
2.  **Preliminary Booking & Terms (`Domain.Action.Beckn.Init.validateRequest`, `Domain.Action.Beckn.Init.handler`)**:
    *   `Domain.Action.Beckn.Init.validateRequest` validates the request based on the merchant and confirmed estimate.
    *   `Domain.Action.Beckn.Init.handler` creates a preliminary `Booking Table` record on the BPP's side, finalizes any terms and conditions, and prepares the `on_init` callback.
3.  **BPP Sends `on_init` Callback**:
    *   The BPP sends an `on_init` callback message back to the `rider-app` (BAP), signaling the successful initialization of the booking on the provider's side.

### 2.8. Rider App Finalizes Booking and Payment (`rider-app` - BAP)

1.  **BAP Receives `on_init` (`API.Beckn.OnInit.hs`)**:
    *   The `rider-app` (BAP) receives the `on_init` callback at its `API.Beckn.OnInit` handler.
    *   `Domain.Action.Beckn.OnInit.onInit` applies the updates to the relevant `Booking` entity.
2.  **Payment Processing**: The `rider-app` UI handles the final payment confirmation. This involves interacting with the platform's payment gateway via `API.UI.Payment` and `API.Action.UI.RidePayment`.
3.  **API Call (Final Confirmation)**: Upon successful payment or final confirmation by the user, the `rider-app` calls its `API.UI.Booking` endpoint, which sends the final Beckn `confirm` request to the BPP.

### 2.9. Driver App Confirms Booking (`dynamic-offer-driver-app` - BPP)

1.  **BPP Receives `confirm` (`API.Beckn.Confirm.hs`)**:
    *   The `dynamic-offer-driver-app` (BPP) receives the Beckn `confirm` request at `POST /v2/beckn/{merchantId}/confirm`.
2.  **Finalize Booking (`Domain.Action.Beckn.Confirm.validateRequest`, `Domain.Action.Beckn.Confirm.handler`)**:
    *   `Domain.Action.Beckn.Confirm.validateRequest` performs final validations.
    *   `Domain.Action.Beckn.Confirm.handler` finalizes the `Booking` on the BPP side, and then creates the `Ride entry`.
3.  **BPP Sends `on_confirm` Callback**:
    *   The BPP sends an `on_confirm` callback message back to the `rider-app` (BAP), indicating the ride is officially confirmed and ready to proceed.
    *   Ride is consumed and a `Ride` is created on the BAP side.

### 2.10. Ride Tracking and Live Updates

Once the ride is confirmed, real-time updates are crucial:

*   **Driver Location Updates**: The `dynamic-offer-driver-app` continuously processes driver location data using the `location-updates` library.
*   **Status Polling/Updates (`status` / `on_status`, `track` / `on_track`, `update` / `on_update`)**:
    *   The `rider-app` (BAP) can poll for the ride's status using `GET /v2/rideBooking/{rideBookingId}/status`, which may trigger Beckn `status` requests from the BAP to the BPP.
    *   The BPP (driver app) responds with Beckn `on_status` callbacks for general ride status changes and `on_track` callbacks for granular driver location updates.
    *   Additionally, the `dynamic-offer-driver-app` can send proactive `on_update` callbacks for significant changes (e.g., driver arrival, fare adjustments, destination changes).
    *   The `rider-app` receives these updates via its `API.Beckn.OnStatus` and `API.Beckn.OnTrack` handlers, updating the UI to provide live tracking and ride information.

### 2.11. Ride Completion & Payment

*   Upon ride completion, the driver app (BPP) sends a final Beckn `on_update` or `on_status` message containing the final fare details.
*   The `rider-app` (BAP) processes this, and the payment is finalized through the platform's payment gateway, managed by `API.UI.Payment` and `API.Action.UI.RidePayment`.
