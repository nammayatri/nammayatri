# Project Brief

_This document provides a high-level overview of the project, its core requirements, and goals._

## Project Overview
NammaYatri appears to be a comprehensive mobility platform, likely focused on ride-hailing and public transport solutions. The name itself (if related to "Namma" meaning "Our" in some South Indian languages and "Yatri" meaning "Traveler") suggests a community-focused or regional transportation service. It aims to connect users (riders) with various transportation providers.

## Core Components (Inferred and Documented)
- **Backend Services**: A suite of applications and libraries managing various aspects of the platform.
    - **`rider-app` (Main Rider Application)**: Located in `Backend/app/rider-platform/rider-app/Main/`, this is the primary Haskell service handling all core rider-facing functionalities.
        - **Key Flows Handled**:
            - **User Registration & Authentication**: Manages user sign-up/login via OTP, signature-based auth, and token-based sessions (endpoints in `API.UI.Registration`, logic in `Domain.Action.UI.Registration`).
                - **Detailed Steps**: Involves initiating auth (OTP sent, `RegistrationToken` created), verifying OTP (session token generated if valid), resending OTP, and logout. Handles various client versions and device information. Uses Passetto for session tokens. Relies on DB/Redis for token/OTP storage and an SMS gateway for OTP dispatch.
            - **Ride Search (Standard & Multimodal)**: Handles requests for rides. 
                - **Endpoints**: `POST /v2/rideSearch` (standard), `POST /v2/multimodalSearch`.
                - **Common Processing**: Both are token-authenticated, process client/device context headers, apply user-specific rate limiting (Redis-backed), and log client versions. A core `SearchRequest` entity is created in the DB by `Domain.Action.UI.Search.search` to track the search, returning a `searchId`.
                - **Standard Search (`/v2/rideSearch`)**: 
                    - Conditionally attempts to cancel previous conflicting active searches/bookings by the same user.
                    - Asynchronously broadcasts Beckn V2 search requests (`TaxiACL.buildSearchReqV2`) to taxi BPPs (`CallBPP.searchV2`).
                    - Can be configured (`riderConfig.makeMultiModalSearch`) to also trigger an internal multimodal search.
                    - Responds quickly with `searchId` and basic route info; actual quotes are polled later.
                - **Multimodal Search (`/v2/multimodalSearch`)**: 
                    - Orchestrates complex journey planning using `Domain.Action.UI.Search`, `Lib.JourneyModule`, and external services.
                    - Fetches user multimodal preferences and rider configurations.
                    - May use direct single-mode public transport lookups (`JMU.getSingleModeRouteDetails` via "FRFS" `IntegratedBPPConfig`).
                    - Primarily calls an external multimodal routing engine (OTP-like, via `Kernel.External.MultiModal.Interface`) to get route suggestions.
                    - Processes and filters routes with `Lib.JourneyModule` (`JM.init`, `JM.filterTransitRoutes`), creating persisted `Journey.Journey` entities.
                    - May initiate the first valid journey (`DMC.postMultimodalInitiate`).
                    - Integrates with CRIS (Indian Railways) for subway legs (`CRISRouteFare.getRouteFare`) to fetch fares and an `sdkToken` after constructing a detailed `CRISFareRequest` (includes decrypted IMEI).
                    - Handles cases where no PT routes are found by generating default "auto" legs.
                    - Returns a `MultimodalSearchResp` with processed `journeys`, `crisSdkToken`, and potential warnings.
                - **Key Supporting Libraries**: `Kernel.Storage.Hedis` (rate limiting), `TaxiACL` & `CallBPP` (Beckn taxi), `Kernel.External.MultiModal.Interface` (OTP client), `ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare` (CRIS client), `Lib.JourneyModule` (multimodal processing), `Domain.Action.UI.Search`, `Domain.Action.UI.MultimodalConfirm`.
            - **Quote Retrieval**: After a search is initiated (standard or multimodal), this flow allows the client to fetch the aggregated ride options/quotes. 
                - **Endpoint**: `GET /v2/rideSearch/{searchId}/results` (within `API.UI.Quote`, part of `API.UI` in `rider-app`). Requires `TokenAuth` and takes the `searchId` path parameter and an optional `allowMultiple` query parameter.
                - **Functionality**: Delegates to `Domain.Action.UI.Quote.getQuotes`. This domain action retrieves all relevant and valid quotes (e.g., `Estimate` records) associated with the `searchId` from the database/cache. It may filter or rank these quotes based on `allowMultiple` or other business logic before returning them as a list of `DQuote.OfferRes` in the `DQuote.GetQuotesRes` response. This allows the user to see and compare available ride options.
            - **Ride Selection**: After search and quote retrieval, this flow (`API.UI.Select` in `rider-app`) manages the user's selection of a specific ride quote/estimate.
                - **Endpoints**: 
                    - `POST /v2/estimate/{estimateId}/select` (primary) & `/select2`: User selects a quote. This triggers a Beckn "select" message to the chosen BPP (`CallBPP.selectV2` with payload from `ACL.buildSelectReqV2`). Uses Redis lock for concurrency control.
                    - `GET /v2/estimate/{estimateId}/quotes` & `/results`: For polling/fetching selection results (BPP's `on_select` response details).
                    - `POST /v2/estimate/{estimateId}/cancel`: To cancel the selection process (may also trigger Beckn cancel to BPP).
                    - `POST /v2/estimate/{estimateId}/rejectUpgrade`: Specific flow to reject a provider upgrade, involves tagging user via `Yudhishthira`.
                - **Functionality**: Delegates to `Domain.Action.UI.Select` and `Domain.Action.UI.Cancel`. Calculates and returns `selectTtl` based on `BecknConfig` to guide client polling for BPP response. Manages internal state of the `Estimate`.
            - **Ride Confirmation (Beckn Init)**: After selection, handles the user's confirmation to proceed (`API.UI.Confirm` via `/v2/rideSearch/quotes/{quoteId}/confirm`). This triggers a Beckn "init" message to the BPP, creates a preliminary booking record, and returns a TTL for the next step.
                - **Functionality**: Delegates to `Domain.Action.UI.Confirm.confirm` which prepares data, and then calls `ACL.buildInitReqV2` and `CallBPP.initV2` to send the Beckn "init" message. Handles errors by attempting to cancel the preliminary booking.
            - **Ride Booking Finalization (Beckn Confirm) & Management**: After the BPP responds to "init" (via an internal `on_init` processing), the `API.UI.Booking` module (e.g., via `POST /rideBooking/{rideBookingId}`) likely triggers the Beckn "confirm" message to the BPP to finalize the booking. 
                - **Endpoints**: `POST /rideBooking/{rideBookingId}` (finalize), `GET /rideBooking/v2/{rideBookingId}` (status polling), `GET /rideBooking/list` & `listV2` (history), `POST /rideBooking/{rideBookingId}/addStop` & `editStop` (in-ride modifications).
                - **Functionality**: Delegates to `Domain.Action.UI.Booking`. The finalization step involves constructing and sending a Beckn "confirm" message to the BPP.
            - **Ride Payment Setup & Management**: Manages user payment methods (list, set default, delete), handles payment intent creation (setup and payment intents for SCA/3DS flows), updates payment methods for rides, and allows adding tips (`API.Action.UI.RidePayment`).
                - **Endpoints**: `GET /payment/methods`, `POST /payment/methods/{id}/makeDefault`, `DELETE /payment/methods/{id}/delete`, `GET /payment/intent/setup`, `GET /payment/intent/payment`, `POST /{rideId}/method/{id}/update`, `POST /{rideId}/addTip`, `GET /payment/customer`.
                - **Functionality**: Delegates to `Domain.Action.UI.RidePayment` for logic involving interaction with payment gateways for managing payment profiles and preparing for transactions.
            - **Ride Payment Execution & Status**: After a ride (or when payment is due), initiates the actual payment order creation for a ride (via `API.UI.Payment` using a generic `Lib.Payment.API` structure) and allows polling for payment status. 
                - **Functionality**: Delegates to `Domain.Action.UI.Payment` to create payment orders and fetch status, interacting with the `payment` shared library and external gateways.
            - **Active Ride Management & Information (`API.UI.Ride`)**: Provides endpoints for tracking driver location, getting current ride status, editing ride destination, and retrieving ride-related media like delivery images or driver photos.
                - **Endpoints**: `POST /v2/ride/{rideId}/driver/location`, `GET /v2/ride/{rideId}/status`, `POST /v2/ride/{rideId}/edit/location`, `GET /v2/ride/{rideId}/deliveryImage`, `GET /v2/ride/driver/photo/media`.
                - **Functionality**: Delegates to `Domain.Action.UI.Ride` and `SharedLogic.Ride`.
            - **Post-Ride Feedback (Rating & Know Your Driver) (`API.UI.Rating`)**: Allows riders to submit ratings for their rides (which are conditionally sent to BPPs via Beckn if provider is `ValueAddNP`) and to retrieve driver profile information.
                - **Endpoints**: `POST /v2/feedback/rateRide`, `GET /v2/feedback/knowYourDriver/{rideId}`, `GET /v2/feedback/knowYourFavDriver/{driverId}`.
                - **Functionality**: Delegates to `Domain.Action.UI.Feedback`. Beckn rating uses `ACL.buildRatingReqV2` and `CallBPP.feedbackV2`.
            - **Detailed Feedback Submission (`API.UI.FeedbackForm`)**: Enables riders to fetch a structured feedback form (potentially based on an initial rating using `GET /v2/feedback/form?rating={rating}`) and submit detailed feedback (`POST /v2/feedback/submit`) about their ride experience.
                - **Functionality**: Delegates to `Domain.Action.UI.FeedbackForm`.
            - **Invoice Retrieval (`API.Action.UI.Invoice` via `API.UI.hs` context)**: Allows riders to fetch their ride invoices for a specified date range (`GET /v2/invoice?from={date}&to={date}`).
                - **Functionality**: Delegates to `Domain.Action.UI.Invoice`.
            - **Price Breakup Retrieval (`API.Action.UI.PriceBreakup`)**: Enables riders to fetch a detailed breakdown of the fare for a specific booking (`GET /v2/priceBreakup?bookingId={bookingId}`).
                - **Functionality**: Delegates to `Domain.Action.UI.PriceBreakup`.
            - **User Profile Management (`API.UI.Profile`)**: Allows riders to get (`GET /v2/profile`) and update (`POST /v2/profile`) their profile details, including personal information and emergency contact settings (via `/updateEmergencySettings`, `/getEmergencySettings`, `/defaultEmergencyNumbers` sub-routes).
                - **Functionality**: Delegates to `Domain.Action.UI.Profile`. Processes client context headers.
            - **SOS / Emergency Alert Management (`API.Action.UI.Sos` and `API.UI.Sos`)**: Handles creation of SOS alerts (`POST /v2/sos/create`), status updates, marking rides as safe, calling police, uploading SOS media (`POST /v2/sos/{sosId}/upload` using `MultipartForm`), and processing IVR outcomes for emergency calls (unauthenticated `GET /sos/IvrOutcome` webhook).
                - **Functionality**: Delegates to `Domain.Action.UI.Sos`.
            - **Issue Management / Support (`API.UI.Issue`)**: Provides a comprehensive system for riders to report and manage issues or grievances.
                - **Shared Component**: Leverages a common `IssueManagement.API.UI.Issue` API structure, adapted for `rider-app` context via a `customerIssueHandle`.
                - **Endpoints**: Supports creating issue reports (linked to rides or ticket bookings), listing reports, uploading/fetching media for issues, getting issue categories and dynamic options (for structured reporting), viewing/updating/deleting specific issues, and updating issue statuses (including customer responses/ratings).
                - **Beckn IGM Integration**: For relevant issues, it constructs and sends Beckn IGM `issue` messages (for creation and updates, e.g., when a rider closes an issue with feedback) to BPPs asynchronously. It also includes an endpoint to sync IGM issue statuses with BPPs by polling them.
                - **External Ticketing**: The `customerIssueHandle` can integrate with an external ticketing system (`Tools.Ticket`).
                - **Internal Reporting**: Can report certain issues internally (`SharedLogic.CallBPPInternal`).
                - **Data Management**: Uses local database (`DIGM.IGMIssue`, `IssueReport`) and cached queries for categories/options. The `customerIssueHandle` is responsible for fetching and casting `rider-app` specific entities (Person, Ride, Booking, etc.) into common types for the Issue Management library.
            - **Direct Support Requests (`API.UI.Support`)**: Complements the formal issue management system by providing more direct support channels.
                - **Endpoints**: Allows users to `POST /v2/support/sendIssue` (for general issues), request a callback (`POST /v2/support/callbackRequest`), and submit safety-check related support requests (`POST /v2/support/safetyCheckSupport`).
                - **Functionality**: Delegates to `Domain.Action.UI.Support` for processing these direct requests, which might involve creating internal tickets or alerting support teams.
            - **Ride Cancellation (`API.UI.Cancel`)**: Manages rider-initiated cancellations.
                - **Endpoints**: Provides options for "soft cancel" (`POST /v2/rideBooking/{rideBookingId}/softCancel`) and "hard cancel" (`POST /v2/rideBooking/{rideBookingId}/cancel` with `DCancel.CancelReq` body). Also allows fetching potential cancellation dues/fees (`GET /v2/rideBooking/cancellationDues`).
                - **Functionality**: Both cancellation types delegate to `Domain.Action.UI.Cancel` and involve sending a Beckn "cancel" message (payload from `ACL.buildCancelReqV2`) to the BPP (`CallBPP.cancelV2`). The hard cancel considers a `reallocate` flag. The domain logic handles state changes and fee calculations (if any).
            - **FCM Push Notification Triggering (`API.Action.UI.TriggerFCM`)**: Provides an authenticated API endpoint (`POST /v2/triggerFCM/message`) for internal services or admin actions to send Firebase Cloud Messaging (FCM) push notifications to riders.
                - **Request**: Takes `TriggerFcmReq` containing target user/device, title, body, and data payload.
                - **Functionality**: Delegates to `Domain.Action.UI.TriggerFCM.postTriggerFCMMessage` which constructs and dispatches the FCM message, likely via the `external` library (e.g., AWS SNS as a proxy to FCM) or a direct FCM HTTP client.
            - **Beckn Callback Handling (`API.Beckn` and `API.BecknAPIV2`)**: Defines the entry points for `rider-app` (acting as a BAP) to receive asynchronous callback messages from BPPs (or the Beckn Gateway).
                - **Endpoints**: Aggregates handlers for all standard Beckn `on_action` callbacks (e.g., `on_search`, `on_select`, `on_init`, `on_confirm`, `on_status`, `on_track`, `on_cancel`, `on_update`) under `/cab/v1/` and `/beckn/cab/v1/{merchantId}/` paths.
                - **Authentication**: All callback endpoints are secured using `SignatureAuth`, verifying the digital signature of incoming requests.
                - **Modular Handling**: Each `on_action` is routed to a specific handler module (e.g., `API.Beckn.OnSearch`, `API.Beckn.OnSelect`) which then delegates to domain logic to process the BPP's response and update the state of the ongoing transaction (search, booking, ride, etc.).
            - **Beckn `on_search` Callback Processing (`API.Beckn.OnSearch`)**: This module specifically handles the `on_search` responses from BPPs, which contain their quotes/offers for a ride search.
                - **Endpoint**: Part of the aggregated Beckn callback API (e.g., `/cab/v1/on_search`), secured by `SignatureAuth`.
                - **Functionality**: 
                    - **Validation**: Extracts `transactionId` (original `searchId`) and `messageId` from the Beckn context. Fetches the original `SearchRequest` and validates the callback's context against it.
                    - **Data Transformation**: Transforms the incoming Beckn `OnSearchReqV2` (BPP's catalog of offers) into an internal domain request (`DOnSearch.DOnSearchReq`) using `TaxiACL.buildOnSearchReqV2`.
                    - **Idempotency**: Uses two-stage Redis locks (based on `messageId` and `bppSubscriberId`) to ensure robust and idempotent processing of potentially duplicate messages from BPPs.
                    - **Offer Validation**: Performs domain-level validation on the transformed offers using `DOnSearch.validateRequest` against the original search criteria.
                    - **Logging**: Asynchronously pushes raw callback logs (e.g., for ONDC compliance) via `TransactionLogs.pushLogs`.
                    - **Quote Persistence**: Asynchronously processes the validated quotes via `Domain.Action.Beckn.OnSearch.onSearch`. This core domain action is responsible for parsing all offers from the BPP response and persisting them as `Estimate` entities (or similar) in the database, linked to the original `SearchRequest`. These stored estimates are then available for retrieval by the rider via `API.UI.Quote`.
                - **Response to BPP**: Sends a standard Beckn `Ack` (acknowledgment of receipt) to the BPP quickly, while core processing happens asynchronously.
            - **Beckn `on_select` Callback Processing (`API.Beckn.OnSelect`)**: This module handles the `on_select` responses from BPPs, which confirm the BPP's acceptance of a previously selected ride offer and provide final quote details.
                - **Endpoint**: Part of the aggregated Beckn callback API (e.g., `/cab/v1/on_select`), secured by `SignatureAuth`.
                - **Functionality**: 
                    - **Validation**: Extracts `transactionId` and `messageId`. Validates the callback context.
                    - **Data Transformation**: Transforms the incoming Beckn `OnSelectReqV2` into an internal domain request (`DOnSelect.DOnSelectReq`) using `ACL.buildOnSelectReqV2`.
                    - **Idempotency**: Uses Redis locks (based on `messageId`) to ensure idempotent processing.
                    - **Offer Validation**: Performs domain-level validation of the BPP's confirmed offer using `DOnSelect.validateRequest` (comparing against the originally selected `Estimate` and `SearchRequest`).
                    - **Logging**: Asynchronously pushes raw callback logs (e.g., for ONDC) via `TransactionLogs.pushLogs`.
                    - **Quote Update**: Asynchronously processes the validated offer via `Domain.Action.Beckn.OnSelect.onSelect`. This updates the corresponding `Estimate` entity in the database with the BPP-confirmed details (final price, terms, fulfillment info), making it ready for the user to proceed to the "init" phase.
                - **Response to BPP**: Sends a standard Beckn `Ack` upon successful reception.
            - **Beckn `on_init` Callback Processing & Auto-"confirm" (`API.Beckn.OnInit`)**: This module handles the `on_init` responses from BPPs, which acknowledge the BAP's "init" (user's intent to book) and provide finalized order details.
                - **Endpoint**: Part of the aggregated Beckn callback API (e.g., `/cab/v1/on_init`), secured by `SignatureAuth`.
                - **Functionality**: 
                    - **Validation & Transformation**: Validates context and transforms the incoming Beckn `OnInitReqV2` into an internal `DOnInit.DOnInitReq` using `TaxiACL.buildOnInitReqV2` (from `Beckn.ACL.OnInit`). If transformation fails, attempts to cancel the booking.
                    - **Idempotency**: Uses a Redis lock (`onInitLockKey` based on `bookingId`) for processing.
                    - **Core Logic (`Domain.Action.Beckn.OnInit.onInit`)**: This domain action validates the BPP's `on_init` data against the existing booking record and updates the booking with the finalized details (e.g., exact fare, payment terms from BPP).
                    - **Automatic Beckn "confirm"**: Upon successful processing of `on_init`, this module *automatically* constructs a Beckn "confirm" message (using `ACL.buildConfirmReqV2` from `Beckn.ACL.Confirm`) and sends it to the BPP via `CallBPP.confirmV2` (asynchronously with retries). Metrics for this outgoing "confirm" are recorded.
                    - **Error Handling for "confirm"**: If sending the Beckn "confirm" fails, an error handler (`errHandler`) attempts to cancel the booking locally (`DCancel.cancel`) and sends a Beckn "cancel" message to the BPP.
                    - **Logging**: Asynchronously pushes raw `on_init` callback logs (e.g., for ONDC) via `TransactionLogs.pushLogs`.
                - **Response to BPP**: Sends a standard Beckn `Ack` upon successful reception/initial validation of `on_init`.
            - **Beckn `on_confirm` Callback Processing (`API.Beckn.OnConfirm`)**: This module handles the `on_confirm` responses from BPPs, which is the BPP's final confirmation of a booking.
                - **Endpoint**: Part of the aggregated Beckn callback API (e.g., `/cab/v1/on_confirm`), secured by `SignatureAuth`.
                - **Functionality**: 
                    - **Validation & Transformation**: Extracts `transactionId` and `bppBookingId`. Transforms the incoming Beckn `OnConfirmReqV2` into an internal domain request (`DOnConfirm.DOnConfirmReq` - which can be `RideAssigned` or `BookingConfirmed`) using `ACL.buildOnConfirmReqV2`, influenced by an `isValueAddNP` flag for the BPP.
                    - **Idempotency**: Uses Redis locks (based on `bppBookingId`) for processing.
                    - **Offer Validation**: Performs domain-level validation using `DOnConfirm.validateRequest`.
                    - **Metrics & Logging**: Finishes `CONFIRM` metrics and asynchronously pushes raw callback logs.
                    - **Booking Finalization**: The core logic `Domain.Action.Beckn.OnConfirm.onConfirm` is called (conditionally forked based on `tripCategory`, e.g., synchronous for `OneWay MeterRide` assignments, otherwise asynchronous). This action updates the local `Booking` status to confirmed and stores final details from the BPP (driver, vehicle, OTPs, etc.). This is the point the booking is officially active in `rider-app`.
                - **Response to BPP**: Sends a standard Beckn `Ack` upon successful reception.
            - **Beckn `on_status` Callback Processing (`API.Beckn.OnStatus`)**: This module handles the `on_status` responses from BPPs, which provide asynchronous updates on the status of an active or ongoing ride.
                - **Endpoint**: Part of the aggregated Beckn callback API (e.g., `/cab/v1/on_status`), secured by `SignatureAuth`.
                - **Functionality**: 
                    - **Validation & Transformation**: Extracts `transactionId` and `messageId`. Transforms the incoming Beckn `OnStatusReqV2` (containing order state and fulfillment updates) into an internal `DOnStatus.DOnStatusReq` using `ACL.buildOnStatusReqV2`.
                    - **Idempotency**: Uses Redis locks (based on `messageId`) for processing.
                    - **Request Validation**: Performs domain-level validation of the status update using `DOnStatus.validateRequest`.
                    - **Logging**: Asynchronously pushes raw callback logs via `TransactionLogs.pushLogs`.
                    - **Status Update**: Asynchronously processes the validated update via `Domain.Action.Beckn.OnStatus.onStatus`. This core domain action updates the local `Booking` and/or `Ride` entities in the database with the new status and any changed details (e.g., driver location, ETAs, fulfillment OTPs) received from the BPP. It may trigger further actions based on the new status (e.g., payment on completion).
                - **Response to BPP**: Sends a standard Beckn `Ack` upon successful reception.
            - **Beckn `on_track` Callback Processing (`API.Beckn.OnTrack`)**: This module handles `on_track` responses from BPPs, providing real-time vehicle tracking information during an active ride.
                - **Endpoint**: Part of the aggregated Beckn callback API (e.g., `/cab/v1/on_track`), secured by `SignatureAuth`.
                - **Functionality**: 
                    - **Validation & Transformation**: Extracts `transactionId`. Transforms the incoming Beckn `OnTrackReqV2` (containing vehicle location in a `tracking` object) into an internal `DOnTrack.DOnTrackReq` using `ACL.buildOnTrackReqV2`.
                    - **Request Validation**: Performs domain-level validation of the tracking update using `DOnTrack.validateRequest` against the active `Ride` and `Booking`.
                    - **Logging**: Asynchronously pushes raw callback logs via `TransactionLogs.pushLogs`.
                    - **Location Update**: Asynchronously processes the validated tracking data via `Domain.Action.Beckn.OnTrack.onTrack`. This core domain action updates the vehicle's current location in the database or a real-time location store, making it available for the rider's live map tracking feature (e.g., via `API.UI.Ride`'s driver location endpoint).
                - **Response to BPP**: Sends a standard Beckn `Ack` upon successful reception.
            - **Beckn `on_cancel` Callback Processing (`API.Beckn.OnCancel`)**: This module handles `on_cancel` responses from BPPs, which confirm the BPP has processed a ride cancellation request.
                - **Endpoint**: Part of the aggregated Beckn callback API (e.g., `/cab/v1/on_cancel`), secured by `SignatureAuth`.
                - **Functionality**: 
                    - **Validation & Status Parsing**: Extracts `transactionId` and `messageId`. Critically, it parses the `orderStatus` from the Beckn message to distinguish between a definitive `CANCELLED` state and a `SOFT_CANCEL` state.
                    - **Data Transformation**: Transforms the incoming Beckn `OnCancelReqV2` into an internal `DOnCancel.DOnCancelReq` using `ACL.buildOnCancelReq`.
                    - **Idempotency**: Uses Redis locks (based on `messageId`) for processing.
                    - **Request Validation**: Performs domain-level validation using `DOnCancel.validateRequest`.
                    - **Logging**: Asynchronously pushes raw callback logs via `TransactionLogs.pushLogs`.
                    - **Cancellation Finalization**: Asynchronously processes the cancellation by calling either `Domain.Action.Beckn.OnCancel.onCancel` (for `CANCELLED`) or `DOnCancel.onSoftCancel` (for `SOFT_CANCEL`). These domain actions finalize the status of the local `Booking` and/or `Ride` entities.
                - **Response to BPP**: Sends a standard Beckn `Ack` upon successful reception.
            - **Beckn `on_update` Callback Processing (`API.Beckn.OnUpdate`)**: Handles `on_update` responses from BPPs, which convey various changes or events related to an active booking/ride (e.g., driver reassignment, ETA changes, fare adjustments, BPP-initiated cancellation, new messages, safety alerts).
                - **Endpoint**: Part of the aggregated Beckn callback API (e.g., `/cab/v1/on_update`), secured by `SignatureAuth`.
                - **Functionality**: 
                    - **Validation & Transformation**: Extracts `transactionId` and `messageId`. Transforms the incoming Beckn `OnUpdateReqV2` into a versatile internal domain sum type (`DOnUpdate.DOnUpdateReq` and then `DOnUpdate.ValidatedOnUpdateReq` with many constructors for different update types) using `ACL.buildOnUpdateReqV2`.
                    - **Idempotency**: Uses Redis locks (based on `messageId`) for processing.
                    - **Request Validation**: Performs domain-level validation of the update using `DOnUpdate.validateRequest`.
                    - **Logging**: Asynchronously pushes raw callback logs via `TransactionLogs.pushLogs`.
                    - **Update Application**: Asynchronously processes the specific validated update via `Domain.Action.Beckn.OnUpdate.onUpdate`. This core domain action pattern matches on the type of update (e.g., `OUValidatedRideAssignedReq`, `OUValidatedRideCompletedReq`, `OUValidatedBookingCancelledReq`, etc.) and applies the corresponding changes to local `Booking` and/or `Ride` entities, potentially triggering notifications or other workflows.
                - **Response to BPP**: Sends a standard Beckn `Ack` upon successful reception.
            - **In-App Call Management (`API.UI.Call`)**: Facilitates voice communication, likely between rider and driver, using an external telephony provider (e.g., Exotel).
                - **Backend-Initiated Calls**: Allows riders to initiate a call to their driver for a specific ride (`POST /v2/ride/{rideId}/call/driver`). The system tracks the call status (`GET /v2/ride/{rideId}/call/{callId}/status`) and receives status updates from the telephony provider via a callback (`POST /v2/ride/call/statusCallback`).
                - **Frontend/IVR-Driven Calls**: Provides endpoints for an external telephony/IVR system (e.g., Exotel) to fetch driver numbers (`GET /v2/exotel/call/driver/number`, `GET /v2/exotel/call/alternate/driver/number`) to connect calls, and for the system to directly report call status updates to `rider-app` (`GET /v2/exotel/call/statusCallback`).
                - **Functionality**: Delegates to `Domain.Action.UI.Call` for interacting with the telephony provider, managing call status records, and number masking.
            - **Call Event Logging (`API.UI.CallEvent`)**: Provides an authenticated endpoint (`POST /v2/callEvent`) for logging specific events that occur during or related to a call (e.g., answered, hangup, DTMF input).
                - **Request**: Takes `DCE.CallEventReq` containing event details.
                - **Functionality**: Delegates to `Domain.Action.UI.CallEvent.logCallEvent` for processing and persisting these granular call events, likely for audit, analytics, or more detailed call state tracking.
            - **Saved Location Management (`API.UI.SavedReqLocation`)**: Allows riders to manage their favorite or frequently used locations.
                - **Endpoints**: Provides authenticated endpoints to `POST /v2/savedLocation` (create/save a new location with a tag), `GET /v2/savedLocation/list` (retrieve all saved locations), and `DELETE /v2/savedLocation/{tag}` (delete a location by its tag).
                - **Functionality**: Delegates to `Domain.Action.UI.SavedReqLocation` for validating and persisting/deleting saved location data in the database, associated with the rider.
            - **Rentals/Intercity Fare Cache Retrieval (`API.UI.RentalsIntercityCache`)**: Provides an authenticated endpoint (`POST /v2/fetchFareCache`) for fetching cached fare information, likely for rental or intercity services.
                - **Request**: Takes `RIC.FareCacheReq` (likely containing origin, destination, rental package details, etc.).
                - **Functionality**: Delegates to `Domain.Action.UI.RentalsIntercityCache.rentalsIntercityCache` which retrieves pre-computed or previously fetched fare estimates from a cache to optimize response times for these potentially complex fare calculations.
            - **Multimodal Journey Lifecycle Management (`API.Action.UI.MultimodalConfirm`)**: Manages the entire lifecycle of a selected multimodal journey.
                - **Endpoints**: Provides a comprehensive suite of authenticated endpoints under `/multimodal/` for initiating a journey, confirming it, getting booking/payment/journey status, updating payment orders, switching/skipping/adding/extending legs (including getting fares for extensions), cancelling the journey, updating rider location, submitting/retrieving feedback for the journey, and managing user multimodal preferences. Also includes endpoints for fetching general public transport data and lite transit options.
                - **Functionality**: Delegates all logic to `Domain.Action.UI.MultimodalConfirm`. This domain module is responsible for complex orchestration, including state management of `Journey.Journey` and `Lib.JourneyLeg.Types.JourneyLegStatus`, potential Beckn interactions for individual legs, payment updates, and preference management.
            - **BBPS Integration (`API.Action.UI.BBPS`)**: Manages interactions with the Bharat Bill Payment System.
                - **Endpoints**: Provides authenticated endpoints for creating a BBPS session (`POST /v2/bbps/session`), creating a BBPS payment order (`POST /v2/bbps/createOrder`), getting order status (`GET /v2/bbps/getOrderStatus/{orderId}`), and listing BBPS orders (`GET /v2/bbps/orders`). Also includes unauthenticated callbacks (`POST /bbps/confirm-payment`, `POST /bbps/payment-status`) for external BBPS systems to post updates.
                - **Functionality**: Delegates to `Domain.Action.UI.BBPS`. Enables users to potentially pay for public transport tickets/passes or other utility bills via BBPS through the platform. Uses standard platform payment order creation for BBPS payments.
            - **CRIS Specific Interactions (`API.Action.UI.CRIS`)**: Manages specialized interactions with the Centre for Railway Information Systems (CRIS) for metro/subway services.
                - **Endpoints**: Provides authenticated endpoints for fetching data required by a CRIS SDK (`POST /v2/cris/getSDKData`), requesting CRIS OTP generation (`GET /v2/cris/otp/generation`), and managing device registration with CRIS (`POST /v2/cris/change/device`).
                - **Functionality**: Delegates to `Domain.Action.UI.CRIS`. These support deeper integration with CRIS beyond just fare retrieval, enabling user authentication or transaction authorization with CRIS and device management for CRIS-based ticketing.
            - **Public Transport Ticket Service (`API.Action.UI.TicketService`)**: Manages the lifecycle of public transport ticketing.
                - **Endpoints**: Provides authenticated endpoints under `/v2/ticket/` for discovering ticketable places (`/places`) and services (`/places/{placeId}/services`), booking tickets (`/places/{placeId}/book` which initiates a payment order), listing bookings (`/bookings`), getting booking details & status, verifying tickets, and cancelling ticket bookings or services.
                - **Functionality**: Delegates to `Domain.Action.UI.TicketService`. Integrates with the platform's payment system for ticket purchases. Supports features like seat updates.
            - **FRFS Public Transport Ticket Service (`API.Action.UI.FRFSTicketService`)**: Manages a comprehensive lifecycle for ticketing related to FRFS-based public transport (e.g., buses).
                - **Endpoints**: Extensive authenticated APIs under `/v2/frfs/` for discovering routes (`/routes`), stations (`/stations`, with autocomplete), and specific route details (`/route/{routeCode}`); searching for trips (`/search`, `/discovery/search`); getting quotes (`/search/{searchId}/quote`); confirming quotes and booking (`/quote/{quoteId}/confirm`); managing bookings (status, list, cancel, check cancellability); verifying tickets (`/ticket/verify`); and fetching FRFS configurations (`/config`).
                - **Functionality**: Delegates all logic to `Domain.Action.UI.FRFSTicketService`. Supports a full ticketing flow from discovery to post-booking management, likely integrating with external FRFS data sources or BPPs and the platform's payment system for ticket purchases.
            - **Kapture Customer Login (`API.Action.UI.TicketKapture`)**: Facilitates integration with an external Kapture CRM or customer support platform.
                - **Endpoint**: Provides an authenticated endpoint `POST /v2/kaptureCustomerLogin`.
                - **Functionality**: Delegates to `Domain.Action.UI.TicketKapture.postKaptureCustomerLogin`. This action likely uses the authenticated NammaYatri user's context to establish a session or retrieve user-specific information/tickets from the Kapture system, enabling a more seamless support experience across platforms.
            - **Insurance Details Retrieval (`API.Action.UI.Insurance`)**: Provides a way for riders to fetch details of their insurance policies.
                - **Endpoint**: Authenticated endpoint `GET /v2/insurance/{referenceId}` where `referenceId` is the ID of the insurance policy.
                - **Functionality**: Delegates to `Domain.Action.UI.Insurance.getInsurance`. This retrieves the specified insurance policy details from the database, likely after an authorization check to ensure the user can access it. Does not cover offering or purchasing insurance.
            - **Customer Referral Program (`API.Action.UI.CustomerReferral`)**: Manages a customer referral system.
                - **Endpoints**: Authenticated APIs for users to apply a referral code (`POST /v2/person/applyReferral`), get their referral count (`GET /v2/CustomerRefferal/count`), view payout history (`GET /v2/referralPayout/history`), verify a VPA for payouts (`GET /v2/referral/verifyVpa`), and add/update their payout VPA (`POST /v2/payoutVpa/upsert`).
                - **Functionality**: Delegates to `Domain.Action.UI.CustomerReferral`. Handles referral code validation, linking referrer/referee, crediting benefits, and managing VPA-based payouts for referrals.
            - **Social Media Login & Profile Update (`API.Action.UI.SocialLogin`)**: Enables user authentication and profile linking via third-party social media providers.
                - **Endpoints**: Provides an unauthenticated endpoint `POST /v2/social/login` (for social sign-in/sign-up) and an authenticated endpoint `POST /v2/social/update/profile` (to link social account to existing NammaYatri profile).
                - **Request**: `SocialLoginReq` (with provider name and social token/code) for login; `SocialUpdateProfileReq` for profile update.
                - **Functionality**: Delegates to `Domain.Action.UI.SocialLogin`. This involves server-to-server validation of the social token with the provider, fetching user details, and then either creating a new NammaYatri user or linking to an existing one, finally returning NammaYatri session tokens.
            - **Favourite Driver Management (`API.Action.UI.FavouriteDriver`)**: Allows riders to manage their list of favorite drivers.
                - **Endpoints**: Provides authenticated APIs to `GET /v2/driver/favorites` (to list favorite drivers) and `POST /v2/favorites/{driverId}/remove` (to remove a driver from favorites). Note: Adding a favorite driver is likely handled elsewhere (e.g., post-ride rating).
                - **Functionality**: Delegates to `Domain.Action.UI.FavouriteDriver` for retrieving the list of favorite drivers associated with the user and for removing a specific driver from that list in the database.
            - **Ride Estimate Breakup Retrieval (`API.Action.UI.EstimateBP`)**: Enables riders to fetch a detailed fare estimate breakup for a specific ride.
                - **Endpoint**: Authenticated endpoint `GET /v2/ride/{rideId}/estimateBreakup`.
                - **Functionality**: Delegates to `Domain.Action.UI.EstimateBP.getRideEstimateBreakup`. This retrieves or calculates all components of a ride's fare (base, distance, time, tolls, taxes, surge, discounts) to provide a detailed breakdown for the user, specific to a `Ride` entity.
            - **Nearby Drivers Retrieval (`API.Action.UI.NearbyDrivers`)**: Allows riders to fetch information about available drivers in their vicinity.
                - **Endpoint**: Authenticated endpoint `POST /v2/nearbyDrivers`.
                - **Request**: Takes `NearbyDriverReq` (containing rider's current location, search radius, optional filters like vehicle type).
                - **Functionality**: Delegates to `Domain.Action.UI.NearbyDrivers.postNearbyDrivers`. This domain action queries a real-time driver location store, performs geospatial filtering based on rider location and criteria, and returns a list of nearby available drivers (`NearbyDriverRes`), typically for display on a map interface.
            - **Nearby Buses & Next Vehicle Details (`API.Action.UI.NearbyBuses`)**: Provides real-time public transport information.
                - **Endpoints**: Authenticated endpoints `POST /v2/nearbyBusBooking` (to find nearby buses based on user location and criteria) and `GET /v2/nextVehicleDetails/{routeCode}/{stopCode}` (to get upcoming vehicle details for a specific route/stop, with optional `vehicleType` filter).
                - **Functionality**: Delegates to `Domain.Action.UI.NearbyBuses`. Fetches and returns information about nearby operating buses (`NearbyBusesResponse`) or details of next vehicle arrivals (`UpcomingTripInfo` from `Lib.JourneyModule.Utils`), likely by querying real-time transit data feeds.
            - **Places Information Retrieval (`API.Action.UI.Places`)**: Allows riders to fetch information about specific places or Points of Interest (POIs).
                - **Endpoint**: Authenticated endpoint `POST /v2/places`.
                - **Request**: Takes `PlacesRequest` (likely containing location context, search query, category filters).
                - **Functionality**: Delegates to `Domain.Action.UI.Places.postPlaces`. This domain action queries an external POI database or place search API to find and return relevant place information (`PlacesResponse`).
            - **Live Vehicle Tracking on Route (`API.Action.UI.TrackRoute`)**: Enables riders to track live vehicle locations (e.g., buses) on a specific public transport route.
                - **Endpoint**: Authenticated endpoint `GET /v2/track/{routeCode}/vehicles`.
                - **Request**: Takes `routeCode` (path param) and optional `platformType` and `vehicleType` query params.
                - **Functionality**: Delegates to `Domain.Action.UI.TrackRoute.getTrackVehicles`. This action queries real-time vehicle location systems for the specified route and filters, returning live tracking data (`TrackingResp`) for vehicles on that route.
            - **Static Route Information Retrieval (`API.UI.Route`)**: Provides endpoints for fetching static route geometry and details (polyline, distance, time) from an external mapping service.
                - **Endpoints**: Authenticated `POST` endpoints: `/route` (general route), `/pickup/route` (driver to rider pickup), and `/trip/route` (specific trip path).
                - **Request**: Takes `Maps.GetRoutesReq` or `DRoute.GetPickupRoutesReq` (for pickup route).
                - **Functionality**: Delegates to `Domain.Action.UI.Route` which proxies requests to an external mapping service (via `Kernel.External.Maps.Interface`) and returns `Maps.GetRoutesResp`.
            - **Location Serviceability & Intercity Check (`API.UI.Serviceability`)**: Provides endpoints to check if origin/destination locations are serviceable and if a trip is intercity.
                - **Endpoints**: Authenticated `POST` endpoints: `/serviceability/origin` and `/serviceability/destination` (take `ServiceabilityReq` with `LatLong`), and `/serviceability/isInterCity` (takes `BPPInternal.IsIntercityReq`).
                - **Functionality**: Origin/destination checks delegate to `Domain.Action.UI.Serviceability.checkServiceability` (using geofencing). Intercity check delegates to `SharedLogic.CallBPPInternal.getIsInterCity` (using merchant configs/BPP logic).
            - **Hotspot Identification and Retrieval (`API.UI.HotSpot`)**: Identifies and returns relevant "hotspots" (areas of significant activity or designated special locations) near a user-provided location.
                - **Endpoint**: Authenticated endpoint `GET /v2/getHotSpot` (takes `Maps.LatLong` in request body).
                - **Functionality**: Retrieves `HotSpotConfig` for the merchant. Uses geohashing and custom geospatial logic to find neighboring geohashes. Fetches pre-aggregated `HotSpot` frequency data (based on user activities like pickups, trip starts/ends, saved locations, and special designations) from Redis. Filters and ranks these hotspots based on configured thresholds and weights, returning a list of `HotSpotInfo`.
            - **Map Services Proxy (`API.UI.Maps`)**: Proxies requests to an external mapping service for functionalities like address autocomplete, place details, and reverse geocoding.
                - **Endpoints**: Authenticated `POST` endpoints under `/v2/maps/`: `/autoComplete` (takes `AutoCompleteReq`), `/getPlaceDetails` (takes `GetPlaceDetailsReq`), and `/getPlaceName` (takes `GetPlaceNameReq` for reverse geocoding).
                - **Functionality**: Delegates to `Domain.Action.UI.Maps` which interacts with an external mapping service provider (e.g., Google Maps) to provide these common map-related utility services.
            - **Frontend State and Event Notification (`API.UI.Frontend`)**: Manages user flow status and handles event notifications from the frontend.
                - **Endpoints**: Authenticated endpoints under `/v2/frontend/`: `GET /flowStatus` (to get current user flow status, with optional `isPolling`, `checkForActiveBooking` flags) and `POST /notifyEvent` (for frontend to send `NotifyEventReq` containing `FrontendEvent` details to backend).
                - **Functionality**: Delegates to `Domain.Action.UI.Frontend`. `getPersonFlowStatus` determines and returns the user's current application flow state (`GetPersonFlowStatusRes`). `notifyEvent` processes frontend UI events, which can be used for analytics, state synchronization, or triggering backend actions.
            - **Location Edit Result and Confirmation (`API.Action.UI.EditLocation`)**: Manages the asynchronous process of a rider editing a location (e.g., destination) for an active ride, including BPP approval and rider confirmation.
                - **Endpoints**: Authenticated endpoints under `/v2/edit/`: `GET /{bookingUpdateRequestId}/result` (to poll for the outcome of an edit request, like new fare/ETA) and `POST /result/{bookingUpdateRequestId}/confirm` (for the rider to accept the proposed changes).
                - **Functionality**: Delegates to `Domain.Action.UI.EditLocation`. This flow handles BPP responses to location edit requests (received via `on_update` callbacks which update a `BookingUpdateRequest` entity) and the rider's subsequent confirmation of these changes. The initial request to edit is likely made via `API.UI.Ride`.
            - **Ride Sharing and Tracking (`API.Action.UI.FollowRide`)**: Enables riders to share their ride progress and for others to track it, enhancing safety and convenience.
                - **Endpoints**: Authenticated endpoints: `GET /v2/follow/ride` (to list followers), `POST /v2/share/ride` (to initiate ride sharing with `ShareRideReq`), `GET /v2/followRide/ECStatus/{rideId}` (for followers to check emergency contact status), and `GET /v2/followRide/{rideId}/customerDetails` (for followers to get basic rider details).
                - **Functionality**: Delegates to `Domain.Action.UI.FollowRide`. Manages sharing permissions, records follower information, and provides relevant ride/rider data to authorized followers.
            - **Aadhaar Verification (`API.UI.AadhaarVerification`)**: Supports Aadhaar-based eKYC for identity verification.
                - **Endpoints**: Authenticated `POST` endpoints under `/v2/verifyAadhaar/`: `/generateOtp` (takes `AadhaarOtpReq` with Aadhaar number) and `/verifyOtp` (takes `VerifyAadhaarOtpReq` with OTP and transaction ID).
                - **Functionality**: Delegates to `Domain.Action.UI.AadhaarVerification`. `generateAadhaarOtp` interacts with an external Aadhaar service to send an OTP. `verifyAadhaarOtp` submits the OTP to the service for validation. Successful verification can update the user's KYC status and store fetched details.
            - **App Installation Tracking/Registration (`API.UI.AppInstalls`)**: Handles the registration of app installation details with the backend.
                - **Endpoint**: Unauthenticated endpoint `POST /v2/appInstalls/create`.
                - **Request**: Takes `AppInstallsReq` (likely containing app version, device ID, OS details).
                - **Functionality**: Delegates to `Domain.Action.UI.AppInstalls.createAppInstallsDetails`. This action stores app/device information, useful for analytics, version tracking, and potentially linking the device to a user later.
            - **Cancellation Reasons Retrieval (`API.UI.CancellationReason`)**: Provides a list of standardized cancellation reasons.
                - **Endpoint**: Authenticated endpoint `GET /v2/cancellationReason/list`.
                - **Request**: Takes a mandatory query parameter `cancellationStage` (e.g., before/after driver assignment).
                - **Functionality**: Delegates to `Domain.Action.UI.CancellationReason.list`. Fetches and returns a context-specific list of `CancellationReasonAPIEntity` objects, useful for standardizing cancellation inputs for analytics and policy application.
            - **Disability/Accessibility Options Listing (`API.UI.Disability`)**: Provides a list of supported disability types or accessibility options.
                - **Endpoint**: Authenticated endpoint `GET /v2/disability/list`.
                - **Functionality**: Delegates to `Domain.Action.UI.Disability.listDisabilities`. Fetches and returns a list of predefined `Disability.Disability` types, which can be used by users to declare accessibility needs in their profile or for filtering services.
            - **Google Translate Proxy (`API.UI.GoogleTranslate`)**: Provides a backend proxy for text translation services.
                - **Endpoint**: Authenticated endpoint `GET /v2/language/translate`.
                - **Request**: Takes query parameters for `source` language, `target` language, and `q` (query text).
                - **Functionality**: Delegates to `SharedLogic.GoogleTranslate.translate`, which calls the external Google Translate API and returns the `TranslateResp` (translated text).
            - **FRFS Ticketing via Partner Organizations (`API.UI.PartnerOrganizationFRFS`)**: Exposes FRFS ticketing functionalities to authenticated partner organizations (B2B).
                - **Partner Endpoints (API Key Auth & Rate Limited)**: `/frfs/partnerOrganization/{apiKey}/upsertPersonAndGetFare` (and `getFareV2`), `/getConfig/...`, `/upsertPersonAndQuoteConfirm`. These allow partners to get FRFS fares and confirm bookings for their end-users, often involving an upsert of user details into NammaYatri.
                - **User/Ticket Endpoints**: `/frfs/shareTicketInfo/{ticketBookingId}`, `/frfs/auth/{ticketBookingId}`, `/frfs/authVerify` for managing tickets potentially booked via partners.
                - **Functionality**: Delegates to `Domain.Action.UI.PartnerOrganizationFRFS` and reuses `Domain.Action.UI.FRFSTicketService` logic. Manages partner configurations, API key validation, rate limiting, and the "upsert person" flow for partner-initiated transactions.
            - **User Personal Statistics Retrieval (`API.UI.PersonStats`)**: Allows authenticated users to fetch their personal usage statistics on the platform.
                - **Endpoint**: Authenticated endpoint `GET /v2/personStats`.
                - **Functionality**: Delegates to `Domain.Action.UI.PersonStats.getPersonStats`. This action queries various data sources to aggregate and calculate user-specific metrics like total rides, distance, spending, etc., returning them in `PersonStatsRes`.
            - **WhatsApp Opt-in/Opt-out (`API.UI.Whatsapp`)**: Manages user preferences for receiving WhatsApp communications.
                - **Endpoint**: Authenticated endpoint `POST /v2/whatsapp/opt`.
                - **Request**: Takes `Whatsapp.OptAPIRequest` (likely containing a boolean opt-in/opt-out flag).
                - **Functionality**: Delegates to `Domain.Action.UI.Whatsapp.whatsAppOptAPI`. This action updates the user's WhatsApp communication preferences in their profile or a dedicated store and may interact with a WhatsApp Business API provider.
            - **Dynamic UI Configuration Retrieval (`API.Action.UI.Cac`)**: Provides a flexible way for client applications to fetch dynamic UI configurations.
                - **Endpoint**: Authenticated endpoint `POST /v2/getUiConfigs`.
                - **Request**: Takes optional `toss` (Int for A/B testing) and `tenant` (Text) query params, and a generic JSON object (`Data.Aeson.Object`) in the body to specify requested configs.
                - **Functionality**: Delegates to `Domain.Action.UI.Cac.postGetUiConfigs`. This action fetches and returns UI configurations (as a JSON object) based on the request, potentially from a config store or by applying rules (e.g., via `ConfigPilotFrontend`). Allows for dynamic UI tailoring, feature flagging, and A/B testing.
            - **User Account Deletion Request (`API.Action.UI.DeletedPerson`)**: Allows authenticated users to request the deletion of their account.
                - **Endpoint**: Authenticated endpoint `POST /v2/deleted/person`.
                - **Request**: Takes `DeletedPersonReq` (likely for confirmation or reason).
                - **Functionality**: Delegates to `Domain.Action.UI.DeletedPerson.postDeletedPerson`. This action initiates the account deletion process, which involves marking the account, anonymizing/deleting PII according to data retention policies, and invalidating sessions.
            - **QR Scan Test Utility (`API.Action.UI.Miscellaneous`)**: Provides a utility for testing QR code scanning and backend interpretation.
                - **Endpoint**: Authenticated endpoint `POST /v2/misc/testScanQR`.
                - **Request**: Takes `QRScanTestReq` (containing scanned QR data).
                - **Functionality**: Delegates to `Domain.Action.UI.Miscellaneous.postMiscTestScanQR`. This action parses the QR data and performs test logic (e.g., validation, entity lookup, action simulation), returning a `QRScanTestResp` with the outcome.
        - **API Structure**: Exposes a multifaceted API (built with Servant and defined via Namma DSL) that includes:
            - `API.UI`: For main rider mobile/web application interactions (search, quote retrieval, select, confirm, book, payment setup, track, pay, profile, registration, etc.).
            - `API.Beckn` & `API.BecknAPIV2`: For Beckn protocol interactions.
            - `API.Dashboard`: For rider dashboard data.
            - `API.IGM`: Likely for Issue/Grievance Management.
            - `API.FRFS`: For functionality related to Fares/Rules/Fees.
            - `API.Internal`: For internal administrative operations.
            - Direct webhook endpoints for Juspay payments and payouts.
            - **Mapping Service Interactions (`API.UI.Maps`)**: Provides proxy endpoints to an external mapping service (likely Google Maps).
                - **Endpoints**: Includes `POST /v2/maps/autoComplete` (for address/place suggestions), `POST /v2/maps/getPlaceDetails` (for detailed info on a place ID), and `POST /v2/maps/getPlaceName` (for reverse geocoding lat/lon to address).
                - **Functionality**: All endpoints are token-authenticated and delegate logic to `Domain.Action.UI.Maps` which handles communication with the external map provider (request construction, API key usage, response parsing).
        It manages user authentication (likely via `passetto-client`, plus its own OTP/token logic), integrates with numerous shared libraries (`beckn-services`, `payment`, `yudhishthira`, `special-zone`, `external`, `utils`, etc.), and its own database. Emits Prometheus metrics and may consume GraphQL APIs.
    - **Public Transport Rider Platform**: Manages public transport integration for riders (e.g., `Main` BAP, `search-consumer`).
    - **Dashboards**: `safety-dashboard`, `dashboard/rider-dashboard`.
    - **Event Processing (`kafka-consumers`)**: Consumes and processes Kafka messages.
    - Other services related to provider interactions (e.g., `dynamic-offer-driver-app`), location tracking, payment processing.
- **Shared Libraries (`Backend/lib/`)**:
    - **`beckn-spec`**: Foundational library for Beckn protocol specifications (data types, core API structure definitions).
    - **`beckn-services`**: A Haskell library providing client implementations and higher-level services for interacting with Beckn APIs. It builds upon `beckn-spec` and `mobility-core`, using Servant for client generation and `http-client` for making API calls to external Beckn entities.
    - **`shared-services`**: Provides common business services (Registry, Issue Management, URL Shortener, ConfigPilotFrontend).
    - **`location-updates`**: Handles and processes real-time location updates.
    - **`special-zone`**: Manages geographically specific operational zones.
    - **`payment`**: Handles all payment processing functionalities.
    - **`scheduler`**: Manages and executes scheduled tasks.
    - **`utils`**: A comprehensive Haskell library providing a wide array of general-purpose utility functions and helper modules. It supports data conversion, HTTP interactions, image processing, geohashing, database helpers, and integrates with many other platform libraries.
    - **`yudhishthira`**: A Haskell library likely serving as a **rule engine** or **decision-making/policy enforcement hub**. It uses `json-logic-hs` and integrates with numerous platform components like `haskell-cac`, `passetto-client`, `beckn-services`, and `shared-services`. It defines its own API/storage via Namma DSL.
    - **`external`**: A Haskell library for managing integrations with various third-party services. This likely includes:
        - Email services (e.g., via AWS SES).
        - Notification services (e.g., via AWS SNS).
        - Slack integration.
        - AI Chat Completion services.
        - External logging for Kafka and transactions.
    - **`sessionizer-metrics`**: A Haskell library for collecting, processing, and exposing metrics (especially session-based ones) via Prometheus.
    - **`webhook`**: A Haskell library for receiving, processing, and managing incoming webhook notifications from external or internal services. It uses Servant for API endpoints and likely Beam for database interactions, with its own API/storage defined via Namma DSL.
    - **`producer`**: A Haskell library and executable (`producer-exe`) for generating messages (likely for Kafka) based on events or data from other services like `rider-app`, `dynamic-offer-driver-app`, `yudhishthira`, or `scheduler`. It interacts with databases and Redis.
    - `mobility-core`: (Core domain logic/types - to be detailed further if a direct source is found).
    - `haskell-cac`: (Haskell client for a C Authentication/Certificate library - used by `yudhishthira` and `utils`).
    - Other shared libraries for utilities, etc. (to be detailed).
- **Frontend Applications**: User interfaces for different actors.
- **Beckn Protocol Integration**: Deeply integrated via `beckn-gateway`, `beckn-spec`, `beckn-services`, `
- **Namma DSL (`alchemist`)**: // ... (existing content) ...
- **Developer Tools & Templates**: // ... (existing content) ...
- **Mocking Services**: // ... (existing content) ...

## Likely Goals
- To provide a technology platform for booking and managing rides, with `rider-app` serving as the core backend for rider interactions, including a secure and flexible user registration and authentication system capable of handling OTPs, signed requests, and session tokens.
- (Other goals remain largely the same but are now supported by a more detailed understanding of `rider-app` capabilities)

_
// --- Appended details for Dashboard API Structure (`API.Dashboard`) (from a previous turn) ---
            - **Dashboard API Structure (`API.Dashboard`)**: This module (`API/Dashboard.hs`) aggregates various dashboard-facing API endpoints, generally under the `/dashboard/` base path (often further contextualized by `{merchantId}/{city}`). These are typically authenticated using `DashboardTokenAuth`.
                - **Management APIs (`API.Action.Dashboard.Management`)**: A sub-aggregator for various entity management dashboard functions.
                    - **Booking Management (`API.Action.Dashboard.Management.Booking`)**: ... (details as previously added) ...
                    - **Customer Management (`API.Action.Dashboard.Management.Customer`)**: Offers dashboard endpoints for listing/searching customers (`GET .../list`), deleting (`DELETE .../{customerId}`), blocking/unblocking (`POST .../{customerId}/block`, `POST .../{customerId}/unblock`), getting customer info (`GET .../{customerId}/info`), managing cancellation dues, updating safety center blocking status, and looking up customers by phone or ID. Delegates to `Domain.Action.Dashboard.Customer`.
// ... (ensure other existing API Structure list items, particularly for other API.Dashboard and API.Action.Dashboard.Management sub-modules when they are analyzed, are preserved below this) ...

### Key Components & Services (Deep Dive)

This section will be populated with detailed summaries of key services and their flows as they are analyzed.

*   **`rider-app` (`Backend/app/rider-platform/rider-app/Main/`)**: The primary backend application serving rider-facing UIs and orchestrating the rider journey. It handles:
    *   // ... (all previously detailed rider-app flows) ...

*   **`dynamic-offer-driver-app` (`Backend/app/provider-platform/dynamic-offer-driver-app/Main/`)**: A core backend application for drivers, acting as a Beckn Provider Platform (BPP). It enables drivers to participate in services, particularly those involving dynamic offers. Key responsibilities include:
    *   **Driver Authentication & Onboarding (via `API.UI.Registration`, `API.Action.UI.DriverOnboardingV2`)**:
        *   **Initial Registration/Login (`API.UI.Registration`)**: Handles driver login and initial registration via an OTP-based flow (`POST /ui/auth`, `POST /ui/auth/{authId}/verify`). Manages `RegistrationToken`s and issues session tokens (Passetto) upon successful OTP verification. Also handles OTP resend and logout. Captures client/device context from headers.
        *   **Detailed Onboarding (`API.Action.UI.DriverOnboardingV2`)**: After initial authentication, drivers go through a comprehensive onboarding process including:
            *   Fetching onboarding configurations (e.g., mandatory documents).
            *   Viewing applicable rate cards.
            *   Managing vehicle details: submitting/viewing vehicle photos, updating AC status, managing selected service tiers.
            *   Submitting identity and regulatory documents: SSN/equivalent, PAN card, Aadhaar card.
            *   Initiating/checking background verification.
            *   Linking and verifying bank accounts for payouts.
            *   Performing liveness checks (e.g., submitting a live selfie, logging calls to an HV SDK for KYC).
    *   **Core Driver Operations & Ride Lifecycle Management (via `API.UI.Driver`)**: After onboarding, drivers use these authenticated APIs (under `/ui/driver/`) for daily operations:
        *   **Activity & Availability**: Setting online/offline status (`/setActivity`), managing "Go Home" mode (CRUD and activate/deactivate under `/goHome/`).
        *   **Ride Offer Interaction**: Fetching nearby ride requests (`/nearbyRideRequest`), proactively offering quotes (`/searchRequest/quote/offer`), responding to allocated ride offers (`/searchRequest/quote/respond`).
        *   **Active Ride Management**: Starting OTP-based rides (`/otpRide/start`), managing standard ride lifecycle events for a given `rideId` (listing rides, arrived at pickup, start ride with OTP, arrived at/departed from intermediate stops, arrived at destination, end ride with OTP/details), cancelling rides, and uploading odometer/delivery images.
        *   **Profile & Performance**: Viewing/updating profile, stats, earnings, managing profile photos.
        *   **Financials**: Managing alternate phone numbers, VPA verification, viewing payment history, clearing dues, downloading invoices.
        *   **Scheduled Bookings**: Listing and accepting pre-scheduled bookings.
        *   All logic delegated to `Domain.Action.UI.Driver` and its sub-modules (`RideStart`, `RideEnd`, `RideCancel`).
    *   **Driver-Specific UI Features (via various `API.UI.*` and `API.Action.UI.*` modules)**:
        *   **Demand Hotspots (`API.Action.UI.DemandHotspots`)**: // ... (details as previously added) ...
        *   **Profile Summary (`API.UI.DriverProfileSummary`)**: // ... (details as previously added) ...
        *   **Driver Referral Program (`API.UI.DriverReferral`)**: // ... (details as previously added) ...
        *   **Driver-Side Payment Operations (`API.UI.Payment`)**: // ... (details as previously added) ...
        *   **Driver Referral Payout Management (`API.Action.UI.ReferralPayout`)**: // ... (details as previously added) ...
        *   **Performance Metrics (`API.UI.Performance`)**: Allows authenticated drivers (`GET /ui/driver/performance`) to fetch their performance statistics (e.g., ride counts, ratings, earnings). Delegates to `Domain.Action.UI.Performance`.
        *   **Driver Messaging (`API.UI.Message`)**: Enables drivers to manage and interact with platform messages.
            *   Endpoints (under `/ui/message/`): `GET /list` (list messages), `GET /{messageId}` (view message), `PUT /{messageId}/seen` (mark seen), `PUT /{messageId}/like` (like message), `PUT /{messageId}/response` (reply to message, takes `MessageReplyReq`), `GET /media` (fetch message media).
            *   Delegates to `Domain.Action.UI.Message`.
        *   **Exotel-Triggered End Ride (`API.UI.ExotelEndRide`)**: Provides an unauthenticated endpoint (`GET /ui/exotel/ride/end`) for drivers to end rides by calling a designated Exotel number. The system identifies the driver and ride based on `CallFrom` and `CallTo` parameters and the `Exophone` configuration. Delegates to `Domain.Action.UI.ExotelEndRide`.
        *   **Merchant-Specific City Listing (`API.UI.City`)**: Allows authenticated drivers (or other UI clients) to fetch a list of cities associated with a given `merchantId` (`GET /ui/city/{merchantId}/list`). Delegates to `Domain.Action.UI.City`.
        *   **Kiosk Location Listing (`API.UI.KioskLocation`)**: Allows authenticated drivers (`GET /ui/kioskLocation/list`) to fetch a list of designated kiosk locations relevant to their operational context (merchant, city). Delegates to `Domain.Action.UI.KioskLocation`.
        *   **Driver Leaderboards (`API.UI.LeaderBoard`)**: // ... (details as previously added) ...
        *   **FCM Notification Trigger (OnMessage) (`API.UI.OnMessage`)**: Allows triggering FCM push notifications (`POST /ui/onMessage` with `FCMReq`), likely based on specific messaging events or conditions to alert drivers. Delegates to `Domain.Action.UI.OnMessage`.
        *   **Ride-Specific Route Info for Drivers (`API.UI.RideRoute`)**: Allows authenticated drivers (`POST /ui/{rideId}/route`) to fetch route geometry and details for their current assigned ride (e.g., to pickup, to destination). Delegates to `Domain.Action.UI.RideRoute` which uses an external mapping service.
        *   **Driver Plan Management (`API.UI.Plan`)**: Allows authenticated drivers to manage their subscription plans.
            *   Endpoints (under `/ui/plan/`): `GET /list` (list available plans), `PUT /suspend` & `PUT /resume` (manage plan status), `GET /currentPlan` (view active plan), `POST /{planId}/subscribe` (subscribe to a new plan, handles autopay status and dues), `PUT /{planId}/select` (switch plans), `GET /services` (list available service names for plans).
            *   Delegates to `Domain.Action.UI.Plan` and interacts with `Domain.Action.UI.Driver` for dues management.
        *   **Driver Coins/Rewards Management (`API.UI.DriverCoins`)**: Allows authenticated drivers to manage their earned coins/rewards.
            *   Endpoints (under `/ui/coins/`): `GET /transactions` (view coin history for a date), `GET /usageHistory` (paginated coin usage), `POST /convertCoinToCash` (redeem coins for cash), `GET /rideStatusPastDays` (get ride history relevant to coin earning), `GET /info` (get current coin balance and program info).
            *   Delegates to `Domain.Action.UI.DriverCoin`.
        *   **Driver Ride Summaries (`API.UI.RideSummary`)**: Allows authenticated drivers (`POST /ui/rideSummary/list` with a list of `[Day]`) to fetch summaries of their rides for specified dates. Delegates to `Domain.Action.UI.RideSummary`.
        *   **General Route Calculation for Drivers (`API.UI.Route`)**: Provides general route calculation (`POST /ui/route`), pickup route (`POST /ui/pickup/route`), and trip route (`POST /ui/trip/route`) services for authenticated drivers, taking `Maps.GetRoutesReq` and returning `Maps.GetRoutesResp`. Delegates to `Domain.Action.UI.Route` which uses an external mapping service.
        *   **Driver Issue Reporting (including SOS/Emergency) (`API.UI.Issue`)**: Allows authenticated drivers to report issues (including safety/SOS via specific categories) using a shared `IssueManagement` library. Endpoints include issue creation, listing, media upload, category/option fetching. Delegates to `IssueManagement.Common.UI.Issue` handlers, using a `driverIssueHandle` for context-specific data.
        *   **Driver Vehicle Details Management (`API.Action.UI.VehicleDetails`)**: Allows authenticated drivers to manage their vehicle information.
            *   Endpoints include: `GET /ui/vehicleMakes` (fetch makes), `POST /ui/vehicleModels` (fetch models for a make, takes `VehicleModelsReq`), `POST /ui/vehicleDetails` (submit/update full vehicle details, takes `VehicleDetailsReq`).
            *   Delegates to `Domain.Action.UI.VehicleDetails`.
        *   **Dynamic UI Configuration Retrieval (CaC) (`API.Action.UI.Cac`)**: // ... (details as previously added) ...
        *   **Driver Call Feedback Submission (`API.Action.UI.CallFeedback`)**: Allows authenticated drivers (`POST /ui/driver/call/feedback` with `CallFeedbackReq`) to submit feedback on calls made through the platform. Delegates to `Domain.Action.UI.CallFeedback`.
        *   **Dynamic Driver Profile Questions (`API.Action.UI.DriverProfileQuestions`)**: Allows fetching dynamic questions for drivers (`GET /ui/DriverProfileQues`) and submitting their answers (`POST /ui/DriverProfileQues` with `DriverProfileQuesReq`). Used for flexible data collection during onboarding/updates. Delegates to `Domain.Action.UI.DriverProfileQuestions`.
        *   **Driver Response to Booking Edits (`API.Action.UI.EditBooking`)**: Allows authenticated drivers (`POST /ui/edit/result/{bookingUpdateRequestId}` with `EditBookingRespondAPIReq`) to accept/reject proposed in-ride booking modifications (e.g., destination change). Delegates to `Domain.Action.UI.EditBooking`.
        *   **Fare Calculation/Estimation for Drivers (`API.Action.UI.FareCalculator`)**: Allows authenticated drivers (`GET /ui/calculateFare`) to estimate fares based on pickup/drop locations and a `distanceWeightage` parameter. Delegates to `Domain.Action.UI.FareCalculator`.
        *   **Driver-Side Insurance Details Retrieval (`API.Action.UI.Insurance`)**: // ... (details as previously added) ...
        *   **Driver Invoice Retrieval (Filtered) (`API.Action.UI.Invoice`)**: Allows authenticated drivers (`GET /ui/invoice`) to fetch their invoices, filterable by date range (`fromDate`, `toDate`) and vehicle registration number (`rcNo`). Delegates to `Domain.Action.UI.Invoice`.
        *   **Driver Learning Management System (LMS) (`API.Action.UI.LmsModule`)**: // ... (details as previously added) ...
        *   **Driver-Contextual City Configurations Retrieval (`API.Action.UI.Merchant`)**: Allows authenticated drivers (`GET /ui/cityConfigs`) to fetch configurations specific to their current merchant and operating city. Delegates to `Domain.Action.UI.Merchant`.
        *   **Meter Ride Management (`API.Action.UI.MeterRide`)**: Allows drivers to manage meter-based rides, including adding a destination (`POST /ui/meterRide/{rideId}/addDestination`) and sharing a receipt (`POST /ui/meterRide/{rideId}/shareReceipt`). Delegates to `Domain.Action.UI.MeterRide`.
        *   **Driver Interaction with Operation Hubs (`API.Action.UI.OperationHub`)**: Allows drivers to list operation hubs (`GET /ui/operation/getAllHubs`), create requests to these hubs (`POST /ui/operation/createRequest` with `DriverOperationHubRequest`), and view their request history (`GET /ui/operation/getRequests` with filters including `rcNo`). Delegates to `Domain.Action.UI.OperationHub`.
        *   **Driver Consent to Operator Association (`API.Action.UI.Operator`)**: Allows authenticated drivers (`POST /ui/operator/consent`) to consent to an association with a fleet operator. Delegates to `Domain.Action.UI.Operator`.
        *   **Driver Fare Details (Meter Pricing & Price Breakup) (`API.Action.UI.PriceBreakup`)**: Allows drivers to finalize meter ride prices (`POST /ui/meterRide/price`) and get detailed fare breakdowns for any ride (`GET /ui/priceBreakup`). Delegates to `Domain.Action.UI.PriceBreakup`.
        *   **Driver "Reels" (Short Video Content) Retrieval (`API.Action.UI.Reels`)**: Allows authenticated drivers (`GET /ui/reels/getAllReelVideos`) to fetch short video content based on a `reelsKey` and optional `language`. Delegates to `Domain.Action.UI.Reels`.
        *   **Driver Social Login & Profile Linking (`API.Action.UI.SocialLogin`)**: Enables drivers to sign-up/login via social media (`POST /ui/social/login`) or link social accounts to existing profiles (`POST /ui/social/update/profile`). Delegates to `Domain.Action.UI.SocialLogin`.
        *   **Driver Special Location Listing (`API.Action.UI.SpecialLocation`)**: Allows authenticated drivers (`GET /ui/specialLocation/list`) to fetch a list of special locations (e.g., airports, malls) with an optional `isOrigin` filter. Delegates to `Domain.Action.UI.SpecialLocation`.
        *   **Special Location "Warrior" Operations (`API.Action.UI.SpecialLocationWarrior`)**: Allows authorized personnel ("Warriors") to list special locations by category (`GET /ui/specialLocation/list/category`), and get/update operational info for other drivers at these locations (`GET /ui/getInfo/specialLocWarrior`, `POST /ui/updateInfo/specialLocWarrior`). Delegates to `Domain.Action.UI.SpecialLocationWarrior`.
        *   **Driver SDK Token for Payment Tokenization (`API.Action.UI.Tokenization`)**: Allows authenticated drivers (`GET /ui/driver/sdkToken`) to obtain an SDK token for a specified tokenization `service` with a given `expiry`. Used by client to securely tokenize payment details. Delegates to `Domain.Action.UI.Tokenization`.
        *   **WMB/Fleet Operations & Managed Trips (`API.Action.UI.WMB`)**: Allows drivers in fleet/managed operations to: list fleet badges, discover/get details for available WMB routes, manage WMB trips (QR start, active trip, list, start/end), handle trip-related requests/alerts, consent to fleet association, and get fleet configurations. Delegates to `Domain.Action.UI.WMB`.
        *   **Driver Call Management (`API.UI.Call`)**: Enables drivers to initiate masked calls to customers (for a `rideId`), get call status, and supports telephony provider (Exotel, Twilio) webhooks and IVR interactions for number lookup and status updates. Includes SDK token generation for Twilio. Delegates to `Domain.Action.UI.Call`.
        *   **Driver Call Event Logging (`API.UI.CallEvent`)**: Allows authenticated drivers/app (`POST /ui/callEvent` with `CallEventReq`) to log specific granular events related to calls (e.g., answered, hangup, DTMF input). Delegates to `Domain.Action.UI.CallEvent`.
        *   **Organization Administrator Profile Management (`API.UI.OrgAdmin`)**: Provides profile management (get/update) for authenticated Organization Administrators (`GET /ui/orgAdmin/profile`, `POST /ui/orgAdmin/profile` using `AdminTokenAuth`). Delegates to `Domain.Action.UI.OrgAdmin`.
        *   **Driver Submits Feedback for Ride (`API.UI.Rating`)**: Allows authenticated drivers (`POST /ui/feedback/rateRide` with `CallBAPInternal.FeedbackReq`) to submit ratings and comments for completed rides. Delegates to `Domain.Action.UI.Rating`.
        *   **Driver Cancellation Reasons Retrieval (`API.UI.CancellationReason`)**: Allows authenticated drivers (`GET /ui/cancellationReason/list`) to fetch a list of standardized reasons for cancelling a ride. Delegates to `Domain.Action.UI.CancellationReason`.
        *   **Driver WhatsApp Opt-in/Opt-out (`API.UI.Whatsapp`)**: Allows authenticated drivers (`POST /ui/whatsapp/opt` with `OptAPIRequest`) to manage their preferences for receiving WhatsApp communications. Delegates to `Domain.Action.UI.Whatsapp`.
        *   **Driver Fetches Associated Transporter/Merchant Details (`API.UI.Transporter`)**: Allows authenticated drivers (`GET /ui/transporter`) to fetch details about their associated transporter/merchant. Delegates to `Domain.Action.UI.Transporter`.
    *   **Handling Beckn BPP Requests (via `API.Beckn.hs` aggregator)**:
        *   // ... (all previously detailed Beckn BPP flows) ...
    *   Processing driver location updates (via `location-updates` library).
    *   Integrating with payment systems for driver earnings/payouts.
    *   Exposing metrics via Prometheus.
    *   Utilizing shared platform services for configuration (CAC), specialized geographic zones, webhooks, and scheduling.

// ... (Placeholder for other services like kafka-consumers, provider-dashboard etc.) ...