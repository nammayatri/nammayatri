# System Patterns

_This document describes the system architecture, key technical decisions, design patterns in use, component relationships, and critical implementation paths._

## Overall Architecture (Inferred)
The NammaYatri platform employs a microservices or service-oriented architecture, deeply integrated with Beckn and built on Haskell/Nix.

Key architectural characteristics seem to include:
- **Central Rider Application (`rider-app`)**: The `rider-app` service (located in `Backend/app/rider-platform/rider-app/Main/`) is a core backend component. It orchestrates the entire rider experience. Its API, built with Servant and defined via Namma DSL, is modularized:
    - **Key Flows & API Modules**:
        - **User Registration & Authentication (`API.UI.Registration`)**: Handles user sign-up and login. 
            - **Initiation**: Supports initial authentication via an identifier (e.g., phone number in `DRegistration.AuthReq`) which triggers OTP generation and dispatch. A `RegistrationToken` (e.g., `SR.RegistrationToken`) is created and stored (DB/Redis) to track the attempt, holding the OTP, user details, and expiry. This phase also processes client version/device headers (`x-bundle-version`, `x-device`, etc.) for context.
            - **Signature Authentication**: An alternative path (`/v2/auth/signature`) allows request authentication via a signature in the `x-sdk-authorization` header, likely for trusted SDK clients.
            - **Verification**: The user submits the OTP against the `authId` of the `RegistrationToken`. The system validates the OTP and token expiry. Upon success, a session token (e.g., JWT via Passetto, managed by `Tools.Auth`) is generated and returned in `DRegistration.AuthVerifyRes`.
            - **Session Management**: Subsequent API calls are authenticated using this session token (`TokenAuth`).
            - **OTP Resend**: Functionality to resend OTP for a given `authId`.
            - **Logout**: Securely invalidates the user's session token.
            - **Domain Logic**: All core business logic (OTP generation/validation, token management, user lookup/creation, session handling) is delegated to `Domain.Action.UI.Registration`.
        - **Saved Location Management (`API.UI.SavedReqLocation`)**: Facilitates user management of frequently used locations.
            - **Endpoints**: `POST /savedLocation` (create), `GET /savedLocation/list` (list), `DELETE /savedLocation/{tag}` (delete).
            - **Authentication**: All endpoints are secured with `TokenAuth`.
            - **Workflow**: API handlers in `API.UI.SavedReqLocation` receive requests and delegate to `Domain.Action.UI.SavedReqLocation`.
            - **Domain Logic**: `Domain.Action.UI.SavedReqLocation` performs business logic, including:
                - Validation (e.g., tag uniqueness per user, non-empty tag).
                - Construction of `SavedReqLocation` domain objects.
                - Interaction with the storage layer for persistence.
            - **Storage Interaction**: Uses functions from `Storage.Queries.SavedReqLocation` (e.g., `findAllByRiderIdAndTag`, `create`, `findAllByRiderId`, `deleteByRiderIdAndTag`) to interact with the database. Read operations like listing saved locations may use a read-replica.
        - **Ride Search (Standard & Multimodal) (`API.UI.Search`)**: Handles requests for rides via `POST /v2/rideSearch` (standard) and `POST /v2/multimodalSearch`. 
            - **Common Initiation**: Both endpoints require `TokenAuth` and process client/device context from headers. A core `SearchRequest` entity is created by `Domain.Action.UI.Search.search` to track the overall search, storing `searchId`, origin/destination, user details, timestamps, etc. User-specific rate limiting (via Redis) is applied before processing.
            - **Standard Ride Search (`/v2/rideSearch`)**: 
                - Implements conditional logic (if `merchant.enableForMultipleSearchIssue` is true) to detect and attempt cancellation of prior active searches or bookings by the same user, involving checks against `Estimate` and `Booking` states and potentially calling `Select.cancelSearch'` or `Domain.Action.UI.Cancel.cancel` (which then calls Beckn BPPs).
                - Asynchronously dispatches Beckn V2 search requests (constructed by `TaxiACL.buildSearchReqV2`) to configured taxi BPPs using `CallBPP.searchV2` and `dSearchRes.gatewayUrl`.
                - Can be configured (via `riderConfig.makeMultiModalSearch`) to also initiate an internal multimodal search in parallel.
                - The API responds quickly with `searchId` and basic route info; detailed quotes are retrieved later via `API.UI.Quote`.
            - **Multimodal Ride Search (`/v2/multimodalSearch`)**: Orchestrates a more complex journey planning process.
                - Fetches user preferences (`DMC.getMultimodalUserPreferences`) and platform configurations (`DRC.RiderConfig`, `DIBC.IntegratedBPPConfig`).
                - May first attempt optimized single-mode public transport routing (`JMU.getSingleModeRouteDetails`) for FRFS domain services.
                - Primarily relies on an external multimodal routing engine (e.g., OTP, via `Kernel.External.MultiModal.Interface.getTransitRoutes`) to obtain route suggestions based on origin, destination, and various preferences.
                - Uses `Lib.JourneyModule` (specifically `JM.init` and `JM.filterTransitRoutes`) to process, validate, and structure the routes from the engine into `Journey.Journey` entities, breaking them into legs and refining details.
                - Conditionally initiates the first valid journey leg if `initateJourney` header is true (`DMC.postMultimodalInitiate`).
                - Integrates with CRIS for Indian Railways subway legs: uses `ExternalBPP.ExternalAPI.CallAPI.buildStations` and `CRISRouteFare.getRouteFare` to fetch fare details and an `sdkToken`, requiring decrypted user IMEI and a generated session ID for the `CRISFareRequest`.
                - Handles scenarios with no public transport routes by generating a default "auto" leg (`mkAutoLeg`).
                - Updates `SearchRequest` status (`updateHasMultimodalSearch`, `updateAllJourneysLoaded`).
            - **Domain Logic**: Core search initiation is in `Domain.Action.UI.Search`; multimodal aspects involve `Domain.Action.UI.MultimodalConfirm`, `Lib.JourneyModule`, and specific external client modules.
        - **Quote Retrieval (`API.UI.Quote`)**: This module is responsible for providing clients with the aggregated results of a ride search.
            - **Endpoint**: `GET /v2/rideSearch/{searchId}/results`. Requires `TokenAuth` and uses `searchId` to identify the search context. Accepts an optional `allowMultiple` query parameter.
            - **Functionality**: It delegates to `Domain.Action.UI.Quote.getQuotes`. This domain function retrieves all corresponding ride offers/quotes (e.g., `Estimate` entities or processed `Journey` data) for the given `searchId` from persistent storage or cache. These offers are BPP responses to the initial Beckn search or the outcomes of multimodal route processing.
            - **Data Processing**: The domain logic may filter, rank, or de-duplicate quotes (e.g., based on `allowMultiple`) and must ensure only currently valid (non-expired) quotes are returned.
            - **Response**: Returns a list of `DQuote.OfferRes` (individual ride offers) within a `DQuote.GetQuotesRes` structure, enabling the user to compare options.
            - **Role in Flow**: Acts as the pull mechanism for clients to fetch results after the asynchronous search initiation.
        - **Ride Selection (`API.UI.Select`)**: This module orchestrates the user's selection of a specific ride quote (`Estimate`) and the subsequent Beckn "select" interaction with the BPP.
            - **Endpoints & Actions**:
                - `POST /v2/estimate/{estimateId}/select` (and `/select2`): User chooses an `estimateId`. The handler acquires a Redis lock (`selectEstimateLockKey` per `personId`) to prevent concurrent selections.
                    - It calls `Domain.Action.UI.Select.select` (or `.select2`) to update internal state and prepare data for the Beckn call.
                    - `ACL.buildSelectReqV2` constructs the Beckn "select" request.
                    - `CallBPP.selectV2` sends this request asynchronously to the BPP's `providerUrl`.
                    - For the primary `/select` endpoint, it returns `DSelect.DSelectResultRes` containing a `selectTtl`. This TTL is calculated based on `BecknConfig` values (`selectTTLSec`, `initTTLSec`, `confirmTTLSec`, `confirmBufferTTLSec`) and whether `autoAssignEnabled` (from the `SearchRequest`) is active, guiding client polling.
                - `GET /v2/estimate/{estimateId}/quotes`: Calls `Domain.Action.UI.Select.selectList` to fetch current quote/selection options.
                - `GET /v2/estimate/{estimateId}/results`: Calls `Domain.Action.UI.Select.selectResult` to allow polling for the BPP's `on_select` response data.
                - `POST /v2/estimate/{estimateId}/cancel`: Handled by `cancelSearchUtil`. Checks if a booking is already active for the parent search. If not, calls `Domain.Action.UI.Cancel.mkDomainCancelSearch`, then conditionally sends a Beckn cancel message to the BPP (`CACL.buildCancelSearchReqV2` and `CallBPP.cancelV2`), and finally calls `Domain.Action.UI.Cancel.cancelSearch` to update internal state.
                - `POST /v2/estimate/{estimateId}/rejectUpgrade`: User rejects a provider upgrade. It first adds a `rejectUpgradeTag` to the user's profile (via `Yudhishthira.fetchNammaTagExpiry` and `QP.updateCustomerTags`) and then calls `cancelSearchUtil` to process the cancellation.
            - **Domain Logic**: Core selection, state management, and cancellation logic is in `Domain.Action.UI.Select` and `Domain.Action.UI.Cancel`.
            - **Integrations**: Redis (locking), `CallBPP` (Beckn calls), `BecknConfig` (TTLs), `Yudhishthira` (tagging).
        - **Ride Confirmation / Beckn Init (`API.UI.Confirm`)**: After selection, the user confirms their intent to book, triggering a Beckn "init" message to the BPP.
        - **Ride Booking Finalization & Management (`API.UI.Booking`)**: After the BPP responds to "init" (typically via an `on_init` callback processed by the BAP), this module handles the final stage of booking. 
            - The `POST /rideBooking/{rideBookingId}` endpoint likely triggers the sending of a Beckn "confirm" message to the BPP to finalize the booking.
            - It manages the lifecycle of the booking (e.g., confirmed, active, completed) in the database.
            - Provides endpoints for polling booking status, retrieving booking history, and in-ride modifications (e.g., adding/editing stops).
            - Business logic is delegated to `Domain.Action.UI.Booking`.
        - **Ride Payment Setup & Management (`API.Action.UI.RidePayment`)**: Manages user payment methods, payment intent creation (setup and payment intents for SCA/3DS flows), and ride-specific payment settings like tips.
        - **Ride Payment Execution & Status (`API.UI.Payment`)**: This module, utilizing a generic API structure from `Lib.Payment.API` (within the `payment` shared lib), handles the creation of payment orders for specific rides and allows for polling their status. The actual interaction with payment gateways is done via `Domain.Action.UI.Payment` which calls the `payment` shared library.
        - **Active Ride Management & Information (`API.UI.Ride`)**: This module provides APIs for riders to interact with and get information about their ongoing or recently completed rides. Key functionalities include:
            - Fetching real-time driver location (`/driver/location`).
            - Getting current ride status (`/status`).
            - Requesting edits to ride destination (`/edit/location`).
            - Retrieving ride-related media like delivery images or driver photos.
            - Logic is delegated to `Domain.Action.UI.Ride` and `SharedLogic.Ride`.
        - **Post-Ride Feedback & Driver Information (`API.UI.Rating`)**: This module allows riders to submit ratings for their rides and to view driver profiles ("Know Your Driver"). 
            - Ratings are processed by `Domain.Action.UI.Feedback` and conditionally sent as a Beckn "rating" message to the BPP (if the provider is a "ValueAddNP").
            - Driver profile requests are also handled by `Domain.Action.UI.Feedback`.
        - **Detailed Feedback Submission (`API.UI.FeedbackForm`)**: This module provides APIs for fetching a structured feedback form (which can be conditional on an initial rating) and for submitting the detailed feedback. Logic is delegated to `Domain.Action.UI.FeedbackForm`.
        - **Invoice Retrieval (`API.Action.UI.Invoice` as part of `API.UI`)**: This module provides an endpoint for authenticated users to retrieve their ride invoices for a specified date range. The logic is delegated to `Domain.Action.UI.Invoice` to gather necessary ride and payment data.
        - **Price Breakup Retrieval (`API.Action.UI.PriceBreakup` as part of `API.UI`)**: This module provides an endpoint for authenticated users to retrieve a detailed fare breakdown for a specific booking. The logic is delegated to `Domain.Action.UI.PriceBreakup` to gather and structure this information.
        - **User Profile Management (`API.UI.Profile`)**: This module provides API endpoints for authenticated users to retrieve and update their personal profile information (name, contacts, preferences) as well as manage emergency settings and default emergency contacts. The core logic for these operations is delegated to `Domain.Action.UI.Profile`. It also processes various client version and context headers, suggesting adaptable profile data presentation or handling.
        - **SOS / Emergency Alert Management (`API.Action.UI.Sos` and `API.UI.Sos` as part of `API.UI`)**: These modules provide a comprehensive system for handling emergency SOS events. 
            - `API.Action.UI.Sos` handles core SOS logic: creating alerts, fetching details, updating status, marking rides safe, and initiating calls to police.
            - `API.UI.Sos` specifically handles the upload of media (e.g., video) associated with an SOS event.
            - An unauthenticated endpoint in `API.Action.UI.Sos` receives IVR call outcomes.
            - All logic is delegated to `Domain.Action.UI.Sos`.
        - **Issue Management / Support (`API.UI.Issue`)**: This module orchestrates rider-initiated issue and grievance management. 
            - **Shared Component Utilization**: It acts as a frontend to a shared `IssueManagement` library/component, using an adapter pattern (`customerIssueHandle`) to provide `rider-app` specific data access (casting `Person`, `Ride`, `Booking` entities) and action implementations (like calling `Tools.Ticket` for external ticketing or `SharedLogic.CallBPPInternal` for internal issue reporting).
            - **Beckn IGM Integration**: A core responsibility is managing Beckn IGM flows. For relevant issues, it constructs Beckn IGM `issue` payloads (using `Beckn.ACL.IGM.Issue.buildIssueReq`) with proper context (booking details, validated subcategories per domain) and sends them asynchronously to BPPs (via `SharedLogic.CallIGMBPP.issue`). It also handles updates to these IGM issues (e.g., when a user closes an issue with feedback) and provides a mechanism to poll BPPs for `issue_status` updates (via `SharedLogic.CallIGMBPP.issueStatus`).
            - **Local State**: It manages local representations of IGM issues (`DIGM.IGMIssue`) and general issue reports (`IssueReport`) in its database.
            - **Dynamic Forms**: Supports dynamic issue reporting forms by providing APIs to fetch issue categories and cascading options.
        - **Direct Support Requests (`API.UI.Support`)**: This module offers more direct channels for users to request assistance, complementing the formal `API.UI.Issue` (IGM) system.
            - **Endpoints**: Provides authenticated endpoints for general issue submission (`POST /v2/support/sendIssue` with `DSupport.SendIssueReq`), requesting a callback from support (`POST /v2/support/callbackRequest`), and submitting specific safety check related queries (`POST /v2/support/safetyCheckSupport` with `DSupport.SafetyCheckSupportReq`).
            - **Functionality**: The API handlers delegate to corresponding functions in `Domain.Action.UI.Support` (e.g., `DSupport.sendIssue`, `DSupport.callbackRequest`, `DSupport.safetyCheckSupport`). These domain actions are responsible for processing the requests, which might involve creating internal support tickets, logging the request, or notifying the appropriate support team.
        - **Ride Cancellation (`API.UI.Cancel`)**: This module handles the cancellation of a ride booking.
        - **FCM Push Notification Triggering (`API.Action.UI.TriggerFCM` as part of `API.UI`)**: This module includes a dedicated mechanism to send targeted FCM push notifications, enhancing user engagement and information delivery.
        - **Beckn Callback Handling (`API.Beckn` and `API.BecknAPIV2`)**: These modules define the secure (via `SignatureAuth` for the MOBILITY domain) entry points for `rider-app` to receive all asynchronous `on_action` callbacks from BPPs or the Beckn Gateway (e.g., `on_search`, `on_select`, `on_init`, `on_confirm`, `on_status`, `on_track`, `on_cancel`, `on_update`). 
            - **Routing**: They act as routers, delegating the processing of each specific `on_action` message to a dedicated handler module (e.g., `API.Beckn.OnSearch.handler`, `API.Beckn.OnInit.handler`). This modular approach is key to managing the complexity of the various Beckn interaction stages.
            - **Role**: This is fundamental to `rider-app`'s BAP functionality, enabling it to complete the asynchronous Beckn flows initiated by rider actions.
            - **Beckn `on_update` Callback Processing (`API.Beckn.OnUpdate`)**: This module handles the versatile `on_update` callbacks from BPPs, which convey various real-time changes to an active booking or ride.
                - **Endpoint**: Part of the aggregated Beckn callback API (e.g., `/cab/v1/on_update`), secured by `SignatureAuth`.
                - **Functionality**: 
                    - **Validation & Transformation**: Extracts `transactionId` and `messageId`. Transforms the incoming Beckn `OnUpdateReqV2` payload into a rich internal domain sum type (`DOnUpdate.DOnUpdateReq` then `DOnUpdate.ValidatedOnUpdateReq`) using `ACL.buildOnUpdateReqV2`. This sum type covers numerous specific update events (e.g., ride assigned, started, completed, BPP cancelled, fare changes, new messages, destination edits, safety alerts).
                    - **Idempotency**: Uses two-stage Redis locks (based on `messageId`) for processing.
                    - **Request Validation**: Performs domain-level validation of the specific update type using `DOnUpdate.validateRequest`.
                    - **Logging**: Asynchronously pushes raw callback logs via `TransactionLogs.pushLogs`.
                    - **Update Application**: Asynchronously processes the specific validated update via `Domain.Action.Beckn.OnUpdate.onUpdate`. This core domain action uses pattern matching on the type of `ValidatedOnUpdateReq` to apply the corresponding changes to local `Booking` and/or `Ride` entities and may trigger further actions (e.g., user notifications, payment adjustments).
                - **Response to BPP**: Sends a standard Beckn `Ack` upon successful reception.
        - `API.UI` (other modules): Handles primary interactions for the rider-facing frontend.
        - `API.Beckn` & `API.BecknAPIV2`: Manages Beckn protocol communications specific to rider scenarios.
        - `API.Dashboard`: Serves data for rider dashboards.
        - `API.IGM`: Provides endpoints for Issue/Grievance Management.
        - `API.FRFS`: Deals with Fares, Rules, Fees, or similar parameters.
        - `API.Internal`: For internal operational/administrative endpoints.
        - Direct Juspay Webhook Endpoints: Processes payment and payout status updates from Juspay.
    It interacts with its own PostgreSQL database (via Beam) and integrates heavily with numerous shared libraries.
- **Separation of Concerns**: Modular services and shared libraries.
- **Beckn Protocol as a Core Tenet**: Standardized via `beckn-spec` (definitions) and enacted via `beckn-services` (client implementations), with `rider-app` being a key consumer through its `API.Beckn` module.
- **Rule Engine / Policy Hub (`yudhishthira` library)**: Central engine for evaluating complex business rules and policies, used by `rider-app`.
- **Third-Party Service Integration (`external` library)**: Centralizes interactions with external third-party services, also used by `rider-app`.
- **Metrics Collection & Monitoring (`sessionizer-metrics` library)**: Provides standardized metrics collection for Prometheus, utilized by `rider-app`.
- **Inbound Webhook Processing (`webhook` library)**: General infrastructure for receiving webhooks (while `rider-app` also handles some specific ones like Juspay).
- **Event-Driven Architecture (EDA)**: 
    - **Message Production (`producer` library & `producer-exe`)**: Generates messages based on various triggers, including events from `rider-app`.
    - **Message Consumption (`kafka-consumers` service)**: Consumes messages from Kafka for asynchronous processing.
- **Real-time Location Processing**: Via `location-updates` library.
- **Geographically-Defined Operational Logic (`special-zone` library)**: Manages geo-fenced zones.
- **Centralized Payment Processing (`payment` library)**: Handles financial transactions.
- **Automated Task Scheduling (`scheduler` library)**: Manages and executes background tasks.
- **Foundational Utilities (`utils` library)**: Provides common helper functions.
- **Haskell-based Backend**: Predominantly Haskell, built with Nix.
- **DSL-Driven Development (Namma DSL)**: `alchemist` for code generation (extensively used by `rider-app`).
- **Reusable Shared Services (`shared-services` library)**.
- **Service Templating (`example-service`)**.
- **Comprehensive Testing Strategy**.

## Component Relationships (High-Level)
- **Frontend Applications** interact with **Backend Services**, primarily `rider-app` via its modular APIs. The registration flow starts with the frontend sending auth requests (standard or signed) to `API.UI.Registration`. The search flow starts with the frontend sending search requests to `API.UI.Search`. The quote retrieval flow involves the frontend polling `API.UI.Quote` after initiating a search. The issue management flow is initiated via `API.UI.Issue`.
- **Backend Services and Libraries** communicate internally and externally.
    - `rider-app` acts as an orchestrator. Its `API.UI.Registration` module calls `Domain.Action.UI.Registration` to handle the core logic of OTP generation, `RegistrationToken` management (creation, storage in DB/Redis, validation), OTP validation against the token, session creation (including Passetto token generation), and OTP dispatch (likely via `external` library or direct SMS gateway integration).
    - Its `API.UI.Search` module:
        - Initializes a `SearchRequest` via `Domain.Action.UI.Search.search`.
        - For standard searches, uses `TaxiACL` and `CallBPP.searchV2` for asynchronous Beckn calls.
        - For multimodal searches, it coordinates with `Kernel.External.MultiModal.Interface` (external router), `Lib.JourneyModule` (local processing), `ExternalBPP.ExternalAPI.Subway.CRIS.RouteFare` (CRIS integration), and `Domain.Action.UI.MultimodalConfirm`.
        - Uses Redis (via `Kernel.Storage.Hedis.slidingWindowLimiter`) for rate limiting.
    - Its various API modules triggering logic that calls out to: 
        - `beckn-services` (via `API.Beckn`).
        - `payment` for payment processing.
        - `yudhishthira` for rule evaluation.
        - `special-zone` for geo-fenced rules.
        - `external` for 3rd party communications.
        - `producer` to send messages for asynchronous tasks.
        - Its own database for persistent rider and ride state.
    - The `producer` library/executable is invoked by services like `rider-app`.
    - External services send asynchronous updates to the platform via endpoints provided by the `webhook` library or directly to `rider-app` (e.g., Juspay).
    - Its SOS-related API modules (`API.Action.UI.Sos`, `API.UI.Sos`):
        - Handle client requests for initiating SOS alerts, managing their lifecycle, uploading media, and triggering emergency actions.
        - Receive external updates from IVR systems regarding emergency call statuses.
        - Delegate all core logic to `Domain.Action.UI.Sos` for processing.
    - Its `API.UI.Quote` module:
        - Receives requests for quotes for a specific `searchId`.
        - Delegates to `Domain.Action.UI.Quote.getQuotes` to fetch, filter, rank (if applicable), and structure the quote data from the database/cache where BPP responses and processed multimodal journey options are stored.
    - Its `API.UI.Issue` module:
        - Uses the `customerIssueHandle` to interface with `rider-app`'s data (e.g., `Storage.Queries.*` for `Person`, `Ride`, `Booking`) and specialized tools (`Tools.Ticket`, `SharedLogic.CallBPPInternal`).
        - Leverages the generic `IssueManagement.Common.UI.Issue` actions for core CRUD operations on issues.
        - For Beckn IGM, it uses `Beckn.ACL.IGM.*` to build messages and `SharedLogic.CallIGMBPP` to communicate with BPPs. Local IGM state is stored in `DIGM.IGMIssue` tables.
        - Interacts with cached queries for `IssueCategory` (`QIC`) and `IssueOption` (`QIO`).
    - Its `API.Beckn` and `API.BecknAPIV2` modules serve as the primary ingress points for asynchronous Beckn `on_action` callbacks from BPPs/Gateway.
        - These modules verify the incoming request's signature (`SignatureAuth`).
        - They then route the validated callback to the appropriate specialized handler (e.g., `API.Beckn.OnSearch.handler`, `API.Beckn.OnInit.handler`). These handlers, in turn, invoke domain actions to process the BPP's response, update database state (e.g., `Estimate`, `Booking`, `Ride` entities), and potentially trigger further actions like push notifications to the rider.
    - Its `API.Beckn.OnUpdate` module:
        - Receives and validates diverse `on_update` callbacks from BPPs.
        - Transforms these into specific, typed domain update requests.
        - Delegates to `Domain.Action.Beckn.OnUpdate.onUpdate` for processing, which involves updating local `Booking`/`Ride` state based on the update type and potentially triggering subsequent actions.
- **Specialized Services**: `rider-platform` (containing `rider-app`, `rider-app-drainer`, `public-transport-rider-platform`), `provider-platform`, `dashboards`, `kafka-consumers`, `beckn-gateway`.
- **Shared Libraries (`Backend/lib/`)**: `beckn-spec`, `beckn-services`, `shared-services`, `location-updates`, `special-zone`, `payment`, `scheduler`, `utils`, `yudhishthira`, `external`, `sessionizer-metrics`, `webhook`, `producer`, `mobility-core`, `haskell-cac`.
- **Monitoring Infrastructure**: Prometheus scrapes metrics from services like `rider-app`.
- **Developer & Operational Tools**: `beckn-cli`, `alchemist`.
- **Mocking Infrastructure**: `mocks/google`, `mocks/rider-platform`, `mock-registry`.
- **External Integrations**: OSRM, AWS (SES, SNS), Passetto, Kafka, Beckn Network participants, map/geocoding services, Payment Gateways (Juspay providing webhooks to `rider-app`), Slack, Chat AI services, CAC, GraphQL APIs.

## Key Technical Decisions (from `flake.nix` and structure)
- **Nix for Reproducibility**.
- **Haskell for Backend Robustness**.
- **PureScript for Frontend**.
- **Deep Adoption of Beckn Protocol**.
- **Model-Driven Development with Namma DSL** (central to `rider-app` and its API modules).
- **Orchestration of Rider Services in `rider-app`** with modular API design.
- **Comprehensive Event-Driven Architecture**.
- **Standardized Inbound Webhook Handling** (both generic via `webhook` lib and specific in `rider-app`).
- **Comprehensive Metrics for Monitoring**.
- **Centralized Management of External Service Integrations**.
- **Centralized Rule Engine**.
- **Provision of a Rich Common Utilities Layer**.
- **Centralization of Common Services**.
- **Dedicated Payment Processing Layer**.
- **Asynchronous Processing with Kafka** and **Scheduled Task Execution**.
- **Dedicated Location Processing Layer**.
- **Geofencing and Zone Management**.
- **Standardized Service Creation**.
- **Emphasis on Testability**.
- **Secure and Flexible User Authentication**: Implemented in `rider-app` with a two-step OTP process (initiate/verify using `RegistrationToken` for state), support for signature-based authentication for specific clients, and session token (`TokenAuth` via Passetto) management. Client context (versions, device) is captured.
- **Phased Ride Booking (Beckn Select -> Init -> Confirm)**: `rider-app` implements the Beckn booking flow, with distinct API modules for selection (`API.UI.Select`), initial confirmation/initiation (`API.UI.Confirm`), and final booking/management (`API.UI.Booking`).
- **Secure Payment Method Management and Intent Creation**: Handled by `rider-app` (`API.Action.UI.RidePayment`).
- **Ride Payment Order Execution**: `rider-app` (`API.UI.Payment`) initiates final payment orders for rides, building on the payment setup and leveraging the generic `payment` library.
- **In-Ride Information and Control**: `rider-app` (`API.UI.Ride`) provides riders with real-time tracking, status, and ride modification capabilities.
- **Post-Ride Feedback Loop**: `rider-app` (`API.UI.Rating`, `API.UI.FeedbackForm`) enables users to provide ratings, detailed structured feedback, and view driver details, fostering accountability and trust, and providing valuable data for service improvement.
- **Access to Financial Records**: `rider-app` (`API.Action.UI.Invoice`, `API.Action.UI.PriceBreakup`) provides users with access to their ride invoices and detailed fare breakdowns for transparency and record-keeping.
- **Comprehensive User Profile Management**: `rider-app` (`API.UI.Profile`) allows users to manage personal details and critical safety settings like emergency contacts.
- **Robust SOS and Emergency Handling**: `rider-app` (via `API.Action.UI.Sos` and `API.UI.Sos`) provides a multi-faceted SOS system, including media uploads and IVR integration for enhanced rider safety.
- **Sophisticated Ride Search Capabilities**: `rider-app` (`API.UI.Search`) supports both standard Beckn-based taxi searches (asynchronous) and complex multimodal journey planning. Multimodal search integrates external routing engines, specific public transport provider APIs (e.g., CRIS), and local journey processing logic (`Lib.JourneyModule`). Both flows incorporate rate limiting and client context handling.
- **Asynchronous Quote Aggregation and Retrieval**: The platform supports asynchronous fetching of quotes from multiple BPPs (initiated by `API.UI.Search`) and provides a polling mechanism (`API.UI.Quote`) for the client to retrieve these aggregated results. This ensures the initial search call is responsive.
- **Shared Issue Management Framework with Beckn IGM Integration**: The platform uses a common `IssueManagement` component. `rider-app` (`API.UI.Issue`) adapts this for its context and integrates deeply with Beckn IGM for standardized grievance redressal with BPPs, including validation of IGM subcategories and asynchronous communication.
- **Proactive User Communication via Push Notifications**: The `rider-app` (via `API.Action.UI.TriggerFCM`) includes a dedicated mechanism to send targeted FCM push notifications, enhancing user engagement and information delivery.
- **Secure and Modular Beckn Callback Processing**: `rider-app` implements dedicated, signature-authenticated API endpoints (`API.Beckn`, `API.BecknAPIV2`) to receive all Beckn `on_action` callbacks, with each action delegated to a specific handler module for organized processing. This is essential for its BAP role in managing asynchronous Beckn flows.
- **Beckn `on_select` Callback Processing (`API.Beckn.OnSelect`)**: This module handles the `on_select` responses from BPPs. This callback is the BPP's confirmation of their offer in response to the BAP's "select" message.
    - **Endpoint**: Part of the aggregated Beckn callback API (e.g., `/cab/v1/on_select`), secured by `SignatureAuth`.
    - **Functionality**: 
        - **Validation**: Extracts `transactionId` and `messageId`. Validates the callback context against the original `SearchRequest` and selected `Estimate`.
        - **Data Transformation**: Transforms the incoming Beckn `OnSelectReqV2` (BPP's confirmed offer) into an internal domain request (`DOnSelect.DOnSelectReq`) using `ACL.buildOnSelectReqV2` (from `Beckn.ACL.OnSelect`).
        - **Idempotency**: Uses two-stage Redis locks (based on `messageId`) to ensure robust and idempotent processing.
        - **Offer Validation**: Performs domain-level validation of the BPP's confirmed offer using `Domain.Action.Beckn.OnSelect.validateRequest`. This step compares the `on_select` details against the originally selected `Estimate` to check for discrepancies in price, terms, etc.
        - **Logging**: Asynchronously pushes raw callback logs (e.g., for ONDC) via `TransactionLogs.pushLogs`.
        - **Quote Update**: Asynchronously processes the validated offer via `Domain.Action.Beckn.OnSelect.onSelect`. This core domain action updates the corresponding `Estimate` entity in the database with the BPP-confirmed details (final price, terms, fulfillment info). This effectively makes the selected quote ready for the next step by the user: the "init" call (confirm intent to book).
    - **Response to BPP**: Sends a standard Beckn `Ack` upon successful reception and initial validation.
- **Handling Diverse Real-time Ride Updates (Beckn `on_update`)**: The platform supports a wide array of BPP-initiated updates to active rides via the `on_update` callback, using a sum type in the domain layer to manage the varied logic, ensuring data consistency and timely information for the rider.
- **In-App Call Management (`API.UI.Call`)**: This module is responsible for facilitating voice communication between riders and drivers, typically using an external telephony provider (e.g., Exotel) for call connection and number masking.
    - **Backend-Initiated Calls**: Provides an endpoint (`POST /v2/ride/{rideId}/call/driver`) for riders to initiate a call to the driver. `rider-app` then instructs the telephony provider to connect the call. It also offers an endpoint (`GET /v2/ride/{rideId}/call/{callId}/status`) to poll for the status of such calls and a callback endpoint (`POST /v2/ride/call/statusCallback`) for the telephony provider to post asynchronous status updates.
    - **Frontend/IVR-Driven Interactions**: Exposes endpoints (e.g., `GET /v2/exotel/call/driver/number`, `GET /v2/exotel/call/alternate/driver/number`) that an external telephony/IVR system (like Exotel) can query to retrieve driver phone numbers (likely masked) to establish calls. It also has a callback (`GET /v2/exotel/call/statusCallback`) for this external system to report final call statuses directly.
    - **Functionality**: Delegates core logic to `Domain.Action.UI.Call`, which handles interactions with the telephony service, manages `CallStatus` records in the database, and implements number masking logic.
- **Secure Rider-Driver Communication**: `rider-app` (`API.UI.Call`) facilitates in-app calling with number masking via an external telephony provider, supporting both backend-initiated calls and interactions driven by external IVR/telephony systems.
- **Detailed Call Event Logging for Analytics and Debugging**: Beyond general call status, `rider-app` (`API.UI.CallEvent`) supports logging of specific in-call events, providing richer data for operational analysis and troubleshooting communication issues.
- **Rentals/Intercity Fare Cache Retrieval (`API.UI.RentalsIntercityCache`)**: This module provides an API endpoint (`POST /v2/fetchFareCache`) for clients to fetch cached fare information for rental or intercity services.
    - **Purpose**: To optimize performance by serving potentially complex or frequently requested fare estimates from a cache, reducing latency for the user.
    - **Workflow**: The API receives a `RIC.FareCacheReq` with service-specific parameters. It delegates to `Domain.Action.UI.RentalsIntercityCache.rentalsIntercityCache` which handles the cache lookup logic. If a valid cache entry exists, it's returned; otherwise, the domain action might trigger a live fetch and then cache the result (this cache-miss logic is not explicitly detailed in the API module itself but is a common pattern for such features).
    - **Authentication**: Requires `TokenAuth`.
- **Multimodal Journey Lifecycle Management (`API.Action.UI.MultimodalConfirm`)**: This extensive module, group under `/multimodal/`, manages the full lifecycle of a user-selected multimodal journey (`Journey.Journey`).
    - **Endpoints**: Provides a rich set of authenticated APIs for initiating (`/{journeyId}/initiate`), confirming (`/{journeyId}/confirm`), getting info/status (`.../booking/info`, `.../booking/paymentStatus`, `.../journey/status`), managing payments (`.../payment/updateOrder`), modifying legs (switch, skip, addSkippedLeg, extend, getLegTierOptions, switchTaxi, switchFRFSTier, setStatus), cancelling (`.../journey/{journeyId}/cancel`), updating rider location (`.../{journeyId}/rider/location`), managing journey feedback, and user multimodal preferences.
    - **Functionality**: All API handlers delegate directly to corresponding functions in `Domain.Action.UI.MultimodalConfirm`. This domain module is responsible for the complex orchestration of these actions, including: managing the state of `Journey.Journey` and individual `Lib.JourneyLeg.Types.JourneyLegStatus`, potentially making Beckn calls (init, confirm, update, cancel) for individual legs to different BPPs, handling payments across legs, and applying user preferences to journey modifications.
    - **Supporting Endpoints**: Also includes helpers like `POST /multimodal/transitOptions/lite` and `GET /publicTransport/data` for broader multimodal context.
- **CRIS Specific Interactions (`API.Action.UI.CRIS` as part of `API.UI`)**: This module handles deeper, specialized interactions with the Centre for Railway Information Systems (CRIS), primarily for metro/subway services, beyond the fare/SDK token retrieval during multimodal search.
    - **Endpoints**: Provides authenticated APIs for fetching data needed by a client-side CRIS SDK (`POST /v2/cris/getSDKData`), requesting CRIS to generate an OTP (`GET /v2/cris/otp/generation` - likely for user authentication with CRIS or transaction authorization), and managing device registration with CRIS (`POST /v2/cris/change/device`).
    - **Purpose**: To enable a more integrated user experience for CRIS-based services, such as in-app CRIS account actions or secure transaction preparations, reducing the need for users to switch to separate CRIS apps.
    - **Functionality**: All API handlers delegate directly to corresponding functions in `Domain.Action.UI.CRIS`, which would encapsulate the specific logic for communicating with CRIS APIs.
- **Deep Integration with Transit Systems (CRIS)**: Beyond basic multimodal routing, `rider-app` (`API.Action.UI.CRIS`) provides APIs for more specific CRIS interactions like OTP generation and device management, supporting a richer integration for users of CRIS-based services.
- **Integrated Public Transport Ticketing**: `rider-app` (`API.Action.UI.TicketService`) offers a full lifecycle for discovering, booking, paying for, and managing public transport tickets, integrated with the platform's payment services.
    - **Endpoints**: Grouped under `/v2/ticket/`, it provides authenticated APIs for:
        - Discovering ticketable places (`/places`) and services available at those places (`/places/{placeId}/services`, filterable by date).
        - Booking tickets for a service at a place (`/places/{placeId}/book`), which initiates a payment order via the platform's standard payment mechanism.
        - Managing booked tickets: listing (`/bookings` with filters), fetching details (`/bookings/{ticketBookingShortId}/details`), getting status (`/bookings/{ticketBookingShortId}/status`), and verifying tickets (`/bookings/{personServiceId}/{ticketServiceShortId}/verify`).
        - Cancelling bookings (`/booking/cancel`) or entire services (`/service/cancel`), and updating seat information (`/bookings/update/seats`).
    - **Functionality**: All handlers delegate to corresponding functions in `Domain.Action.UI.TicketService`. This domain module orchestrates interactions with underlying ticketing providers (which could be specific BPPs or direct integrations) and the internal payment system.
- **FRFS Public Transport Ticket Service (`API.Action.UI.FRFSTicketService` as part of `API.UI`)**: This module provides a comprehensive suite of APIs for discovering, booking, and managing tickets for public transport services that adhere to FRFS (Fare, Route, Fee, Schedule) data models (e.g., buses).
    - **Endpoints**: Grouped under `/v2/frfs/`, it offers authenticated APIs covering the entire ticketing lifecycle: route/station discovery (including autocomplete), service search, quote retrieval, quote confirmation (leading to booking and payment order creation), booking status checks, listing bookings, cancellation (and checking cancellability), ticket verification, and fetching FRFS configurations.
    - **Orchestration**: The API handlers delegate all business logic to `Domain.Action.UI.FRFSTicketService`.
    - **Functionality**: This domain layer is responsible for interacting with FRFS data sources (e.g., GTFS feeds, specific PT operator APIs, or Beckn BPPs providing FRFS data), managing the local state of FRFS ticket bookings, and integrating with the platform's payment system for ticket purchases.
- **Comprehensive FRFS Ticketing Lifecycle Management**: `rider-app` (via `API.Action.UI.FRFSTicketService`) provides a full suite of functionalities for FRFS-based public transport, from discovery and booking to post-booking management and cancellation, integrating with payment systems.
- **Kapture CRM/Support Integration (`API.Action.UI.TicketKapture` as part of `API.UI`)**: This module facilitates integration with an external Kapture CRM or customer support platform.
    - **Endpoint**: Provides an authenticated API endpoint (`POST /v2/kaptureCustomerLogin`) for `rider-app` users.
    - **Purpose**: To establish or link a user's session with the Kapture system, likely enabling a more seamless cross-platform support experience (e.g., viewing Kapture tickets within the app or providing Kapture agents with NammaYatri user context).
    - **Functionality**: The API handler delegates to `Domain.Action.UI.TicketKapture.postKaptureCustomerLogin`. This domain action is responsible for the actual communication with Kapture's APIs to authenticate the user or create a Kapture session based on their NammaYatri identity.
- **Integration with External CRM/Support Systems (Kapture)**: `rider-app` (`API.Action.UI.TicketKapture`) supports linking user sessions with Kapture CRM to streamline customer support interactions.
- **Customer Referral Program with VPA Payouts**: `rider-app` (`API.Action.UI.CustomerReferral`) includes features for managing a referral program, from applying codes to handling payouts via VPA (UPI), including VPA verification.
- **Social Media Login & Profile Update (`API.Action.UI.SocialLogin` as part of `API.UI`)**: This module handles user authentication via third-party social media providers and linking social profiles to NammaYatri accounts.
    - **Endpoints**: Provides an unauthenticated `POST /v2/social/login` for social sign-in/sign-up and an authenticated `POST /v2/social/update/profile` for linking/updating social profiles.
    - **Functionality**: API handlers delegate to `Domain.Action.UI.SocialLogin`. The domain logic involves:
        - Receiving a social provider token/code from the client.
        - Performing server-to-server validation of this token/code with the respective social media provider (e.g., Google, Facebook).
        - Fetching user details (email, name, social ID) from the social provider upon successful validation.
        - For social login: Checking if a NammaYatri user exists with the social ID or email. If yes, a NammaYatri session is established. If no, a new NammaYatri user account is created using the social profile data, linked to the social ID, and then a session is established.
        - For profile update: Linking the validated social ID to the currently authenticated NammaYatri user's profile and potentially updating profile fields.
    - **Session Management**: Successful social login results in the issuance of NammaYatri session tokens (e.g., JWT via Passetto).
- **Favourite Driver Management (`API.Action.UI.FavouriteDriver` as part of `API.UI`)**: This module allows riders to manage their list of favorite drivers.
    - **Endpoints**: Provides authenticated APIs to `GET /v2/driver/favorites` for listing favorite drivers and `POST /v2/favorites/{driverId}/remove` for removing a driver from the list.
    - **Functionality**: The API handlers delegate to `Domain.Action.UI.FavouriteDriver`. This domain module is responsible for querying the database to retrieve the user-specific list of favorite drivers and for executing the deletion of a favorite driver record. The mechanism for adding a driver to favorites is handled elsewhere in the application (e.g., post-ride feedback).
- **Ride Estimate Breakup Retrieval (`API.Action.UI.EstimateBP` as part of `API.UI`)**: This module provides an API endpoint (`GET /v2/ride/{rideId}/estimateBreakup`) for authenticated users to retrieve a detailed fare estimate breakdown for a specific `Ride` instance.
    - **Purpose**: To offer transparency into the components of a ride's estimated or actual fare (base, distance, time, tolls, taxes, surge, discounts).
    - **Functionality**: The API handler delegates to `Domain.Action.UI.EstimateBP.getRideEstimateBreakup`. This domain action is responsible for fetching the relevant `Ride` details and its associated fare components from the database or calculation logic to construct the detailed breakup.
- **Granular Fare Transparency (Ride-Specific Estimate Breakup)**: Beyond general price breakups for bookings, `rider-app` (`API.Action.UI.EstimateBP`) offers detailed fare component breakdowns for individual ride instances, enhancing cost clarity for users.
- **Nearby Drivers Retrieval (`API.Action.UI.NearbyDrivers` as part of `API.UI`)**: This module provides an API endpoint (`POST /v2/nearbyDrivers`) for authenticated riders to fetch information about currently available drivers in their vicinity.
    - **Purpose**: To enhance user experience by displaying nearby vehicles on a map interface, giving a visual sense of service availability before a formal ride search is initiated.
    - **Functionality**: The API handler receives a `NearbyDriverReq` (containing rider's location, search radius, optional filters) and delegates to `Domain.Action.UI.NearbyDrivers.postNearbyDrivers`. This domain action queries a real-time driver location data store (likely populated by the `location-updates` library and possibly using geospatial indexing in Redis or a database) to find and filter available drivers. The response (`NearbyDriverRes`) contains a list of these drivers.
- **Nearby Buses & Next Vehicle Details (`API.Action.UI.NearbyBuses` as part of `API.UI`)**: This module provides APIs for riders to get real-time information about nearby public transport, specifically buses, and upcoming vehicle details for specific routes/stops.
    - **Endpoints**: Includes `POST /v2/nearbyBusBooking` (for finding nearby buses based on location and criteria in `NearbyBusesRequest`) and `GET /v2/nextVehicleDetails/{routeCode}/{stopCode}` (for ETAs of next vehicles on a route/stop, with optional `vehicleType` filter).
    - **Purpose**: Enhances public transport usability by providing live operational data, moving beyond static schedules.
    - **Functionality**: API handlers delegate to `Domain.Action.UI.NearbyBuses`. This domain module queries real-time public transport data feeds (e.g., GTFS-RT, direct transit agency APIs) to fetch bus locations, route ETAs, and structures the response (e.g., `NearbyBusesResponse`, `Lib.JourneyModule.Utils.UpcomingTripInfo`). The use of `UpcomingTripInfo` suggests this data can feed into or update multimodal journey plans.
    - **Authentication**: Requires `TokenAuth`.
- **Places Information Retrieval (`API.Action.UI.Places` as part of `API.UI`)**: This module provides an API endpoint (`POST /v2/places`) for authenticated riders to search for specific places or Points of Interest (POIs).
    - **Purpose**: To complement general map autocomplete/search by allowing more targeted place discovery based on queries, location context, or categories, aiding in accurate origin/destination selection.
    - **Functionality**: Receives a `PlacesRequest` and delegates to `Domain.Action.UI.Places.postPlaces`. The domain action then queries an external POI database or place search API (e.g., Google Places, Foursquare) and returns structured place information in `PlacesResponse`.
    - **Authentication**: Requires `TokenAuth`.
- **Live Vehicle Tracking on Route (`API.Action.UI.TrackRoute` as part of `API.UI`)**: This module enables riders to track the live movement of public transport vehicles (e.g., buses) on a specific route.
    - **Endpoint**: Provides an authenticated API `GET /v2/track/{routeCode}/vehicles` where `{routeCode}` identifies the public transport route. Optional query parameters like `platformType` and `vehicleType` allow for filtering.
    - **Purpose**: To offer users real-time visibility of vehicles on their chosen route, improving predictability and reducing waiting anxiety.
    - **Functionality**: The API handler delegates to `Domain.Action.UI.TrackRoute.getTrackVehicles`. This domain action queries real-time vehicle location systems or transit data feeds (e.g., GTFS-RT, direct agency APIs), filters by the specified route and other criteria, and returns a `TrackingResp` containing the live locations and potentially other details of vehicles on that route.
    - **Authentication**: Requires `TokenAuth`.
- **Static Route Information Retrieval (`API.UI.Route`)**: This module, directly defined in `src/API/UI/Route.hs` (not Namma DSL generated like many `Action` modules), provides endpoints for fetching static route geometry (polylines) and details (distance, estimated time) from an external mapping service.
    - **Endpoints**: Offers three authenticated `POST` endpoints: `/route` (for general route requests), `/pickup/route` (specifically for the driver-to-rider pickup leg), and `/trip/route` (for the main trip path).
    - **Purpose**: To enable the display of planned routes on maps for various contexts within the app.
    - **Functionality**: The API handlers (`getRoute`, `getPickupRoute`, `getTripRoute`) receive `Maps.GetRoutesReq` or `DRoute.GetPickupRoutesReq` and delegate to corresponding functions in `Domain.Action.UI.Route` (e.g., `DRoute.getRoutes`, `DRoute.getPickupRoutes`, `DRoute.getTripRoutes`). These domain actions then use an abstracted interface (`Kernel.External.Maps.Interface`) to call an external mapping service (like OSRM or Google Maps) and return a `Maps.GetRoutesResp` containing the route details.
    - **Authentication**: Requires `TokenAuth`.
- **Location Serviceability & Intercity Check (`API.UI.Serviceability`)**: This module, defined in `src/API/UI/Serviceability.hs`, provides crucial pre-search validation capabilities.
    - **Endpoints**: Offers authenticated `POST` endpoints under `/serviceability/`: `/origin` and `/destination` (for checking if a `LatLong` is serviceable based on geofencing rules) and `/isInterCity` (to determine if a trip between two points is intercity, likely influencing provider/pricing logic).
    - **Purpose**: To inform users upfront about service availability and trip classification, improving user experience by preventing futile searches.
    - **Functionality**: Origin/destination serviceability checks (`checkOrignServiceability`, `checkDestinationServiceability`) delegate to `Domain.Action.UI.Serviceability.checkServiceability`, which uses `GeofencingConfig` (and a `GeoRestriction` accessor) to validate locations. The intercity check (`checkForIsInterCity`) delegates to `SharedLogic.CallBPPInternal.getIsInterCity`, which may use merchant configurations and BPP-specific rules.
    - **Authentication**: Requires `TokenAuth`.
- **Pre-emptive Serviceability Checks**: `rider-app` (`API.UI.Serviceability`) uses geofencing and potentially BPP/merchant-specific logic to validate origin/destination serviceability and classify trips (e.g., intercity) upfront, guiding users and streamlining the search process.
- **Hotspot Identification and Retrieval (`API.UI.HotSpot`)**: This module, defined in `src/API/UI/HotSpot.hs`, provides an intelligent way to suggest relevant locations (hotspots) to users.
    - **Endpoint**: Provides an authenticated `GET /v2/getHotSpot` endpoint that takes a `Maps.LatLong` (latitude/longitude) in the request body.
    - **Purpose**: To identify and return a ranked list of hotspots near the user's provided location, aiding in pickup/drop-off selection by highlighting areas of significant activity or designated special zones.
    - **Functionality**: The logic, largely contained within the `getHotspot` handler, involves:
        - Fetching merchant-specific `HotSpotConfig` (from `QHotSpotConfig`) which defines parameters like search radius, geohash precisions, activity frequency thresholds, and ranking weights for different event types (manual/auto pickups, saved locations, trip starts/ends, special locations).
        - Using geohashing (`Data.Geohash`) and custom geospatial calculations to determine relevant geohash cells around the input location.
        - Retrieving pre-aggregated `HotSpot` frequency data from Redis (keyed by geohashes).
        - Applying filtering based on minimum frequency and then a precision-based grouping and re-ranking using configured weights.
        - Returning a limited list of `HotSpotInfo` objects along with a `blockRadius`.
    - **Authentication**: Requires `TokenAuth`.

_This document will be updated with more detailed patterns as individual services and libraries are analyzed._

- **Proxying Static Route Generation**: `rider-app` (`API.UI.Route`) acts as a proxy to an external mapping service for generating static route polylines and details for display, with specific endpoints for general, pickup, and trip routes.

// --- Appended details for Frontend State and Event Notification (`API.UI.Frontend`) ---
        - **Frontend State and Event Notification (`API.UI.Frontend`)**: This module, defined in `src/API/UI/Frontend.hs`, provides mechanisms for frontend clients to synchronize with and inform the backend about user state and UI interactions.
            - **Endpoints**: Offers authenticated endpoints under `/frontend/`: `GET /flowStatus` (to query the user's current application flow status, with optional polling/active booking check flags) and `POST /notifyEvent` (for the client to send a `NotifyEventReq` containing a `FrontendEvent` detailing a specific UI interaction).
            - **Purpose**: To enable a more responsive and context-aware UI by allowing the frontend to fetch current user state, and to allow the backend to receive notifications of UI events for analytics, state updates, or triggering further logic.
            - **Functionality**: Handlers delegate to `Domain.Action.UI.Frontend` for processing. `getPersonFlowStatus` assesses and returns the user's current position in backend-defined flows. `notifyEvent` processes and potentially logs or acts upon frontend-originated events.
            - **Authentication**: Requires `TokenAuth`.

- **Frontend-Backend State Awareness**: `rider-app` (`API.UI.Frontend`) supports APIs for querying user flow status and for frontends to notify the backend of UI events, enabling more dynamic UIs and better analytics.

// --- Appended details for Location Edit Result and Confirmation (`API.Action.UI.EditLocation`) ---
        - **Location Edit Result and Confirmation (`API.Action.UI.EditLocation` as part of `API.UI`)**: This Namma DSL generated module manages the rider-facing part of an asynchronous location edit process for an active ride (e.g., destination change).
            - **Endpoints**: Offers authenticated endpoints under `/edit/`: `GET /{bookingUpdateRequestId}/result` for polling the outcome of an edit request (e.g., new fare, BPP approval status) and `POST /result/{bookingUpdateRequestId}/confirm` for the rider to accept the proposed changes.
            - **Purpose**: To provide a clear, two-step confirmation flow for in-ride location modifications that may have implications like fare changes, ensuring user consent.
            - **Functionality**: Handlers delegate to `Domain.Action.UI.EditLocation`. The initial edit intent is likely made via another API (e.g., `API.UI.Ride`), creating a `BookingUpdateRequest`. This entity is then updated by BPP `on_update` callbacks. `getEditResult` fetches this updated request. `postEditResultConfirm` finalizes the user's acceptance, updating the `BookingUpdateRequest` and potentially the main `Booking`/`Ride` entities.
            - **Authentication**: Requires `TokenAuth`.

// --- Appended details for Ride Sharing and Tracking (`API.Action.UI.FollowRide`) (from a previous turn) ---
// ... (ensure this is the detailed version from a previous turn) ...

// --- Appended details for Aadhaar Verification (`API.UI.AadhaarVerification`) (from a previous turn) ---
        - **Aadhaar Verification (eKYC) (`API.UI.AadhaarVerification`)**: This module, defined in `src/API/UI/AadhaarVerification.hs`, handles Aadhaar-based identity verification.
            - **Endpoints**: Provides authenticated `POST` endpoints under `/verifyAadhaar/`: `/generateOtp` (for initiating OTP generation with an Aadhaar number) and `/verifyOtp` (for submitting the received OTP).
            - **Purpose**: To enable a standardized eKYC process for users, potentially for regulatory compliance or to unlock specific platform features.
            - **Functionality**: Handlers delegate to `Domain.Action.UI.AadhaarVerification`. `generateAadhaarOtp` takes an `AadhaarOtpReq`, interacts with an external Aadhaar verification service (likely via `Tools.AadhaarVerification`) to send an OTP. `verifyAadhaarOtp` takes a `VerifyAadhaarOtpReq` (with OTP and transaction ID) and sends it to the external service for validation. Successful verification can update the user's KYC status and store relevant details.
            - **Authentication**: Requires `TokenAuth`.

// --- Existing Key Technical Decisions section ---
// ... (ensure all previous Key Technical Decisions are preserved) ...
- **Aadhaar-based eKYC**: `rider-app` (`API.UI.AadhaarVerification`) supports identity verification via Aadhaar OTP through an external service, facilitating KYC processes.

// --- Appended details for Cancellation Reasons Retrieval (`API.UI.CancellationReason`) (from a previous turn) ---
        - **Cancellation Reasons Retrieval (`API.UI.CancellationReason`)**: This module, defined in `src/API/UI/CancellationReason.hs`, provides a standardized list of cancellation reasons.
            - **Endpoint**: Offers an authenticated `GET /cancellationReason/list` endpoint.
            - **Purpose**: To allow clients to fetch a list of appropriate cancellation reasons based on the current `CancellationStage` of a booking/ride (e.g., before driver assignment, after arrival).
            - **Functionality**: The handler delegates to `Domain.Action.UI.CancellationReason.list`, which retrieves a filtered list of `CancellationReasonAPIEntity` objects from a configured source (database or config file). This standardizes cancellation data for analytics and policy application.
            - **Authentication**: Requires `TokenAuth`.

// --- Existing Key Technical Decisions section ---
// ... (ensure all previous Key Technical Decisions are preserved) ...
- **Contextual Cancellation Reasons**: `rider-app` (`API.UI.CancellationReason`) provides cancellation reasons based on the `CancellationStage` of the ride/booking, allowing for more relevant user choices and better data collection for analytics and policy enforcement.

// --- Appended details for FRFS Ticketing via Partner Organizations (`API.UI.PartnerOrganizationFRFS`) (from a previous turn) ---
        - **FRFS Ticketing via Partner Organizations (`API.UI.PartnerOrganizationFRFS` as part of `API.UI`)**: This module, defined in `src/API/UI/PartnerOrganizationFRFS.hs`, provides a B2B interface for third-party partner organizations to access and offer NammaYatri's FRFS public transport ticketing services.
            - **Endpoints**: Exposes endpoints under `/frfs/partnerOrganization/` (using `PartnerOrganizationAPIKey` for authentication and subject to rate limiting) for partners to: get FRFS configurations, request fares for end-users (involving an upsert of the user's details into NammaYatri via `DPOFRFS.upsertPersonAndGetToken`), and confirm FRFS quotes to make bookings. Additional endpoints under `/frfs/` (e.g., `/shareTicketInfo`, `/auth`, `/authVerify`) allow end-users (or partners on their behalf) to manage aspects of tickets booked through this channel.
            - **Purpose**: To enable B2B integration, allowing partners to resell or incorporate NammaYatri's FRFS ticketing into their own platforms.
            - **Functionality**: Handlers delegate to `Domain.Action.UI.PartnerOrganizationFRFS` which manages partner-specific logic (API key validation, rate limits based on `PartnerOrgConfig`, end-user context creation) and then reuses core FRFS functionalities from `Domain.Action.UI.FRFSTicketService` (like `postFrfsSearchHandler`) to process requests.
            - **Authentication**: Uses a combination of partner API keys for B2B endpoints and potentially `TokenAuth` or booking-specific auth for end-user ticket management endpoints.
// ... (ensure other existing API.UI module descriptions are preserved below this) ...

// --- Existing Key Technical Decisions section ---
// ... (ensure all previous Key Technical Decisions are preserved) ...
- **B2B Integration for FRFS Ticketing**: `rider-app` (`API.UI.PartnerOrganizationFRFS`) offers a dedicated API for partner organizations, with API key authentication and rate limiting, to integrate FRFS ticketing. This involves a mechanism for partners to create/link end-user profiles within NammaYatri for these transactions.

// --- Appended details for User Personal Statistics Retrieval (`API.UI.PersonStats`) (from a previous turn) ---
        - **User Personal Statistics Retrieval (`API.UI.PersonStats`)**: This module, defined in `src/API/UI/PersonStats.hs`, provides an endpoint for authenticated users to fetch their aggregated usage statistics.
            - **Endpoint**: Offers an authenticated `GET /personStats` endpoint.
            - **Purpose**: To allow users to view a summary of their activity on the platform, such as total rides, distance, spending, etc.
            - **Functionality**: The handler delegates to `Domain.Action.UI.PersonStats.getPersonStats`, which queries various data sources (e.g., ride, booking, payment tables) to compute and return these user-specific metrics in a `PersonStatsRes`.
            - **Authentication**: Requires `TokenAuth`.

// --- Existing Key Technical Decisions section ---
// ... (ensure all previous Key Technical Decisions are preserved) ...
- **User Activity Summaries**: `rider-app` (`API.UI.PersonStats`) provides users with access to their personal usage statistics, calculated by aggregating data from various backend tables.

// --- Appended details for WhatsApp Opt-in/Opt-out (`API.UI.Whatsapp`) (from a previous turn) ---
        - **WhatsApp Opt-in/Opt-out Management (`API.UI.Whatsapp`)**: This module, defined in `src/API/UI/Whatsapp.hs`, allows users to manage their preferences for receiving communications via WhatsApp.
            - **Endpoint**: Offers an authenticated `POST /whatsapp/opt` endpoint.
            - **Purpose**: To give users control over WhatsApp notifications and messages from the platform, respecting their communication preferences.
            - **Functionality**: Receives a `Whatsapp.OptAPIRequest` (likely containing an opt-in/out flag). Delegates to `Domain.Action.UI.Whatsapp.whatsAppOptAPI`, which updates the user's preferences in their profile or a dedicated preferences store. This may also involve API calls to a WhatsApp Business API provider to reflect the opt-in/out status if required by the provider.
            - **Authentication**: Requires `TokenAuth`.
// ... (ensure other existing API.UI module descriptions are preserved below this) ...

// --- Existing Key Technical Decisions section ---
// ... (ensure all previous Key Technical Decisions are preserved) ...
- **User-Controlled WhatsApp Communication**: `rider-app` (`API.UI.Whatsapp`) provides an explicit opt-in/opt-out mechanism for WhatsApp communications, respecting user preferences and potentially integrating with WhatsApp Business API providers.

// --- Appended details for User Account Deletion Request (`API.Action.UI.DeletedPerson`) (from a previous turn) ---
        - **User Account Deletion Request (`API.Action.UI.DeletedPerson` as part of `API.UI`)**: This Namma DSL generated module provides an endpoint for authenticated users to request the deletion of their account.
            - **Endpoint**: Offers an authenticated `POST /deleted/person` endpoint.
            - **Purpose**: To enable users to exercise their right to data deletion, in compliance with privacy regulations.
            - **Functionality**: Receives a `DeletedPersonReq` (likely for confirmation/reason). Delegates to `Domain.Action.UI.DeletedPerson.postDeletedPerson`. The domain action then initiates the account deletion process, which involves marking the account for deletion, anonymizing or removing PII from various database tables based on retention policies, invalidating sessions, and logging the action. The actual deletion might be immediate or scheduled.
            - **Authentication**: Requires `TokenAuth`.
// ... (ensure other existing API.UI module descriptions are preserved below this) ...

// --- Existing Key Technical Decisions section ---
// ... (ensure all previous Key Technical Decisions are preserved) ...
- **User Account Deletion Process**: `rider-app` (`API.Action.UI.DeletedPerson`) provides a mechanism for users to request account deletion, which triggers a backend process for data anonymization/removal in line with data privacy policies.

// --- Appended details for Internal Ride Feedback Submission (`API.Internal.Rating`) ---
        - **Internal Ride Feedback Submission (`API.Internal.Rating` as part of `API.Internal`)**: Provides a backend channel for submitting ride ratings and feedback.
            - **Endpoint**: `POST /internal/feedback/rateRide`.
            - **Purpose**: To allow internal systems (e.g., customer support tools) or other backend processes to log ride feedback that doesn't originate from the primary user interface.
            - **Functionality**: Receives a `Domain.FeedbackReq` (same as the UI rating endpoint). Delegates to `Domain.Action.Internal.Rating.rating` for processing and storing the feedback. It uses an optional header token for authentication, distinct from standard user sessions.
            - **Authentication**: Optional header token (specific internal/service token).
// ... (ensure other existing API.Internal module descriptions are preserved below this, if any) ...

// --- Existing Key Technical Decisions section ---
// ... (ensure all previous Key Technical Decisions are preserved) ...
- **Internal Feedback Channel**: `rider-app` (`API.Internal.Rating`) provides a separate, internally authenticated endpoint for submitting ride feedback, enabling comprehensive feedback collection from various sources.