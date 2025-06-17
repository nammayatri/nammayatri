# Product Context

_This document explains why this project exists, the problems it solves, how it should work, and user experience goals._ 

## Core Problem Statement
To create a reliable, efficient, and open platform for connecting users with transportation providers and related services. This involves a `rider-app` (BAP) for rider-facing interactions and a `provider-platform` (containing BPPs like `dynamic-offer-driver-app`) for driver/provider operations. These platforms interoperate via the Beckn protocol and provide distinct UIs for their respective users. The `dynamic-offer-driver-app` specifically enables drivers to onboard, manage their availability, respond to ride offers, execute rides (including selecting standardized cancellation reasons if needed), track earnings and performance, report issues, provide feedback, access learning modules, interact with operational hubs, view demand hotspots, relevant kiosk locations and special operational zones, manage platform messages, see their profile summary, manage subscription plans, and participate in incentive programs. It also provides administrative functionalities. The system also supports internal operational needs, various integrations, all underpinned by a robust, event-driven architecture and shared libraries.

## Key Problems Solved
- **Secure User Onboarding and Access (Rider)**: The `rider-app` (via `API.UI.Registration` and `Domain.Action.UI.Registration`) provides a secure and multi-faceted user authentication system:
    - **Initial Authentication**: Users can initiate authentication using an identifier (e.g., phone number). The system generates and sends an OTP via SMS (or a similar mechanism). A `RegistrationToken` is created server-side to manage the state of this attempt, including the OTP, its expiry, and the user identifier. This flow also supports an alternative signature-based authentication for requests originating from trusted SDKs, providing an additional layer of security.
    - **Client Contextualization**: Information like client application version, bundle version, OS, device type, and IP address are captured during authentication initiation, allowing for tailored responses, analytics, or fraud detection.
    - **Verification**: Users verify their identity by submitting the OTP. The system validates this against the stored `RegistrationToken`. Upon success, a session token (e.g., JWT via Passetto) is generated, establishing an authenticated session.
    - **Session Management**: Authenticated sessions are managed using these tokens, enabling secure access to protected rider functionalities. Invalid or expired tokens are rejected.
    - **OTP Resend**: A mechanism to resend OTPs is provided, likely with rate limiting or other controls to prevent abuse.
    - **Logout**: Users can securely terminate their sessions, which invalidates their current session token.
    This multi-step process with OTPs and robust session management ensures that only legitimate users can access the platform and their personal data, while also providing flexibility for different client types (app vs. SDK).
- **Secure Driver Onboarding and Access (`dynamic-offer-driver-app`)**: To enable drivers to participate in the platform, `dynamic-offer-driver-app` (via `API.UI.Registration` and `Domain.Action.UI.Registration`) provides a secure authentication system for them:
    - **Initial Authentication/Registration**: Drivers initiate login or first-time registration using an identifier (typically phone number) via `POST /ui/auth`. The system generates and sends an OTP.
    - **OTP Verification**: Drivers submit the OTP via `POST /ui/auth/{authId}/verify`. Successful verification grants them a session token (Passetto JWT) and provides context like their `PersonId`, `MerchantId`, and `MerchantOperatingCityId`.
    - **Session Management**: This session token is used for subsequent authenticated API calls from the driver app. Logout functionality (`POST /ui/auth/logout`) invalidates the session.
    - **Client Context**: The authentication process also captures client application details (version, device type) from headers.
    This flow establishes a secure session for drivers to interact with the platform, receive offers, and manage their activities. More detailed onboarding steps (document submission, vehicle registration) are likely handled by separate `DriverOnboarding` modules after this initial authentication.
    - **Comprehensive Driver and Vehicle Vetting (`API.Action.UI.DriverOnboardingV2`)**: After initial authentication, `dynamic-offer-driver-app` facilitates a detailed onboarding process for drivers to ensure compliance, safety, and operational readiness:
        - **Dynamic Onboarding Configuration**: Fetches configurations (`GET /onboarding/configs`) that can tailor the required documents and verification steps (e.g., making selfie, Aadhaar, PAN mandatory, or focusing only on vehicle documents), allowing for flexible onboarding requirements.
        - **Vehicle Registration**: Drivers provide detailed vehicle information, including submitting various vehicle photos (`GET /driver/vehiclePhotos`, `GET /driver/vehiclePhotosB64` - upload mechanism is separate but data is viewed/managed here) and updating features like AC status (`POST /driver/updateAirCondition`). They also manage the service tiers their vehicle will operate in (`GET /driver/vehicleServiceTiers`, `POST /driver/updateServiceTiers`).
        - **Identity & Document Submission**: Drivers submit various identification and regulatory documents such as SSN/equivalent (`POST /driver/register/ssn`), PAN card (`POST /driver/register/pancard`), and Aadhaar card (`POST /driver/register/aadhaarCard`).
        - **Verification Processes**: The platform supports initiating background verification (`POST /driver/backgroundVerification`) and liveness checks, including retrieving live selfies (`GET /driver/register/getLiveSelfie`) and logging interactions with specialized KYC SDKs like HyperVerge (`POST /driver/register/logHvSdkCall`).
        - **Financial Setup**: Drivers can link and check the status of their bank accounts (`GET /driver/register/bankAccount/link`, `GET /driver/register/bankAccount/status`) for receiving payouts.
        - **Rate Card Transparency**: Drivers can view applicable rate cards (`GET /driver/rateCard`) based on various trip parameters.
        This thorough process ensures that drivers and their vehicles meet platform standards before they can start accepting rides.
- **Accurate and Up-to-Date Driver Vehicle Information (`dynamic-offer-driver-app` via `API.Action.UI.VehicleDetails`)**: To ensure compliance, accurate service matching, and a safe experience, the platform needs detailed and correct information about each driver's vehicle.
    - **Structured Vehicle Data Collection**: Drivers can select their vehicle's manufacturer from a list of supported makes (`GET /v2/ui/vehicleMakes`) and then select the specific model from a list filtered by that make (`POST /v2/ui/vehicleModels`).
    - **Comprehensive Vehicle Details Submission**: Drivers submit full details about their vehicle (`POST /v2/ui/vehicleDetails`), including registration number, color, year, insurance information, permits, and potentially photos of relevant documents.
    - This information is crucial for the `API.Action.UI.DriverOnboardingV2` process, for matching vehicles to appropriate service tiers, and for ensuring all vehicles on the platform meet regulatory and safety standards.
- **Flexible and Dynamic Data Collection from Drivers (`dynamic-offer-driver-app` via `API.Action.UI.DriverProfileQuestions`)**: To gather more detailed information about drivers, the platform allows drivers to provide additional questions and answers during their onboarding process.
    - Drivers can submit their responses to these questions via `POST /v2/ui/driver/profile/questions`.
    - This data can be used for analytics, improving service quality, and tailoring the platform's offerings to drivers' needs.
- **Effective Driver Operational Management & Ride Execution (`dynamic-offer-driver-app` via `API.UI.Driver`)**: Once onboarded, drivers need a comprehensive toolkit to manage their work and execute rides seamlessly. `API.UI.Driver` (delegating to `Domain.Action.UI.Driver` and its sub-modules like `RideStart`, `RideEnd`, `RideCancel`) provides this through functionalities for:
    - **Availability Control**: Setting their status (online/offline, specific modes like `ONDEMAND` or `SCHEDULED_DUTY`) and managing a "Go Home" feature (activating towards a saved location, managing saved home locations).
    - **Ride Offer Engagement**: Viewing nearby ride requests/opportunities, proactively offering quotes for specific search requests, and responding (accepting/rejecting) to ride offers allocated to them by the BPP system.
    - **Active Ride Lifecycle Management**: 
        - Starting rides (standard OTP-based or special OTP-based for street hails/zone pickups).
        - Indicating arrival at pickup, intermediate stops, and final destination.
        - Indicating departure from intermediate stops.
        - Ending rides, potentially with OTP confirmation and odometer readings.
        - Cancelling assigned or ongoing rides with appropriate reason codes.
        - Uploading supporting media like odometer readings or proof of delivery images.
    - **Profile and Performance Management**: Viewing detailed profile information, performance statistics (daily stats, earnings summaries), and managing profile photos.
    - **Financial Management**: Managing alternate phone numbers (for OTPs, possibly related to payouts), verifying VPA status for payouts, viewing detailed payment/transaction history, managing any outstanding dues to the platform, and downloading invoices/earning statements.
    - **Scheduled Booking Management**: Viewing and accepting pre-scheduled bookings or duties.
    - **Miscellaneous Utilities**: Updating device/app metadata relevant to their operation, fetching city-specific operational details.
    These features provide drivers with end-to-end control over their work cycle, from finding opportunities to completing rides and managing their earnings.
- **Comprehensive Ride Discovery (Rider & Provider Interaction)**:
    - **Rider Perspective (`rider-app`)**: The `rider-app` (via `API.UI.Search` and `Domain.Action.UI.Search`) provides powerful search capabilities for various transport needs:
        - **Standard Ride Search**: Allows users to search for conventional ride-hailing options (e.g., taxis). The platform then broadcasts Beckn search requests to BPPs to gather quotes. The system attempts to manage and cancel prior conflicting search requests to improve user experience if configured.
        - **Multimodal Search**: Enables users to find journeys combining different transport modes, including public transport (buses, subways). This involves integrating with external multimodal routing engines (e.g., OTP-like services), processing complex journey legs (using `Lib.JourneyModule`), and specific integrations like CRIS for Indian Railways subway information (fares, SDK tokens). User preferences for modes, walk distances, and sorting types (e.g., fastest) are considered to tailor results.
        - **Rate Limiting**: Protects the search functionality from abuse by applying user-specific limits, with operational alerts (Slack) for excessive use.
        - **Client Contextualization**: Captures client version and device details during search for analytics and compatibility.
        - **IMEI Handling (Multimodal)**: For multimodal searches involving specific providers like CRIS, IMEI (encrypted) might be used as part of the request parameters, highlighting a specific integration requirement.
        This dual-search capability aims to provide riders with a wide array of transport solutions, from simple point-to-point taxi rides to complex, multi-leg public transit journeys.
    - **Provider Perspective (`dynamic-offer-driver-app` - BPP Search Handling)**: After `rider-app` (as a BAP) broadcasts a search request to BPPs, it receives their responses via the Beckn `on_search` callback endpoint (`API.Beckn.OnSearch`).
        - Each `on_search` message from a BPP contains their catalog of offers/quotes for the requested search (`transactionId` links it to the original `SearchRequest`).
        - `rider-app` validates the context of this callback against the original search and transforms the Beckn payload into internal domain objects.
        - It employs Redis locks to handle potential duplicate messages from BPPs and processes the offers asynchronously.
        - Validated offers are then stored (e.g., as `Estimate` entities), making them available for the rider to retrieve via the `API.UI.Quote` endpoint (`GET /v2/rideSearch/{searchId}/results`).
        - This ensures that the user receives a comprehensive set of quotes from participating providers to compare and choose from.
- **Clear Ride Option Selection & Provider Acknowledgment**: After reviewing quotes, the `rider-app` (via `API.UI.Select` and domain actions `Domain.Action.UI.Select`/`Domain.Action.UI.Cancel`) facilitates a crucial intermediate step before final booking:
    - **User Choice**: Riders can explicitly select their preferred ride option (an `Estimate` identified by `estimateId`) using `POST /v2/estimate/{estimateId}/select` (or `/select2`).
    - **Communication with Provider (Beckn "select")**: This selection is communicated to the chosen BPP by sending a Beckn "select" message. This informs the provider of the user's intent and allows the BPP to prepare for a potential booking (e.g., by provisionally holding the resource).
    - **Response Management**: The system provides a `selectTtl` (Time-To-Live) calculated from `BecknConfig`, guiding the client on how long to await the BPP's `on_select` response (which would confirm or update the terms of the selected offer).
    - **Polling for Selection Outcome**: Riders can use `GET /v2/estimate/{estimateId}/results` to poll for the BPP's `on_select` response (e.g., updated quote, confirmation of terms).
    - **Flexibility to Cancel**: Users can cancel their selection (`POST /v2/estimate/{estimateId}/cancel`) before proceeding to final booking. This may also trigger a cancellation message to the BPP.
    - **Handling Special Cases**: A specific flow for `POST /v2/estimate/{estimateId}/rejectUpgrade` allows users to decline a provider's proposed upgrade, and this action is tagged using `Yudhishthira` for tracking or future rule application.
    - **Provider Confirmation of Selection (Beckn `on_select`)**: When a BAP (like `rider-app`) sends a Beckn `/select` request indicating the rider's chosen offer, the `dynamic-offer-driver-app` (BPP) receives it. After validation and ack, its domain logic (`DSelect.handler`) verifies driver/offer availability, may provisionally assign the driver, and formulates the `on_select` response.
        - It then sends an `on_select` callback to `rider-app` confirming the selection and finalized offer terms.
    This interaction ensures the BPP re-confirms its ability to fulfill the selected offer before the BAP proceeds to the booking initiation (`/init`) phase.
- **Formalizing Booking Intent (Rider & Provider Interaction - Beckn Init/OnInit)**:
    - **Rider Perspective (`rider-app`)**: After the BPP's `on_select` is processed and the rider confirms their intent to book, `rider-app` (via `API.UI.Confirm`) sends a Beckn `/init` message to the BPP. This message includes details from the selected and confirmed offer. `rider-app` then awaits an `on_init` callback from the BPP (processed by `API.Beckn.OnInit`). Critically, upon successful processing of this `on_init` from the BPP, `rider-app` *automatically* constructs and sends a Beckn `/confirm` message to the BPP.
    - **Provider Perspective (`dynamic-offer-driver-app` - BPP Init Handling)**: When a BAP (like `rider-app`) sends a Beckn `/init` request, the `dynamic-offer-driver-app` (via `API.Beckn.Init` and `Domain.Action.Beckn.Init`):
        - Receives the `/init` request, validates the BAP's signature, and transforms the Beckn `InitReqV2` into an internal `DInit.DInitReq`. This includes the `fulfillmentId` (from the `on_select` phase), billing information, and customer details.
        - Uses Redis locks (`initLockKey`, `initProcessingLockKey`, `initProcessedKey` based on `fulfillmentId`) for idempotency and to prevent re-entry if already processed.
        - `DInit.validateRequest` validates the request against the prior confirmed offer and merchant status.
        - Asynchronously (in a forked thread), its `DInit.handler` creates a preliminary `Booking` record internally on the BPP side, finalizing all terms (including payment details as provided by the BAP or determined by the BPP, and fare policy from `SFP.getFarePolicyByEstOrQuoteIdWithoutFallback`).
        - It then constructs an `on_init` callback message (`OnInit.OnInitReq` containing `Spec.OnInitMessage`) using `ACL.mkOnInitMessageV2`. This payload includes the BPP's confirmed booking details, finalized payment terms, fulfillment information (e.g., driver details if assigned at this stage, or an indication that assignment is pending), and a TTL based on `BecknConfig`.
        - This `on_init` callback is sent to the BAP (e.g., `rider-app`) using `Callback.withCallback`. Robust error handling is in place (e.g., attempting to cancel the BPP-side booking if sending `on_init` fails critically).
        - After successfully dispatching `on_init`, the BPP marks the init request as processed in Redis.
        - The BPP immediately sends a Beckn `Ack` to the BAP upon receiving the initial `/init` request.
    This init/on_init exchange finalizes all booking parameters from both BAP and BPP sides before the ultimate confirmation step.
- **Finalizing Booking Confirmation (Rider & Provider Interaction - Beckn Confirm/OnConfirm)**:
    - **Rider Perspective (`rider-app`)**: After the BPP's `on_confirm` is processed and the rider confirms their intent to book, `rider-app` (via `API.UI.Confirm`) sends a Beckn `/confirm` message to the BPP. This message includes details from the selected and confirmed offer. `rider-app` then awaits an `on_confirm` callback from the BPP (processed by `API.Beckn.OnConfirm`).
    - **Provider Perspective (`dynamic-offer-driver-app` - BPP Confirm Handling)**: When a BAP (like `rider-app`) sends a Beckn `/confirm` request, the `dynamic-offer-driver-app` (via `API.Beckn.Confirm` and `Domain.Action.Beckn.Confirm`):
        - Receives the `/confirm` request, validates the BAP's signature, and transforms it into an internal `DConfirm.ConfirmReq`. This includes the `fulfillmentId` (from the `on_confirm` phase), billing information, and customer details.
        - Uses Redis locks (`confirmLockKey`, `confirmProcessingLockKey`, `confirmProcessedKey` based on `fulfillmentId`) for idempotency and to prevent re-entry if already processed.
        - `DConfirm.validateRequest` validates the request against the prior confirmed offer and merchant status.
        - Asynchronously (in a forked thread), its `DConfirm.handler` creates a preliminary `Booking` record internally on the BPP side, finalizing all terms (including payment details as provided by the BAP or determined by the BPP, and fare policy from `SFP.getFarePolicyByEstOrQuoteIdWithoutFallback`).
        - It then constructs an `on_confirm` callback message (`OnConfirm.OnConfirmReq` containing `Spec.OnConfirmMessage`) using `ACL.mkOnConfirmMessageV2`. This payload includes the BPP's confirmed booking details, finalized payment terms, fulfillment information (e.g., driver details if assigned at this stage, or an indication that assignment is pending), and a TTL based on `BecknConfig`.
        - This `on_confirm` callback is sent to the BAP (e.g., `rider-app`) using `Callback.withCallback`. Robust error handling is in place (e.g., attempting to cancel the BPP-side booking if sending `on_confirm` fails critically).
        - After successfully dispatching `on_confirm`, the BPP marks the confirm request as processed in Redis.
        - The BPP immediately sends a Beckn `Ack` to the BAP upon receiving the initial `/confirm` request.
    This confirm/on_confirm exchange finalizes all booking parameters from both BAP and BPP sides before the ultimate confirmation step.
- **Flexible Ride Cancellation (Rider & Provider Interaction)**:
    - **Rider Perspective (`rider-app`)**: `rider-app` (via `API.UI.Cancel`) allows riders to request cancellations. It queries for potential dues and sends a Beckn `/cancel` message to the BPP with the cancellation reason and type (e.g., soft or hard cancel). It then awaits an `on_cancel` callback from the BPP (processed by `API.Beckn.OnCancel`) to know the final outcome, including any cancellation fees.
    - **Provider Perspective (`dynamic-offer-driver-app` - BPP Cancel Handling)**: When a BAP (like `rider-app`) sends a Beckn `/cancel` request, the `dynamic-offer-driver-app` (via `API.Beckn.Cancel` and `Domain.Action.Beckn.Cancel`):
        - Receives the `/cancel` request, validates the BAP's signature, and transforms it into an internal `DCancel.CancelReqInternal`. This can be a `DCancel.CancelRide` (for a booking) or `DCancel.CancelSearch` (to cancel a pre-booking search attempt).
        - For `CancelRide` requests:
            - It checks the `cancelStatus` provided by the BAP (e.g., `Enums.CONFIRM_CANCEL` for a hard cancel, or `Enums.SOFT_CANCEL` to query fees).
            - If `CONFIRM_CANCEL`: It uses a Redis lock, validates the request, and forks processing. `DCancel.cancel` updates the internal booking status, releases the driver (if assigned), determines if the driver was reallocated, and calculates the `cancellationCharge` (potentially involving `yudhishthira`). If not reallocated, an `on_cancel` callback with status `Enums.CANCELLED` and the fee is sent to the BAP.
            - If `SOFT_CANCEL`: It calls `DCancel.getCancellationCharges` to determine potential fees and sends an `on_cancel` callback with status `Enums.SOFT_CANCEL` and these charges to the BAP, allowing the BAP to present this to the user before a hard cancel.
        - For `DCancel.CancelSearch` requests: It validates and uses `SharedLogic.SearchTryLocker` to process the cancellation of the search attempt. No `on_cancel` is typically sent for this.
        - An `Ack` is sent immediately for the `/cancel` request. ONDC logging is performed.
    This ensures cancellations are processed correctly, fees are calculated according to policy, and the BAP is informed of the outcome.
- **Secure and Flexible Payment Preparation**: Before or during a ride, the `rider-app` (via `API.Action.UI.RidePayment` and `Domain.Action.UI.RidePayment`) enables users to:
    - Manage their stored payment methods (list, set default, delete).
    - Securely set up payment methods for future use (via setup intents).
    - Initiate payment intents, which represent an intention to pay and are necessary for modern secure payment flows (e.g., 3D Secure, SCA), providing the client with details to proceed with payment authorization with the gateway.
    - Select or update the payment method for a specific ride.
    - Add tips for a ride.
    - Manage their customer profile with the payment gateway.
    This prepares for the actual charging/debiting that happens upon ride completion or at other transactional points.
- **Ride Payment Execution and Status Tracking**: After a ride is completed or payment is due (and potentially after payment intents are successfully processed by the user with the gateway), the `rider-app` (via `API.UI.Payment` leveraging `Lib.Payment.API`, and delegating to `Domain.Action.UI.Payment`) handles:
    - **Payment Order Creation**: Initiating the actual charge for the ride by creating a payment order with the payment gateway (e.g., Juspay).
    - **Payment Status Monitoring**: Allowing the system and potentially the user to track the status of this payment order (e.g., pending, success, failure).
    - **Retrieval of Order Details**: Providing access to the details of the payment order.
    This ensures that rides are paid for and the financial transaction is properly recorded and tracked.
- **Finalizing Ride Booking & Post-Booking Management**: After the Beckn `init` phase is acknowledged by the provider (via `on_init` leading to BAP sending `confirm`), the `rider-app` (via `API.UI.Booking` and `Domain.Action.UI.Booking`) facilitates:
    - **Final Booking Confirmation (via BPP `on_confirm`)**: The BPP's `on_confirm` callback (handled by `API.Beckn.OnConfirm`) is the definitive confirmation. `Domain.Action.Beckn.OnConfirm` finalizes the local `Booking` and `Ride` status and stores all confirmed details (driver, vehicle, OTPs).
    - **Booking Status Tracking**: Allowing users to poll for real-time updates on their booking status (e.g., driver assigned, en route, completed) via `GET /rideBooking/v2/{rideBookingId}`.
    - **Ride History**: Providing access to a list of past bookings with relevant details via `/rideBooking/list` or `/rideBooking/listV2`.
    - **Active Ride Management**: Enabling features like adding or editing stops for an ongoing ride via `POST /rideBooking/{rideBookingId}/addStop` or `editStop`.
- **In-Ride Experience and Control**: During an active ride, the `rider-app` (via `API.UI.Ride`, `Domain.Action.UI.Ride`, and `SharedLogic.Ride`, and informed by `API.Beckn.OnStatus` and `API.Beckn.OnTrack` callbacks) provides riders with crucial information and control:
    - **Live Driver Tracking**: Ability to see the driver's current location on a map.
    - **Real-time Ride Status**: Updates on the ride's progress (e.g., driver arriving, ride started, en route).
    - **Destination Modification**: Flexibility to edit the drop-off location while the ride is in progress.
    - **Access to Ride Media**: Retrieval of driver photos for identification or delivery images for parcel services, enhancing transparency and trust.
- **Handling Mid-Ride Updates and BPP-Initiated Changes (Beckn `on_update`)**: For ongoing bookings/rides, BPPs may need to send various updates. The `rider-app` (via `API.Beckn.OnUpdate` and `Domain.Action.Beckn.OnUpdate`) handles these `on_update` callbacks, which can signify diverse events such as:
    - Driver/vehicle reassignment for scheduled or active rides.
    - Significant ETA changes.
    - Fare recalculations or updates (e.g., due to route changes, waiting time, or BPP adjustments).
    - BPP-initiated cancellations.
    - New messages from the provider or driver.
    - Safety alerts initiated by the provider.
    - Updates related to phone call interactions.
    - Arrival at intermediate stops or final destination.
    - Confirmation of fare payment from the BPP's end.
    - Updates related to rider-initiated destination edits.
    - Parcel delivery image uploads.
    The platform processes these updates to keep its internal state (booking, ride details) synchronized with the BPP and to inform the rider of any material changes to their ongoing service. This is crucial for managing exceptions and maintaining transparency throughout the active ride lifecycle.
- **Post-Ride Feedback and Driver Transparency**: After a ride, the `rider-app` (via `API.UI.Rating` and `Domain.Action.UI.Feedback`) enables users to:
    - **Submit Ratings**: Provide ratings for their ride experience, which helps maintain service quality and can be (conditionally) shared with providers via Beckn.
    - **Access Driver Information ("Know Your Driver")**: View driver profile details for an active ride or for a favorite driver, enhancing safety, trust, and a personal connection.
- **Structured Detailed Feedback Collection**: Beyond simple ratings, the `rider-app` (via `API.UI.FeedbackForm` and `Domain.Action.UI.FeedbackForm`) allows users to:
    - Request a structured feedback form, potentially tailored based on an initial rating (e.g., different questions for a 1-star vs. a 5-star rating).
    - Submit detailed responses to this form, providing specific insights into their experience.
    This enables the platform to gather more targeted and actionable feedback for service quality improvement, driver performance assessment, and addressing specific rider concerns.
- **Access to Financial Records (Invoices)**: After rides are completed and paid for, the `rider-app` (via `API.Action.UI.Invoice` and `Domain.Action.UI.Invoice`) allows users to retrieve their ride invoices for a specified date range. This provides financial transparency and allows users to keep records for expenses or other purposes.
- **Fare Transparency (Price Breakup for Bookings)**: After a ride, the `rider-app` (via `API.Action.UI.PriceBreakup` and `Domain.Action.UI.PriceBreakup`) allows users to view a detailed breakdown of their ride fare for a specific booking. This includes components like base fare, distance charges, time charges, tolls, taxes, discounts, and surge pricing, providing full transparency into how the final cost was calculated.
- **Detailed Fare Estimate Breakup (for specific Rides)**: To provide quick fare estimates for services like rentals or intercity trips, which might involve more complex calculations or external calls, the `rider-app` (via `API.UI.RentalsIntercityCache` and `Domain.Action.UI.RentalsIntercityCache`) utilizes a caching mechanism.
    - Users can request fare information for these services using specific parameters (`RIC.FareCacheReq`).
    - The system attempts to fetch this information from a pre-existing cache (`POST /v2/fetchFareCache`). If a valid, non-stale cached fare is available, it is returned quickly.
    - This improves user experience by reducing perceived latency for fare estimations for these service types. If the cache misses, the domain logic might then proceed to calculate/fetch the fare live and populate the cache.
- **Personalized User Profile and Safety Settings**: The `rider-app` (via `API.UI.Profile` and `Domain.Action.UI.Profile`) allows users to:
    - View and update their personal profile information (name, contact details, preferences).
    - Manage emergency settings and default emergency contacts, enhancing user safety and preparedness.
    This allows for a more personalized experience and provides crucial safety features.
- **Emergency Handling (SOS System)**: The `rider-app` (via `API.Action.UI.Sos` for core SOS logic and `API.UI.Sos` for media uploads) provides a comprehensive SOS system for riders in distress:
    - **SOS Alert Creation**: Users can quickly initiate an SOS alert during a ride.
    - **Emergency Information**: Ability to get details related to an SOS event for a ride.
    - **Status Updates**: The system can track and update the status of an SOS event.
    - **Resolution**: SOS events can be marked as resolved (e.g., "ride marked as safe").
    - **Emergency Services Contact**: Functionality to facilitate contacting emergency services (e.g., "call police").
    - **Evidence Upload**: Users can upload media (like videos) associated with an SOS event.
    - **IVR Integration**: Receives updates from external IVR systems on the outcome of emergency calls.
    This provides a critical safety net for riders.
- **Comprehensive Issue and Grievance Management**: The `rider-app` (via `API.UI.Issue` leveraging a shared `IssueManagement` component and a `customerIssueHandle` adapter) provides a robust system for users to report and manage issues:
    - **Issue Creation**: Users can report issues related to specific rides or public transport ticket bookings, selecting from predefined categories and options (which can be dynamic and hierarchical) and providing descriptions and media attachments.
    - **Beckn IGM Compliance**: For applicable issues, the system creates a formal Beckn IGM `issue` and sends it to the relevant BPP, ensuring interoperable grievance redressal. This includes validating issue subcategories against Beckn IGM specifications for different domains (On-Demand vs. Public Transport).
    - **Status Tracking & Updates**: Users can list their reported issues, view details, and update them (e.g., providing responses or ratings when an issue is resolved/closed by the support agent/BPP). The system also attempts to synchronize IGM issue statuses with BPPs.
    - **Integration with Ticketing Systems**: The platform can integrate with external ticketing systems for handling the backend support workflow.
    - **Internal Alerts & Reporting**: Mechanisms exist for internal issue reporting and alerting for unattended tickets, linking with SOS features.
    This ensures users have a clear channel to report problems and get them resolved, with adherence to open standards where applicable.
- **Direct & Specialized Support Channels**: In addition to formal issue/grievance management, the `rider-app` (via `API.UI.Support` and `Domain.Action.UI.Support`) offers more immediate or specific ways for users to seek help:
    - **General Issue Submission**: A direct channel (`/support/sendIssue`) for users to report problems that may not fit into the structured IGM categories or require quicker, less formal attention.
    - **Callback Requests**: Users can request a callback (`/support/callbackRequest`) from the support team, catering to those who prefer voice interaction for issue resolution.
    - **Safety Check Support**: A dedicated channel (`/support/safetyCheckSupport`) for queries or issues specifically related to safety checks or procedures, reinforcing the platform's focus on user safety.
    These channels complement the formal IGM process by providing alternative and potentially faster routes for certain types of support needs.
- **Flexible Ride Cancellation**: The `rider-app` (via `API.UI.Cancel` and `Domain.Action.UI.Cancel`) provides riders with options to cancel their bookings, along with clarity on potential costs:
    - **Cancellation Options**: Supports different cancellation scenarios, including "soft cancel" (potentially an early stage or conditional cancellation) and a definitive "hard cancel" which takes a `CancelReq` detailing reasons and preferences (e.g., `reallocate` driver).
    - **Communication with Provider (Beckn "cancel")**: All user-initiated cancellations result in a Beckn "cancel" message being sent to the relevant BPP, ensuring the provider is informed and can act accordingly (e.g., release driver, stop billing if applicable). The BPP's acknowledgment is processed via `API.Beckn.OnCancel`.
    - **Cancellation Dues Transparency**: Users can query potential cancellation fees or dues (`GET /v2/rideBooking/cancellationDues`) before deciding to cancel, allowing for an informed decision. This endpoint can function with or without a specific `rideBookingId`, suggesting it can also provide general policy information.
    This provides necessary flexibility for users while maintaining clear communication with providers and transparency regarding any financial implications of cancellation.
- **Proactive User Communication (Push Notifications)**: The `rider-app` (via `API.Action.UI.TriggerFCM` and `Domain.Action.UI.TriggerFCM`) provides a mechanism for the platform to send targeted Firebase Cloud Messaging (FCM) push notifications to riders. 
    - This enables proactive communication about important events such as ride status updates (driver assigned, arriving, ride started/ended), payment confirmations, promotional messages, safety alerts, or responses to support queries.
    - Notifications can include a title, body, and a custom data payload to trigger specific actions or navigate to relevant sections within the rider app upon opening.
    This keeps users informed in real-time and enhances engagement with the platform.
- **Reliable Asynchronous Beckn Communication (BAP Role)**: For `rider-app` to function effectively as a Beckn Application Platform (BAP), it needs to reliably receive and process asynchronous responses from Beckn Provider Platforms (BPPs). The `API.Beckn` and `API.BecknAPIV2` modules provide the secure (via `SignatureAuth`) endpoints for all standard Beckn `on_action` callbacks (`on_search`, `on_select`, `on_init`, `on_confirm`, `on_status`, `on_track`, `on_cancel`, `on_update`).
    - This ensures that when `rider-app` sends a request (e.g., search, select, init, confirm), it can properly receive and process the BPP's corresponding asynchronous response (e.g., quotes, selection confirmation, booking status), update its internal state, and ultimately reflect these updates to the rider. Each `on_action` is handled by a dedicated module, promoting organized processing of these crucial BPP callbacks.
- **In-App Rider-Driver Communication (Voice Calls)**: The `rider-app` (via `API.UI.Call` and `Domain.Action.UI.Call`) facilitates voice communication between riders and drivers for a specific ride, typically using an external telephony service like Exotel.
    - **Backend-Initiated Calls**: Riders can initiate a call to their assigned driver directly from the app. The backend manages the call setup (likely including number masking for privacy), tracks the call status, and receives updates via callbacks from the telephony provider.
    - **IVR/Telephony Provider Driven Interactions**: The system also supports scenarios where an external IVR or telephony system (e.g., Exotel) drives the call flow. `rider-app` provides endpoints for this system to fetch necessary driver contact information (e.g., masked numbers) and to post call completion statuses and recordings back to the platform.
    This ensures a secure and reliable way for riders and drivers to connect when needed (e.g., for pickup coordination) without exposing direct phone numbers.
- **Granular Call Event Logging**: To provide a deeper understanding of call interactions beyond basic status, the `rider-app` (via `API.UI.CallEvent` and `Domain.Action.UI.CallEvent`) supports logging specific, granular events that occur during or related to a call.
    - This allows the platform to capture details like call answered, call disconnected by either party, DTMF tones entered (e.g., for IVR navigation during a call), call transfer attempts, or specific error conditions encountered during call setup or an active call.
    - Capturing these detailed events via `POST /v2/callEvent` (with `DCE.CallEventReq`) is valuable for debugging call-related issues, analyzing call flow patterns for service improvement, and potentially for audit trails or more sophisticated call analytics, complementing the overall call status information obtained through other callbacks (like those from Exotel via `API.UI.Call`).
- **Efficient Fare Estimation for Specialized Services (Rentals/Intercity)**: To provide quick fare estimates for services like rentals or intercity trips, which might involve more complex calculations or external calls, the `rider-app` (via `API.UI.RentalsIntercityCache` and `Domain.Action.UI.RentalsIntercityCache`) utilizes a caching mechanism.
    - Users can request fare information for these services using specific parameters (`RIC.FareCacheReq`).
    - The system attempts to fetch this information from a pre-existing cache (`POST /v2/fetchFareCache`). If a valid, non-stale cached fare is available, it is returned quickly.
    - This improves user experience by reducing perceived latency for fare estimations for these service types. If the cache misses, the domain logic might then proceed to calculate/fetch the fare live and populate the cache.
- **Comprehensive Lifecycle Management for Multimodal Journeys**: Once a multimodal journey is planned (via `API.UI.Search`), the `rider-app` (via `API.Action.UI.MultimodalConfirm` and `Domain.Action.UI.MultimodalConfirm`) provides a rich set of functionalities for users to manage their entire complex trip:
    - **Initiation and Confirmation**: Users can formally initiate and then confirm their overall multimodal journey, which likely triggers individual booking processes for each leg.
    - **Status and Information**: Access to detailed booking information, payment status, and overall journey status.
    - **Payment Management**: Ability to update payment orders specifically for multimodal journeys.
    - **Leg Modification**: High degree of flexibility to adapt the journey en route, including switching transport modes for a leg, skipping legs, re-adding skipped legs, extending legs (with fare recalculation), and even switching service tiers for specific FRFS (public transport) legs.
    - **Real-time Rider Location Updates**: Users can provide their location during the journey, enabling better coordination for subsequent legs or real-time adjustments.
    - **Journey Cancellation**: Option to cancel the entire multimodal journey.
    - **Feedback**: Submit and retrieve feedback specific to a multimodal journey.
    - **Preference Management**: View and update personal preferences for multimodal travel (e.g., allowed transit modes, sorting options), which influences future multimodal searches.
    - **Supporting Data**: Access to general public transport data and potentially lite transit options for quick reference.
    This suite of features allows users to not just plan but also actively manage and adapt complex multimodal trips, offering a significantly enhanced level of control and personalization for such journeys.
- **Utility Bill Payments & Service Integration (BBPS)**: The `rider-app` (via `API.Action.UI.BBPS` and `Domain.Action.UI.BBPS`) integrates with the Bharat Bill Payment System (BBPS), allowing users to potentially pay for a variety of bills and services beyond just mobility.
    - **Service Discovery & Session**: Users can initiate a BBPS session to identify billers or services.
    - **Payment Orders**: Create payment orders for BBPS transactions, likely leveraging the platform's standard payment infrastructure (e.g., using existing payment methods through the `payment` library).
    - **Status Tracking**: Users can track the status of their BBPS payments.
    - **Transaction History**: Access to a list of past BBPS orders made through the platform.
    - **Asynchronous Updates**: The system includes unauthenticated callback endpoints (`/bbps/confirm-payment`, `/bbps/payment-status`) for external BBPS intermediaries or gateways to post asynchronous updates about payment confirmations and status changes, ensuring the platform reflects the final outcome of these transactions.
    This extends the platform's utility, making it a more versatile tool for users by enabling them to manage and pay for various services through a single interface.
- **Integrated CRIS (Metro/Subway) Ticketing Support**: Beyond just fetching fares during multimodal search (which also uses CRIS for SDK tokens), the `rider-app` (via `API.Action.UI.CRIS` and `Domain.Action.UI.CRIS`) provides deeper integration with the Centre for Railway Information Systems (CRIS) for metro/subway functionalities:
    - **CRIS SDK Data Retrieval**: Allows the client application to fetch necessary configuration or operational data (`POST /v2/cris/getSDKData`) required for a client-side CRIS SDK to function correctly, ensuring a smooth user experience within the CRIS ticketing environment.
    - **CRIS OTP Generation**: Facilitates user authentication or transaction authorization directly with CRIS by enabling the platform to request OTP generation (`GET /v2/cris/otp/generation`) from the CRIS system. This is vital for secure operations like ticket purchase or account modifications within the CRIS ecosystem via NammaYatri.
    - **Device Management for CRIS**: Supports changing the device registered with CRIS (`POST /v2/cris/change/device`), which can be crucial if CRIS ticketing or account access is tied to specific device identifiers for security or operational reasons.
    These features enable a more seamless and integrated experience for users leveraging CRIS-based public transport services through the NammaYatri platform, reducing the need for them to interact with separate CRIS applications for these account management and authorization tasks.
- **FRFS-Based Public Transport Ticketing**: For public transport services operating on a Fare, Route, Fee, Schedule (FRFS) model (e.g., many bus services), the `rider-app` (via `API.Action.UI.FRFSTicketService` and `Domain.Action.UI.FRFSTicketService`) provides an end-to-end ticketing solution:
    - **Discovery**: Users can discover available FRFS routes, stations (with autocomplete), and specific service details for a given city and vehicle type.
    - **Search & Quoting**: Allows searching for specific trips based on criteria, and then retrieving quotes for those trips (which would include fare and schedule information).
    - **Booking & Payment**: Users can confirm a quote and book FRFS tickets, which then integrates with the platform's standard payment order creation process.
    - **Post-Booking Management**: Provides functionalities to list booked FRFS tickets, view their status, check if they can be cancelled, initiate cancellations, and verify tickets (e.g., for boarding or inspection).
    - **Configuration Access**: Allows fetching FRFS-specific configurations for a city.
    This deep integration makes it convenient for users to plan and pay for scheduled public transport alongside other mobility options offered by the platform.
- **Seamless Integration with Kapture CRM/Support**: To provide a unified customer support experience, the `rider-app` (via `API.Action.UI.TicketKapture` and `Domain.Action.UI.TicketKapture`) facilitates integration with an external Kapture CRM system.
    - **Customer Session Establishment**: The `POST /v2/kaptureCustomerLogin` endpoint allows an authenticated NammaYatri user to be logged into or have their session established with Kapture.
    - This enables riders to potentially view their support ticket history within Kapture, interact with Kapture-based support channels, or for support agents using Kapture to have immediate context about the NammaYatri user.
    - This creates a more streamlined support process by linking the NammaYatri user identity with their corresponding profile or case history in the Kapture CRM.

- **Access to Ride-Related Insurance Information**: The `rider-app` (via `API.Action.UI.Insurance` and `Domain.Action.UI.Insurance`) allows authenticated users to retrieve details of insurance policies that may be associated with their rides or profile.
    - Users can fetch specific policy information using a reference ID (`GET /v2/insurance/{referenceId}`).
    - This provides transparency and allows users to access their insurance coverage details, terms, and conditions directly through the platform, which is important for peace of mind and in case of claims.
    - Note: This flow focuses on retrieving existing policy details; the offering and purchasing of insurance would be handled by a separate process, likely during booking.

- **User Acquisition and Engagement through Referrals**: The `rider-app` (via `API.Action.UI.CustomerReferral` and `Domain.Action.UI.CustomerReferral`) implements a customer referral program to incentivize user growth and engagement:
    - **Applying Referral Codes**: New or existing users can apply referral codes (`POST /v2/person/applyReferral`) to potentially receive benefits, and the referrer is also acknowledged.
    - **Tracking Referrals**: Referrers can track the number of successful referrals they've made (`GET /v2/CustomerRefferal/count`).
    - **Managing Payouts**: Referrers can view their payout history for referrals (`GET /v2/referralPayout/history`) and manage their VPA (Virtual Payment Address, for UPI) for receiving these payouts (`POST /v2/payoutVpa/upsert`).
    - **VPA Verification**: The system supports VPA verification (`GET /v2/referral/verifyVpa`) to ensure payout details are correct.
    This system helps grow the user base organically and rewards existing users for advocacy.

- **Convenient Social Media Authentication & Profile Linking**: To simplify user onboarding and login, the `rider-app` (via `API.Action.UI.SocialLogin` and `Domain.Action.UI.SocialLogin`) integrates with third-party social media providers.
    - **Social Login/Sign-up**: Users can choose to register or log in using their existing social media accounts (`POST /v2/social/login`), reducing friction and the need to create new credentials. The platform securely validates the social media token and fetches basic profile information to create/link the NammaYatri account.
    - **Profile Linking**: Authenticated NammaYatri users can link their social media accounts to their existing platform profile (`POST /v2/social/update/profile`), which can further personalize their experience or be used for future social interactions if applicable.
    This provides users with a familiar and often quicker way to access the platform.

- **Personalization through Favourite Driver Management**: The `rider-app` (via `API.Action.UI.FavouriteDriver` and `Domain.Action.UI.FavouriteDriver`) allows riders to personalize their experience by managing a list of their favorite drivers.
    - **View Favourites**: Users can retrieve a list of all drivers they have previously marked as favorites (`GET /v2/driver/favorites`).
    - **Remove Favourite**: Users can remove a driver from their favorites list (`POST /v2/favorites/{driverId}/remove`).
    - (Note: The mechanism for *adding* a driver to favorites is likely part of another flow, such as post-ride rating or from ride history, as it's not exposed in this specific API module.)
    This feature can enhance rider satisfaction by allowing them to easily identify and potentially prioritize drivers with whom they've had good experiences, if the platform later uses this information in driver matching or search presentation.

- **Real-time Visibility of Nearby Drivers**: To give users an immediate sense of vehicle availability before initiating a formal search, the `rider-app` (via `API.Action.UI.NearbyDrivers` and `Domain.Action.UI.NearbyDrivers`) allows fetching and displaying nearby available drivers.
    - Users provide their current location (and optionally a radius or vehicle type preferences) via `POST /v2/nearbyDrivers` (`NearbyDriverReq`).
    - The system queries a real-time driver location data store to find active and available drivers within the specified vicinity, matching the criteria.
    - This information (`NearbyDriverRes`, including driver locations and vehicle types) can then be displayed on a map in the rider's app, enhancing their perception of service availability and potential wait times.

- **Real-time Public Transport Visibility (Nearby Buses & Next Vehicle ETAs)**: To improve the utility of public transport options, `rider-app` (via `API.Action.UI.NearbyBuses` and `Domain.Action.UI.NearbyBuses`) provides riders with real-time information:
    - **Nearby Buses**: Users can request information about buses operating near their current location (`POST /v2/nearbyBusBooking` with `NearbyBusesRequest`), helping them understand immediate PT availability.
    - **Next Vehicle Details**: For a specific route/stop, they can get upcoming vehicle ETAs via `GET /v2/nextVehicleDetails/{routeCode}/{stopCode}`. This information (`UpcomingTripInfo`) is structured to potentially integrate with and update multimodal journey plans.
    This enhances the user's ability to make informed decisions about using public transport by providing live operational data, rather than just static schedules.

- **Targeted Place/Point of Interest (POI) Search**: To assist users in accurately specifying origins, destinations, or finding points of interest, the `rider-app` (via `API.Action.UI.Places` and `Domain.Action.UI.Places`) provides a dedicated place search capability.
    - Users can search for places using keywords, location context, or category filters (`POST /v2/places` with `PlacesRequest`).
    - The system then queries an external POI database or specialized place search API to return a list of matching places (`PlacesResponse`) with details like name, address, and coordinates.
    This complements general map autocomplete by allowing for more specific or categorized searches, improving the ease of setting precise locations for ride requests or discovering relevant POIs.

- **Live Tracking of Public Transport Vehicles on Specific Routes**: To further enhance predictability for public transport users, `rider-app` (via `API.Action.UI.TrackRoute` and `Domain.Action.UI.TrackRoute`) allows users to track the live locations of vehicles (e.g., buses) operating on a specific route.
    - Users can request tracking information for a given `routeCode` (`GET /v2/track/{routeCode}/vehicles`), optionally filtering by `platformType` or `vehicleType`.
    - The system queries real-time vehicle location feeds to provide the current positions and potentially other details (like ETAs to upcoming stops) of active vehicles on that route (`TrackingResp`).
    This feature enables users to see exactly where their bus (or other PT vehicle) is, reducing anxiety and allowing for better planning of their arrival at the stop.

- **Static Route Geometry and Details Retrieval**: To display planned routes on maps (e.g., for a selected ride, driver's path to pickup, or a specific trip's path), `rider-app` (via `API.UI.Route` and `Domain.Action.UI.Route`) fetches static route information from an external mapping service.
    - **General Route**: Provides a generic endpoint (`POST /route`) to get a route between specified waypoints based on `Maps.GetRoutesReq`.
    - **Pickup Route**: Offers a specialized endpoint (`POST /pickup/route`) to fetch the route from a driver to the rider's pickup point using `DRoute.GetPickupRoutesReq`.
    - **Trip Route**: Another specific endpoint (`POST /trip/route`) fetches the route for an ongoing or completed trip, also using `Maps.GetRoutesReq`.
    - The system proxies these requests to an external mapping service (like OSRM or Google Maps), returning route polylines, distance, estimated time, and maneuvers (`Maps.GetRoutesResp`). This is essential for visualizing paths in the user interface for various contexts.

- **Pre-Search Serviceability Checks (Origin/Destination, Intercity)**: To improve user experience and manage expectations, `rider-app` (via `API.UI.Serviceability`) allows for upfront checks before a full search is initiated:
    - **Origin/Destination Serviceability**: Users can have their proposed pickup (`POST /serviceability/origin`) or drop-off (`POST /serviceability/destination`) locations checked against the platform's defined serviceable geofences and provider operational areas. This prevents users from attempting searches that are bound to fail.
    - **Intercity Trip Identification**: The platform can determine if a proposed trip is intercity (`POST /serviceability/isInterCity`), which can influence which providers are searched, pricing models applied, or if the service is available at all.
    These checks help guide the user early in their planning process and ensure that searches are directed towards valid and available service options.

- **Intelligent Hotspot Suggestions**: To assist users in finding convenient and popular pickup/drop-off locations, `rider-app` (via `API.UI.HotSpot`) identifies and suggests relevant "hotspots."
    - Users provide their current location (`GET /v2/getHotSpot` with `Maps.LatLong` in body), and the system retrieves `HotSpotConfig` for the merchant.
    - Based on this configuration (search radius, frequency thresholds, ranking weights), it uses geohashing to find nearby areas and queries Redis for pre-aggregated `HotSpot` data. This data reflects historical user activity (pickups, trip starts/ends, saved locations) and predefined special locations.
    - The system filters and ranks these potential hotspots, returning a list of the most relevant ones (`HotSpotResponse` containing `HotSpotInfo`).
    This feature can simplify location selection, guide users to efficient meeting points, and highlight areas with high service demand or designated zones.

- **Essential Map Utilities (Autocomplete, Place Details, Reverse Geocoding)**: To ensure a smooth and accurate location selection experience, `rider-app` (via `API.UI.Maps` and `Domain.Action.UI.Maps`) provides core map utility services by proxying requests to an external mapping provider.
    - **Address Autocomplete**: As users type an address or place name, they receive real-time suggestions (`POST /v2/maps/autoComplete`), making input faster and more accurate.
    - **Place Details**: After selecting a suggestion, the app can fetch detailed information about that place (`POST /v2/maps/getPlaceDetails`), such as precise coordinates and formatted address.
    - **Reverse Geocoding**: When a user interacts with a map (e.g., dropping a pin), the system can convert these coordinates into a human-readable address (`POST /v2/maps/getPlaceName`).
    These services are fundamental for setting accurate pickup and drop-off locations and for general map interactions within the app.

- **Frontend-Backend State Synchronization and Event Tracking**: To ensure a responsive and coherent user experience across complex application flows, and to gather insights into user behavior, `rider-app` (via `API.UI.Frontend` and `Domain.Action.UI.Frontend`) provides mechanisms for frontend-backend communication regarding user state and UI events:
    - **User Flow Status**: Client applications can query the backend (`GET /v2/frontend/flowStatus`) to determine the user's current status within a particular workflow (e.g., booking, payment). This allows the UI to adapt intelligently, guiding the user appropriately or displaying relevant information based on their backend state (e.g., if there's an active booking).
    - **Frontend Event Notification**: Client applications can notify the backend about specific user interactions or UI events (`POST /v2/frontend/notifyEvent` with `NotifyEventReq` containing `FrontendEvent`). This data can be used for analytics (to understand user journeys and identify friction points), for backend state updates if a UI event has backend implications, or to trigger specific backend logic based on user actions.
    This two-way communication channel helps in building more sophisticated and context-aware user experiences.

- **Managing In-Ride Location Edits and Confirmations**: When a rider requests a change to their ride location (e.g., destination, via `API.UI.Ride`), `rider-app` (through `API.Action.UI.EditLocation` and `Domain.Action.UI.EditLocation`) manages the subsequent confirmation process:
    - After the initial edit request is sent to the BPP and the BPP responds (via `on_update`), the rider can fetch the outcome of this request (e.g., new fare, new ETA) using `GET /v2/edit/{bookingUpdateRequestId}/result`.
    - If the rider agrees to the proposed changes (especially if there are fare implications), they can confirm their acceptance via `POST /v2/edit/result/{bookingUpdateRequestId}/confirm`.
    - This ensures that any modifications to the ride are explicitly approved by the rider if they impact cost or other critical terms, maintaining transparency and user control over in-ride changes.

- **Ride Sharing and Progress Tracking for Safety & Convenience**: To enhance rider safety and provide peace of mind to their contacts, `rider-app` (via `API.Action.UI.FollowRide` and `Domain.Action.UI.FollowRide`) enables ride sharing and tracking functionalities:
    - **Initiate Ride Sharing**: Riders can choose to share their ongoing ride progress (`POST /v2/share/ride` with `ShareRideReq`), allowing designated individuals to follow their journey.
    - **View Followers**: Riders can see who is currently following their shared rides (`GET /v2/follow/ride`).
    - **Follower Access to Ride Status**: Individuals who are following a ride can get basic details about the rider (`GET /v2/followRide/{rideId}/customerDetails`) and, importantly, check the status of the rider's emergency contacts or if an SOS has been triggered for that ride (`GET /v2/followRide/ECStatus/{rideId}`).
    This feature provides a valuable safety net and keeps concerned parties informed about the rider's well-being and trip progress.

- **Aadhaar-Based Identity Verification (eKYC)**: To meet regulatory requirements or enable certain trusted features, `rider-app` (via `API.UI.AadhaarVerification` and `Domain.Action.UI.AadhaarVerification`) supports Aadhaar-based eKYC.
    - **OTP Generation**: Users can initiate Aadhaar verification (`POST /v2/verifyAadhaar/generateOtp` with their Aadhaar number), which triggers an OTP to their Aadhaar-linked mobile number via an external verification service.
    - **OTP Verification**: Users then submit the received OTP (`POST /v2/verifyAadhaar/verifyOtp`) to complete the verification. Successful validation may update their KYC status on the platform and potentially fetch/store KYC data (with consent).
    This provides a standardized and recognized method for identity verification.

- **Tracking App Installations and Device Information**: To gather analytics on app adoption and manage device-specific aspects, `rider-app` (via `API.UI.AppInstalls` and `Domain.Action.UI.AppInstalls`) provides an unauthenticated endpoint for client applications to register their installation details.
    - Upon first launch or significant updates, the client app sends information like its version, device ID, OS details, and platform (`POST /v2/appInstalls/create` with `AppInstallsReq`).
    - The backend stores this information, which can be used for tracking installation numbers, understanding device demographics, version adoption rates, and potentially for associating a device with a user account later on if the user logs in or registers.

- **Standardized Cancellation Reason Collection**: To better understand cancellation patterns and enforce policies consistently, `rider-app` (via `API.UI.CancellationReason` and `Domain.Action.UI.CancellationReason`) provides a list of standardized cancellation reasons.
    - Users or the system can retrieve a context-specific list of reasons (`GET /v2/cancellationReason/list` with `cancellationStage` parameter) when a ride is being cancelled.
    - This allows for collecting structured data on why cancellations occur (e.g., driver denied duty, rider no-show, changed plans), which is valuable for analytics, improving service quality, and applying appropriate cancellation fees or penalties based on the reason and stage.

- **Supporting Accessibility Needs (Disability Options Listing)**: To cater to users with disabilities and promote inclusivity, `rider-app` (via `API.UI.Disability` and `Domain.Action.UI.Disability`) provides a standardized list of disability types or accessibility options.
    - Users can retrieve this list (`GET /v2/disability/list`).
    - This information can then be used by riders to declare their specific accessibility requirements in their profile (likely handled by `API.UI.Profile`).
    - This declaration can, in turn, inform service providers (e.g., drivers, transport operators) about the user's needs, or be used to filter for services and vehicles that are equipped with necessary accessibility features (e.g., wheelchair accessible vehicles).

- **Multilingual Support via Translation Proxy**: To cater to a diverse user base, `rider-app` (via `API.UI.GoogleTranslate` and `SharedLogic.GoogleTranslate`) provides a backend proxy to Google Translate.
    - Authenticated users can request translation of text (`GET /v2/language/translate`) by specifying source language, target language, and the query text.
    - This enables the platform to translate user-generated content (e.g., feedback, chat messages if applicable), dynamically localize information from providers, or support a multilingual user interface, making the service more accessible across different language groups.

- **B2B Integration for FRFS Ticketing (Partner Organizations)**: The `rider-app` (via `API.UI.PartnerOrganizationFRFS` and `Domain.Action.UI.PartnerOrganizationFRFS`) provides a dedicated API for partner organizations to integrate NammaYatri's FRFS public transport ticketing into their own services.
    - **Partner-Specific Access**: Partners use API key authentication for endpoints like `/frfs/partnerOrganization/{apiKey}/upsertPersonAndGetFare`, `getFareV2`, `getConfig`, and `upsertPersonAndQuoteConfirm`. These are rate-limited.
    - **End-User Management**: When partners request fares or book tickets, they provide end-user details. The system handles creating or linking these to NammaYatri user profiles (`DPOFRFS.upsertPersonAndGetToken`), enabling transactions on behalf of these users.
    - **FRFS Service Consumption**: Partners can get FRFS configurations, retrieve fare information (which reuses the core FRFS search logic of `DFRFSTicketService`), and confirm quotes to book tickets for their users.
    - **Ticket Management for End-Users**: Additional endpoints (e.g., `/frfs/shareTicketInfo`, `/frfs/auth`, `/frfs/authVerify`) allow end-users whose tickets were booked via partners to manage or authenticate access to their ticket details.
    This B2B interface allows NammaYatri to expand its FRFS ticketing reach through third-party platforms and services.

- **Access to Personal Usage Statistics**: To provide users with insights into their platform activity, `rider-app` (via `API.UI.PersonStats` and `Domain.Action.UI.PersonStats`) allows authenticated users to retrieve their personal usage statistics.
    - Users can fetch a summary (`GET /v2/personStats`) of metrics such as total rides taken, total distance traveled, amount spent, number of cancellations, and potentially other engagement or environmental impact metrics (e.g., CO2 saved).
    - This feature empowers users by giving them a clear overview of their interaction history and value derived from the platform.

- **Management of WhatsApp Communication Preferences**: To respect user choices regarding communication channels, `rider-app` (via `API.UI.Whatsapp` and `Domain.Action.UI.Whatsapp`) allows users to manage their opt-in/opt-out status for WhatsApp messages.
    - Users can update their preference (`POST /v2/whatsapp/opt` with `OptAPIRequest` indicating opt-in or opt-out).
    - This ensures that users only receive WhatsApp communications (like notifications or support messages) if they have consented, aligning with privacy best practices and user expectations.

- **Dynamic Client UI Configuration**: To enable flexible and adaptable user interfaces without frequent app releases, `rider-app` (via `API.Action.UI.Cac` and `Domain.Action.UI.Cac`) provides a mechanism for client applications to fetch dynamic UI configurations.
    - Clients can request configurations (`POST /v2/getUiConfigs`) by specifying parameters like `tenant` or a `toss` value (for A/B testing variants), and potentially providing a JSON body detailing the specific configurations needed.
    - The backend returns a JSON object containing these configurations, which can include feature flags, UI themes, text strings, remote URLs, or any other parameters that govern the client's appearance and behavior.
    This allows for rapid iteration on UI/UX, targeted feature rollouts, and A/B testing of different UI presentations.

- **User Account Deletion**: To comply with data privacy rights and provide users with control over their data, `rider-app` (via `API.Action.UI.DeletedPerson` and `Domain.Action.UI.DeletedPerson`) allows authenticated users to request the deletion of their account.
    - Users can initiate an account deletion request (`POST /v2/deleted/person` with `DeletedPersonReq`).
    - The backend then processes this request, which typically involves marking the account for deletion, anonymizing or deleting Personally Identifiable Information (PII) from relevant database tables according to data retention policies, invalidating active sessions, and potentially scheduling related data cleanup tasks.
    This ensures that users can exercise their right to be forgotten.

- **Utility for Testing QR Code Functionality**: To ensure robustness of QR code-based interactions, `rider-app` (via `API.Action.UI.Miscellaneous` and `Domain.Action.UI.Miscellaneous`) provides a test utility.
    - This endpoint (`POST /v2/misc/testScanQR` with `QRScanTestReq`) allows developers or QA to submit scanned QR data to the backend.
    - The backend then parses this data and performs test logic to validate its format, interpret its meaning (e.g., does it map to a known entity or action), and simulate the expected outcome.
    This helps in verifying that various QR code use cases (e.g., for payments, promotions, ride initiation) are handled correctly by the platform.

- **Internal Channel for Ride Feedback Submission**: To accommodate feedback received outside the primary rider app (e.g., via customer support), `rider-app` provides an internal API (`POST /internal/feedback/rateRide` via `API.Internal.Rating` and `Domain.Action.Internal.Rating`).
    - This allows authorized internal systems or personnel to submit ride ratings and comments using the same `Domain.FeedbackReq` structure as the rider-facing UI.
    - This ensures that all feedback, regardless of origin, can be centrally collected, processed, and used for quality monitoring, driver performance assessment, and potentially for triggering communications with BPPs.

- **Internal FRFS Status Updates**: To manage the state and progression of complex journeys (especially multimodal or multi-stop rides), `rider-app` provides an internal mechanism (`POST /internal/stopEvents/stop` via `API.Internal.StopEvents` and `Domain.Action.Internal.StopEvents`) to record and process events related to journey stops.
    - This allows other backend systems or processes to notify `rider-app` about events like arrival at an intermediate stop, departure from a stop, or a stop being skipped.
    - This is critical for updating the trip's lifecycle, recalculating ETAs for subsequent legs, triggering notifications (e.g., to prepare for the next leg of a multimodal journey), and for accurate logging and analytics of trip progression.

- **Triggering Driver Arrival Notifications (Internal)**: To ensure riders are timely informed about their driver's approach and arrival, `rider-app` provides an internal mechanism (`POST /internal/driverArrivalNotification` via `API.Internal.DriverArrivalNotf` and `Domain.Action.Internal.DriverArrivalNotf`).
    - This allows other backend systems (e.g., a location tracking service monitoring driver ETAs) to signal `rider-app` when a driver is arriving soon or has arrived at the pickup location.
    - `rider-app` then takes responsibility for constructing and dispatching the appropriate notification (e.g., push notification, SMS) to the rider, enhancing the pickup experience and reducing missed connections.

- **Informing Drivers about Demand Hotspots (`dynamic-offer-driver-app` via `API.Action.UI.DemandHotspots`)**: To help drivers optimize their positioning and earnings, the platform provides them with information about areas with high ride demand.
    - Authenticated drivers can request demand hotspot information (`GET /v2/ui/driver/demandHotspots`), implicitly for their current operational city/merchant context.
    - The backend (`Domain.Action.UI.DemandHotspots.getDriverDemandHotspots`) processes this by querying a data store or service that aggregates real-time or near real-time demand signals (e.g., search request density, surge data, historical patterns).
    - The response (`GetDemandHotspotsResp`) provides a list of hotspot areas, potentially with demand intensity levels, enabling drivers to move towards areas where they are more likely to receive ride requests.

- **Providing Drivers with a Profile Summary (`dynamic-offer-driver-app` via `API.UI.DriverProfileSummary`)**: To give drivers a quick overview of their key information, the platform provides an endpoint to fetch a profile summary.
    - Authenticated drivers can request their profile summary (`GET /v2/ui/driver/profile/summary`), optionally requesting inclusion of `fleetInfo`.
    - The backend (`Domain.Action.UI.DriverProfileSummary.getDriverProfileSummary`) retrieves and aggregates essential driver details (name, status, vehicle info, overall rating) and, if requested, fleet association information.
    - This allows the driver app to display a concise summary on a main screen or profile page for easy reference.

- **Driver Referral Program Management (`dynamic-offer-driver-app` via `API.UI.DriverReferral`)**: To incentivize driver growth and engagement, `dynamic-offer-driver-app` allows drivers to refer new drivers to the platform.
    - **Referral Code Generation**: Drivers can generate referral codes (`POST /v2/driver/referral/generate`) for potential new drivers.
    - **Referral Tracking**: The system tracks the number of successful referrals (`GET /v2/driver/referral/count`) and the status of each referral.
    - **Referral Rewards**: Drivers receive a reward for each successful referral.
    - **Referral Management**: Drivers can manage their referral codes and track the status of their referrals (`GET /v2/driver/referral/list`).
    - **Referral Terms**: The platform defines terms and conditions for the referral program.
    This feature encourages drivers to refer new drivers, increasing the platform's driver base and improving the overall service quality.

- **Driver-Side Financial Management (Payouts, Settlements) (`dynamic-offer-driver-app` via `API.UI.Payment`)**: To ensure drivers receive their earnings and manage financial transactions, `dynamic-offer-driver-app` (using `API.UI.Payment` which leverages `Lib.Payment.API` and `Domain.Action.UI.Payment`):
    - **Payout Initiation**: Allows for the creation of payment orders (`POST /payment`), likely to initiate payouts to drivers based on an `Invoice` ID. This suggests a system where driver earnings are invoiced, and then these invoices trigger payout payment orders.
    - **Transaction Status Tracking**: Drivers can track the status of these payment orders (`GET /payment/{orderId}/status`) and view their details (`GET /payment/{orderId}`).
    - **Notification Status**: Supports checking the status of asynchronous payment notifications (`GET /payment/notification/{notificationId}/status`), which is important for confirming the final state of payouts.
    This system provides drivers with transparency and control over their financial transactions with the platform.

- **Driver Referral Payout Management (`dynamic-offer-driver-app` via `API.Action.UI.ReferralPayout`)**: // ... (existing content for driver referral payouts) ...

- **Driver Performance Monitoring (`dynamic-offer-driver-app` via `API.UI.Performance`)**: To help drivers track their activity and effectiveness on the platform, `dynamic-offer-driver-app` provides an endpoint for them to retrieve their performance metrics.
    - Authenticated drivers can fetch a summary of their performance (`GET /v2/ui/driver/performance`).
    - The backend (`Domain.Action.UI.Performance.getDriverPerformance`) aggregates data such as total rides, acceptance rates, average ratings, earnings over specific periods, and other relevant KPIs.
    - This allows drivers to monitor their performance, understand how effectively they are utilizing the platform, and identify areas where they can improve.

- **In-App Messaging for Drivers (`dynamic-offer-driver-app` via `API.UI.Message`)**: To facilitate communication between the platform and drivers, `dynamic-offer-driver-app` provides an in-app messaging system.
    - Drivers can list their messages (`GET /v2/ui/message/list`), view specific messages (`GET /v2/ui/message/{messageId}`), and mark messages as seen (`PUT /v2/ui/message/{messageId}/seen`) or liked (`PUT /v2/ui/message/{messageId}/like`).
    - The system also supports replying to messages (`PUT /v2/ui/message/{messageId}/response` with `MessageReplyReq`) and fetching associated media (`GET /v2/ui/message/media`).
    - This allows the platform to send important announcements, alerts, ride-related information, or support communications directly to drivers within their app, ensuring they are well-informed.

- **Call-Based Ride Termination (Exotel Integration) (`dynamic-offer-driver-app` via `API.UI.ExotelEndRide`)**: // ... (existing content from a previous turn) ...

- **Providing Merchant-Specific Operational City Information to Drivers (`dynamic-offer-driver-app` via `API.UI.City`)**: To ensure drivers are aware of and can operate within their designated service areas, especially if their associated merchant operates in multiple cities.
    - Authenticated drivers can fetch a list of cities where their current merchant operates (`GET /v2/ui/city/{merchantId}/list`).
    - This allows the driver application to display relevant cities, enabling drivers to, for example, select their current city of operation or understand the scope of their service agreement with the merchant.

- **Listing Kiosk Locations for Drivers (`dynamic-offer-driver-app` via `API.UI.KioskLocation`)**: To assist drivers in finding and utilizing designated operational points, the platform provides a way to list relevant kiosk locations.
    - Authenticated drivers can fetch a list of kiosk locations (`GET /v2/ui/kioskLocation/list`) pertinent to their current merchant and operating city.
    - These kiosks might be important as designated pickup/drop-off zones (e.g., at airports, malls), driver support centers, or areas with specific operational procedures.
    - This helps drivers navigate to and operate effectively from these defined points.

- **Driver Engagement through Leaderboards (`dynamic-offer-driver-app` via `API.UI.LeaderBoard`)**: To motivate drivers and foster a sense of healthy competition, the platform provides performance-based leaderboards.
    - Authenticated drivers can fetch daily (`GET /v2/ui/driver/leaderBoard/daily`), weekly (`GET /v2/ui/driver/leaderBoard/weekly`), and monthly (`GET /v2/ui/driver/leaderBoard/monthly`) leaderboards based on their merchant and city context.
    - The backend (`Domain.Action.UI.LeaderBoard`) aggregates performance data (e.g., ride counts, earnings, ratings) and ranks drivers.
    - This gamification element can encourage drivers to improve their performance and engagement with the platform.

- **Proactive Driver Communication via Push Notifications (OnMessage) (`dynamic-offer-driver-app` via `API.UI.OnMessage`)**: To ensure drivers receive timely alerts even when not actively using the app, `dynamic-offer-driver-app` can trigger FCM push notifications based on specific message events or other conditions.
    - An authenticated endpoint (`POST /v2/ui/onMessage` with `FCMReq`) allows the system to initiate a push notification to a driver.
    - This could be used to alert drivers about new critical messages in their in-app inbox, important platform announcements, or other events requiring their attention.
    - The backend (`Domain.Action.UI.OnMessage.sendMessageFCM`) constructs and dispatches the FCM notification.

- **Providing Navigational Routes for Assigned Rides (`dynamic-offer-driver-app` via `API.UI.RideRoute`)**: To assist drivers in navigation, the platform provides specific route information for their current ride.
    - Once a ride is assigned, the authenticated driver can fetch the route details for that specific `rideId` (`POST /v2/ui/{rideId}/route`).
    - The backend (`Domain.Action.UI.RideRoute.rideRoute`) retrieves the ride's waypoints (pickup, destination, intermediate stops) and uses an external mapping service to calculate and return the optimal route polyline, distance, and estimated travel time (`RouteInfo`).
    - This allows the driver application to display the route on a map, providing clear turn-by-turn guidance for reaching the pickup location and then navigating to the destination(s).

- **Driver Subscription Plan Management (`dynamic-offer-driver-app` via `API.UI.Plan`)**: To offer varied service levels, features, or earning models to drivers, the platform allows drivers to manage subscription plans.
    - Drivers can list available plans (`GET /v2/ui/plan/list`), filtered by vehicle variant and service name (e.g., `YATRI_SUBSCRIPTION`).
    - They can view their current active plan (`GET /v2/ui/plan/currentPlan`) and list available service contexts for plans (`GET /v2/ui/plan/services`).
    - Drivers can subscribe to a new plan (`POST /v2/ui/plan/{planId}/subscribe`). This process checks for existing suspended autopay and attempts to resume it and clear dues before processing the new subscription.
    - They can also switch to a different plan (`PUT /v2/ui/plan/{planId}/select`) or suspend/resume their current plan (`PUT /v2/ui/plan/suspend`, `PUT /v2/ui/plan/resume`).
    This system enables flexible service offerings and potentially tiered access for drivers based on their chosen subscription plan.

- **Driver Incentive and Rewards Program (Coins) (`dynamic-offer-driver-app` via `API.UI.DriverCoins`)**: To motivate and reward drivers for their activity and performance, the platform includes a coin-based rewards system.
    - **Coin Information**: Drivers can get an overview of their current coin balance and information about the rewards program (`GET /v2/ui/coins/info`).
    - **Transaction History**: They can view their coin transaction history (earned, spent) for specific dates (`GET /v2/ui/coins/transactions`) and a general paginated history of coin usage (`GET /v2/ui/coins/usageHistory`).
    - **Coin Redemption**: Drivers can convert their earned coins into cash (`POST /v2/ui/coins/convertCoinToCash`), which likely triggers a payout process.
    - **Earning Context**: Drivers can also fetch ride status information for past days (`GET /v2/ui/coins/rideStatusPastDays`), which may help them understand how their activities (like ride completions) contribute to coin earnings.
    This system aims to enhance driver engagement and loyalty by providing tangible rewards for their participation on the platform.

- **Access to Daily/Specific Date Ride Summaries for Drivers (`dynamic-offer-driver-app` via `API.UI.RideSummary`)**: To help drivers track their work and earnings over specific periods, the platform allows them to fetch ride summaries for requested dates.
    - Authenticated drivers can request summaries for a list of dates (`POST /v2/ui/rideSummary/list` with `[Day]`).
    - The backend (`Domain.Action.UI.RideSummary.listDailyRidesSummary`) aggregates and returns key metrics for rides completed on those dates (e.g., total rides, earnings, distance for each day or a list of ride summaries).
    - This provides drivers with a convenient way to review their past activity for specific days.

- **General-Purpose Route Calculation for Drivers (`dynamic-offer-driver-app` via `API.UI.Route`)**: To support various navigational and estimation needs beyond specific active rides, the platform provides general route calculation services for drivers.
    - Authenticated drivers can request routes for generic purposes (`POST /v2/ui/route`), specifically to a pickup point (`POST /v2/ui/pickup/route`), or for an entire trip (`POST /v2/ui/trip/route`), by providing origin, destination, and optional waypoints (`Maps.GetRoutesReq`).
    - The backend (`Domain.Action.UI.Route` functions) interacts with an external mapping service to calculate and return route details (polyline, distance, ETA - `Maps.GetRoutesResp`).
    - This allows the driver app to display routes for planning, estimate ETAs for potential new rides, or support features like navigating to a custom "Go Home" location.

- **Comprehensive Issue Reporting for Drivers (including SOS/Emergency) (`dynamic-offer-driver-app` via `API.UI.Issue`)**: To enable drivers to report various problems, including safety concerns or emergencies, the platform provides a structured issue reporting system.
    - This system, accessed via endpoints under `/v2/ui/issue/`, leverages a shared `IssueManagement` library, adapted for the driver context through a `driverIssueHandle`.
    - **Issue Creation**: Drivers can create new issue reports (`POST /`), providing details and selecting from predefined issue categories (`GET /category/list`) and options (`GET /option/list`). SOS or emergency scenarios would be handled by selecting an appropriately configured critical category.
    - **Media Upload**: They can upload supporting media (images/videos) for their issue reports (`POST /media/upload`).
    - **Tracking and Management**: Drivers can list their reported issues (`GET /list`), view details of a specific issue (`GET /{issueReportId}`), update issue options, and potentially delete or update the status of their reports, depending on configured permissions.
    This provides a vital channel for driver feedback, support requests, and critical incident reporting, ensuring driver safety and well-being.

- **Dynamic UI and Feature Configuration for Driver App (CaC) (`dynamic-offer-driver-app` via `API.Action.UI.Cac`)**: // ... (existing content from a previous turn) ...

- **Driver Feedback on Platform-Mediated Calls (`dynamic-offer-driver-app` via `API.Action.UI.CallFeedback`)**: To monitor and improve the quality of telephony services (e.g., driver-rider calls, calls to support), the platform allows drivers to submit feedback on their call experiences.
    - After a call facilitated by the platform, authenticated drivers can submit feedback (`POST /v2/ui/driver/call/feedback`) including a rating, feedback category, and comments (`API.Types.UI.CallFeedback.CallFeedbackReq`).
    - The backend (`Domain.Action.UI.CallFeedback.postDriverCallFeedback`) stores this feedback, linking it to the call record.
    - This data is valuable for assessing call quality, addressing issues with the telephony provider, and understanding driver experiences during these interactions.

- **Driver Consent for In-Ride Booking Modifications (`dynamic-offer-driver-app` via `API.Action.UI.EditBooking`)**: To ensure drivers agree to significant changes during an active ride (e.g., destination change requested by a rider), the platform facilitates driver consent.
    - When a booking modification is proposed (typically initiated by the rider via the BAP and processed by the BPP), the driver is notified and can respond.
    - The driver submits their decision (accept/reject) via `POST /v2/ui/edit/result/{bookingUpdateRequestId}` with `EditBookingRespondAPIReq`.
    - The backend (`Domain.Action.UI.EditBooking.postEditResult`) updates the `BookingUpdateRequest` and the `Ride` if accepted, and informs the rider (BAP) of the outcome.
    - This ensures driver agreement for material changes to the ongoing trip, maintaining transparency and control.

- **Fare Estimation Tool for Drivers (`dynamic-offer-driver-app` via `API.Action.UI.FareCalculator`)**: To help drivers understand potential earnings or provide indicative quotes, the platform offers a fare calculation tool.
    - Authenticated drivers can request a fare estimate (`GET /v2/ui/calculateFare`) by providing pickup and drop-off locations, along with a `distanceWeightage` parameter that might influence the calculation.
    - The backend (`Domain.Action.UI.FareCalculator.getCalculateFare`) uses this information, potentially combined with route data from a mapping service and the driver's vehicle/service type, to apply relevant rate cards and pricing rules, returning an estimated fare (`FareResponse`).
    - This tool aids drivers in making informed decisions about accepting rides or in scenarios where they might need to provide an upfront fare indication.

- **Access to Ride-Related Insurance Details for Drivers (`dynamic-offer-driver-app` via `API.Action.UI.Insurance`)**: To ensure drivers have access to relevant insurance information for their trips, the platform allows them to retrieve policy details.
    - Authenticated drivers can fetch insurance details (`GET /v2/ui/insurance/{referenceId}`) using a `referenceId` (e.g., ride ID, booking ID, or policy ID).
    - The backend (`Domain.Action.UI.Insurance.getInsurance`) retrieves this information, potentially by calling an internal service or the BAP's insurance endpoint, and returns it as `SharedLogic.CallBAPInternal.InsuranceAPIEntity`.
    - This provides drivers with transparency regarding insurance coverage applicable to their work on the platform.

- **Filtered Invoice Retrieval for Drivers (`dynamic-offer-driver-app` via `API.Action.UI.Invoice`)**: To enable drivers to easily access and manage their financial records, the platform provides a way to retrieve invoices with specific filters.
    - Authenticated drivers can fetch a list of their invoices (`GET /v2/ui/invoice`) by optionally filtering by a date range (`fromDate`, `toDate`) and/or vehicle registration number (`rcNo`).
    - The backend (`Domain.Action.UI.Invoice.getInvoice`) queries for relevant invoices (e.g., for rides, periodic earnings statements) and returns a list of `InvoiceRes` objects containing details like invoice ID, date, amount, status, and potentially links to PDF versions.
    - This allows drivers to efficiently find and review their invoices for specific periods or vehicles, aiding in financial tracking and reconciliation.

- **Driver Training and Certification through Learning Management System (LMS) (`dynamic-offer-driver-app` via `API.Action.UI.LmsModule`)**: // ... (existing content from a previous turn) ...

- **City-Specific Operational Configurations for Drivers (`dynamic-offer-driver-app` via `API.Action.UI.Merchant`)**: To ensure driver applications adapt to local operational rules and settings, the platform provides city-specific configurations.
    - Authenticated drivers can fetch configurations pertinent to their current merchant and operating city (`GET /v2/ui/cityConfigs`).
    - The backend (`Domain.Action.UI.Merchant.getCityConfigs`) retrieves these settings (e.g., local operational parameters, UI tweaks, city-specific feature flags) from a configuration store.
    - This allows the driver app to tailor its behavior and UI according to the specific city the driver is operating in, enhancing compliance and local relevance.

- **Managing Meter-Based Rides (`dynamic-offer-driver-app` via `API.Action.UI.MeterRide`)**: For services that operate on a metered fare basis, the platform provides specific tools for drivers.
    - **Adding/Updating Destination**: During an ongoing meter ride, drivers can add or update the destination (`POST /v2/ui/meterRide/{rideId}/addDestination` with `MeterRideAddDestinationReq`). This would likely involve fare recalculation based on the new destination.
    - **Sharing Ride Receipt**: After completing a meter ride, drivers can trigger the sharing of the ride receipt with the rider (`POST /v2/ui/meterRide/{rideId}/shareReceipt` with `SendReceiptRequest`).
    This ensures proper handling and documentation for meter-based trips.

- **Driver Interaction with Operation Hubs (`dynamic-offer-driver-app` via `API.Action.UI.OperationHub`)**: To provide drivers with a structured way to manage various operational tasks requiring interaction with specific service points or teams.
    - **Hub Discovery**: Drivers can list available operation hubs (`GET /v2/ui/operation/getAllHubs`) to understand where they can seek specific services or support.
    - **Request Submission**: Drivers can submit formal requests to these hubs (`POST /v2/ui/operation/createRequest` with `DriverOperationHubRequest`), for needs such as vehicle maintenance, document updates, scheduling queries, or other operational assistance. Requests are often tied to a specific vehicle (`rcNo`).
    - **Request Tracking**: Drivers can view the history and status of their submitted requests (`GET /v2/ui/operation/getRequests`), filtered by date, status, request type, and vehicle.
    This system streamlines driver support and manages operational workflows that extend beyond immediate ride execution.

- **Formalizing Driver-Operator Associations (`dynamic-offer-driver-app` via `API.Action.UI.Operator`)**: // ... (existing content from a previous turn) ...

- **Providing Fare Transparency to Drivers (Meter Pricing & Breakdowns) (`dynamic-offer-driver-app` via `API.Action.UI.PriceBreakup`)**: To ensure drivers understand their earnings and how fares are calculated, the platform provides tools for fare finalization and detailed breakdowns.
    - **Meter Ride Price Finalization**: For meter-based rides, drivers (or the app) can submit final ride parameters (`POST /v2/ui/meterRide/price` with `MeterRidePriceReq` for a specific `rideId`) to calculate and confirm the final price.
    - **Detailed Fare Breakup**: For any ride, drivers can fetch a detailed component-wise breakdown of the fare (`GET /v2/ui/priceBreakup` for a specific `rideId`), showing how elements like base fare, distance charge, time charge, tolls, etc., contribute to the total. This is returned as a list of `RateCardItem` objects.
    This functionality is critical for driver trust, financial clarity, and resolving any fare-related discrepancies.

- **Driver Engagement with Short Video Content (Reels) (`dynamic-offer-driver-app` via `API.Action.UI.Reels`)**: To enhance driver engagement and disseminate information effectively, the platform provides access to short video content ("Reels").
    - Authenticated drivers can fetch a list of reels (`GET /v2/ui/reels/getAllReelVideos`) based on a specific `reelsKey` (to categorize content like safety tips, new features, announcements) and an optional `language` preference.
    - The backend (`Domain.Action.UI.Reels.getReelsGetAllReelVideos`) retrieves these reels from a content store, providing drivers with engaging, easily digestible information.
    - This feature supports micro-learnings, updates, and general driver communication.

- **Simplified Driver Authentication with Social Media Accounts (`dynamic-offer-driver-app` via `API.Action.UI.SocialLogin`)**: To offer drivers convenient sign-up and login options, the platform supports social media authentication.
    - **Social Sign-up/Login**: New or existing drivers can use their social media accounts (e.g., Google) to sign in (`POST /v2/ui/social/login` with social provider token). If the social ID is new, a platform account is created; otherwise, it logs into the existing linked account, returning platform session tokens.
    - **Linking Social Accounts**: Authenticated drivers can link their existing platform profile to a social media account (`POST /v2/ui/social/update/profile` with social provider token) for future login convenience.
    This enhances user experience by reducing the need to remember separate platform credentials.

- **Providing Information on Special Operational Locations to Drivers (`dynamic-offer-driver-app` via `API.Action.UI.SpecialLocation`)**: To help drivers navigate and operate correctly within designated special zones (e.g., airports, malls, event venues), the platform allows them to fetch a list of these locations.
    - Authenticated drivers can retrieve a list of special locations (`GET /v2/ui/specialLocation/list`) relevant to their current merchant and operating city, with an option to filter by `isOrigin` (e.g., to see only pickup-allowed zones).
    - The backend (`Domain.Action.UI.SpecialLocation.getSpecialLocationList`) queries and returns detailed information for each location (`SpecialLocationFull`), including boundaries, type, and specific operational rules.
    - This helps drivers adhere to specific procedures and manage their operations effectively in these controlled areas.

- **Managing Operations and Personnel at Special Locations ("Warriors") (`dynamic-offer-driver-app` via `API.Action.UI.SpecialLocationWarrior`)**: For specialized roles (e.g., ground staff, team leads, designated "Warriors") managing activities at special locations.
    - **Categorized Special Location Listing**: Authorized personnel can fetch special locations filtered by a specific operational `category` (`GET /v2/ui/specialLocation/list/category`).
    - **Warrior Information Management**: They can retrieve (`GET /v2/ui/getInfo/specialLocWarrior`) and update (`POST /v2/ui/updateInfo/specialLocWarrior`) operational information pertaining to other drivers (target "Warriors") associated with or working at these special locations.
    This system supports coordinated management of personnel and resources within complex operational zones like airports or event venues.

- **Secure Handling of Driver Payment Information via Tokenization (`dynamic-offer-driver-app` via `API.Action.UI.Tokenization`)**: To securely manage driver payment details (e.g., for payouts or platform fees collected via card), the platform uses a tokenization approach.
    - Authenticated drivers, when needing to add or update payment card information, can request an SDK token from the backend (`GET /v2/ui/driver/sdkToken`). This request specifies the target tokenization `service` (e.g., Juspay) and desired `expiry` for the SDK token.
    - The backend (`Domain.Action.UI.Tokenization.getDriverSdkToken`) interacts with the specified external tokenization service to generate this temporary, single-use SDK token.
    - The driver application uses this SDK token to initialize the payment provider's client-side SDK, which then securely captures and tokenizes the card details directly with the provider.
    - The resulting payment token (not the SDK token or raw card details) is then sent to the backend for storage and future use in transactions, ensuring PCI compliance.
    This process enhances security by minimizing the backend's exposure to sensitive payment information.

- **Facilitating Fleet Operations and Managed Trips (WMB) (`dynamic-offer-driver-app` via `API.Action.UI.WMB`)**: For drivers operating within specific fleet or managed service contexts (termed "WMB"), the platform provides tools to:
    - **Identify and Discover**: List fleet-specific badges (`GET /wmb/fleetBadges`), discover available WMB routes (`POST /wmb/availableRoutes`), and get route details (`GET /wmb/route/{routeCode}/details`).
    - **Manage Trips**: Initiate WMB trips (e.g., via QR scan - `POST /wmb/qr/start`), view active trips (`GET /wmb/trip/active`), list past trips (`GET /wmb/trip/list`), and manage the lifecycle (start/end) of specific WMB trip transactions.
    - **Handle In-Trip Requests/Alerts**: Submit operational requests related to a WMB trip (`POST /wmb/trip/{tripTransactionId}/request`) and track their status (`GET /wmb/requests/{approvalRequestId}/status`, `POST /wmb/requests/{approvalRequestId}/cancel`).
    - **Fleet Association**: Consent to fleet association (`POST /fleet/consent`) and retrieve fleet-specific configurations (`GET /fleet/config`).
    This suite of tools supports structured operations for drivers working under a fleet or managed service model.

- **Driver Call Management for Customer Communication (`dynamic-offer-driver-app` via `API.UI.Call`)**: // ... (existing content from a previous turn) ...

- **Granular Logging of Call-Related Events for Drivers (`dynamic-offer-driver-app` via `API.UI.CallEvent`)**: To enable detailed tracking and analysis of telephony interactions, the platform allows the driver application to log specific events related to calls.
    - Authenticated drivers (or their application) can send call event data (`POST /v2/ui/callEvent` with `DCE.CallEventReq`) such as "call answered," "call hangup," "DTMF input," or "call failed."
    - The backend (`Domain.Action.UI.CallEvent.logCallEvent`) stores these granular events, linking them to the relevant call session.
    - This detailed logging is valuable for troubleshooting call issues, understanding IVR interactions, and improving the overall quality of platform-mediated communication.

- **Profile Management for Organization Administrators (`dynamic-offer-driver-app` via `API.UI.OrgAdmin`)**: // ... (existing content from a previous turn) ...

- **Enabling Drivers to Provide Feedback on Rides/Riders (`dynamic-offer-driver-app` via `API.UI.Rating`)**: To maintain a fair and high-quality service, drivers can submit feedback on their ride experiences and interactions with riders.
    - After a ride, authenticated drivers can submit a rating and comments (`POST /v2/ui/feedback/rateRide` with `CallBAPInternal.FeedbackReq`).
    - The backend (`Domain.Action.UI.Rating.rating`) stores this feedback, associating it with the specific ride and rider.
    - This feedback mechanism contributes to a balanced two-way rating system and helps in addressing any issues concerning rider conduct or the overall ride experience from the driver's perspective.

- **Standardized Ride Cancellation Reasons for Drivers (`dynamic-offer-driver-app` via `API.UI.CancellationReason`)**: To ensure consistent data and fair policy application when drivers need to cancel rides.
    - Authenticated drivers can fetch a list of predefined cancellation reasons (`GET /v2/ui/cancellationReason/list`).
    - The backend (`Domain.Action.UI.CancellationReason.list`) provides these standardized reasons (e.g., rider no-show, vehicle issue).
    - This allows drivers to select an appropriate reason if they must cancel an assigned trip, which is important for analytics and policy enforcement.

- **Providing Transporter/Merchant Details to Drivers (`dynamic-offer-driver-app` via `API.UI.Transporter`)**: To ensure drivers are aware of the organization they are formally associated with.
    - Authenticated drivers can fetch detailed information about their linked transporter or merchant (`GET /v2/ui/transporter`).
    - The backend (`Domain.Action.UI.Transporter.getTransporter`) uses the driver's session context to retrieve and return these details (`TransporterRec`).
    - This provides transparency for drivers regarding their affiliation and access to relevant organizational information.

## How It Should Work (High-Level)
- **Riders & BAP (`rider-app`)**: 
    - **Authentication Flow**: // ... (existing detailed content from a previous turn) ...
    - // ... (All previously detailed rider flows and internal flows through Internal Insurance Lookup) ...
    - **Dashboard - Booking Management (Admin Users)**:
        - An administrator using a dashboard UI identifies stuck bookings and triggers a bulk cancellation via `POST .../bookingCancelAllStuck`.
        - An administrator needs to sync multiple bookings and uses the dashboard to call `POST .../bookingSyncMultiple` with relevant booking IDs and data.
- **Drivers & BPP (`dynamic-offer-driver-app`)**:
    - **Driver Authentication**: In addition to OTP, drivers can use `POST /v2/ui/social/login` with a social media token to sign up or log in.
    - **Detailed Onboarding**: During payment setup (part of onboarding or later profile management), the driver app:
        - Calls `GET /v2/ui/driver/sdkToken` to get a token for the chosen payment tokenization service.
        - Uses this SDK token with the payment provider's client-side SDK to capture and tokenize card details.
        - Sends the resulting payment token (not raw card data) to a backend endpoint (likely part of `API.UI.Driver` or `API.UI.Payment`) for storage against the driver's profile.
    - **Core Operations via Driver App**: // ... (existing content) ...
        - Authenticated drivers can link their social accounts using `POST /v2/ui/social/update/profile`.
    - // ... (existing content for other driver UI features and BPP Beckn flows) ...

- **External Telephony Providers (e.g., Exotel, Twilio)**: // ... (existing content, their events might also trigger logging via webhooks or app) ...

- **External Systems (Payment Gateways, Mapping Services, KYC SDKs, FCM, Ticketing Systems, CaC/Config Management System, Telephony Providers, Pricing/Rule Engines, Insurance Providers, LMS Content Platforms, Operation Hub Management Systems, Operator Management Portals, Social Media Identity Providers, CMS for Reels, Special Zone Configuration Tools, Payment Tokenization Services, Fleet Management Systems, etc.)**: // ... (existing content) ...

- **Platform (Overall Interaction for Init/OnInit)**:
    1.  Rider confirms booking intent in `rider-app` (BAP) after a successful select/on_select.
    2.  `rider-app` sends a Beckn `/init` request to `dynamic-offer-driver-app` (BPP).
    3.  `dynamic-offer-driver-app` acknowledges (`Ack`), then asynchronously validates the request, creates a preliminary booking, and finalizes all terms.
    4.  `dynamic-offer-driver-app` sends an `on_init` callback to `rider-app` with these finalized details.
    5.  `rider-app` processes the `on_init` (via `API.Beckn.OnInit`) and, if successful, automatically sends a Beckn `/confirm` request to `dynamic-offer-driver-app`.

- **Platform (Overall Interaction for Cancel/OnCancel)**:
    1.  Rider initiates cancellation in `rider-app` (BAP).
    2.  `rider-app` sends a Beckn `/cancel` request (possibly after a soft cancel query) to `dynamic-offer-driver-app` (BPP).
    3.  `dynamic-offer-driver-app` acknowledges (`Ack`), then asynchronously processes the cancellation, updates its state, and calculates fees.
    4.  `dynamic-offer-driver-app` sends an `on_cancel` callback to `rider-app` with the outcome.
    5.  `rider-app` processes the `on_cancel` (via `API.Beckn.OnCancel`), updates the rider, and potentially charges cancellation fees.

## User Experience Goals
- **Secure and Easy Onboarding (Rider & Driver)**: Both riders and drivers should have a straightforward and secure registration and login process. For drivers, after initial OTP authentication, the detailed onboarding for document submission, vehicle registration, and KYC checks should be clear, guided, and efficient, with transparency on requirements and status.
    - **Streamlined Vehicle Registration**: Drivers should find it easy to provide their vehicle details through a structured process (selecting make, then model, then providing specific attributes), with clear guidance on required information and document uploads.
    - **Clear and Contextual Data Collection**: Drivers should be able to provide contextual data about their vehicle and operational preferences, which can be used for personalized service recommendations and operational efficiency.
    - **Effective Onboarding Training**: Drivers should receive clear and concise training on platform features, operational guidelines, and safety best practices.
    - **Transparent Operator Association**: Drivers should have a clear understanding of their role and responsibilities within the platform's ecosystem, including expectations for customer service, vehicle maintenance, and emergency procedures.
    - **Convenient Authentication Options**: Drivers should have multiple convenient ways to sign up and log in, including using their existing social media accounts.
    - **Secure Payment Method Setup**: Drivers should be able to securely add and manage their payment methods (e.g., bank accounts for payouts, cards for platform fees) through a process that inspires confidence, typically involving client-side tokenization to protect sensitive data.
- **Empowered and Efficient Driver Operations**: Drivers should have a clear, intuitive application that allows them to easily manage their availability, view and respond to ride offers effectively, seamlessly execute rides (start, navigate stops, end), track their earnings and performance, manage their profile, and access necessary financial information. The "Go Home" feature should provide a convenient way to end their workday.
    - **Optimized Earnings through Demand Insights**: Drivers should be able to easily access information about current demand hotspots, allowing them to strategically position themselves to maximize ride opportunities and earnings.
    - **Quick Access to Key Profile Information**: Drivers should be able to easily view a summary of their important profile details (like status, vehicle, overall rating, fleet info if applicable) within their app.
    - **Transparent and Reliable Financials for Drivers**: Drivers should have a clear understanding of their earnings, how fares are calculated (including detailed breakdowns for each ride), and be able to easily initiate or track their payouts.
        - **Accessible Invoice History**: Drivers should be able to easily find and view their invoices, with options to filter by date or vehicle, to keep track of their earnings and platform-related charges.
    - **Clear Insight into Performance**: Drivers should have easy access to their key performance indicators, helping them understand their effectiveness, track progress towards goals, and identify opportunities for improvement.
    - **Effective Platform Communication for Drivers**: Drivers should receive timely and relevant communications from the platform (announcements, alerts, ride updates, support messages) through an accessible in-app messaging system, with options to interact (mark seen, like, reply) as appropriate.
        - **Timely Alerts for Important Messages**: Drivers receive push notifications for critical messages or alerts, ensuring they don't miss important information even if not actively using the driver app.
    - **Accessible Ride Controls (Fallback Options)**: Users should have clear and easy-to-use options for requesting assistance or canceling a ride, such as a dedicated "Help" button or a "Cancel" option during the ride.
    - **Clear Understanding of Operational Areas**: Users should have a clear understanding of the operational areas they can request rides from, including the types of vehicles available and the expected wait times.
    - **Easy Navigation to Designated Kiosk Locations**: Users should be able to easily find and navigate to designated kiosk locations for pickup and drop-off, minimizing the need for direct contact with drivers.
    - **Motivation and Engagement through Gamification**: The platform should provide incentives and recognition for drivers who perform well, such as leaderboards, rewards, or personalized feedback.
    - **Clear Navigational Guidance for Rides**: The platform should provide clear and consistent guidance for riders to navigate through the booking process, including understanding the different stages of the booking lifecycle.
    - **Flexible Service Plan Options**: The platform should offer a variety of service plan options to suit different rider needs, allowing them to choose the one that best fits their preferences and budget.
    - **Easy Access to Past Ride Summaries**: The platform should provide easy access to past ride summaries for riders to review their past experiences and make informed decisions about future rides.
    - **Versatile Navigational Support**: The platform should support a range of navigational needs, including searching for rides, requesting ride updates, and managing bookings.
    - **Reliable and Accessible Issue Reporting (including SOS)**: The platform should provide clear and easy-to-use channels for riders to report issues and receive timely support, ensuring that their concerns are addressed promptly.
    - **Contextually Relevant and Adaptable UI**: The platform should adapt its user interface and functionality based on the user's current context, providing a seamless and intuitive experience.
    - **Ability to Provide Feedback on Call Experiences**: The platform should provide a clear and easy-to-use mechanism for riders to provide feedback on their call experiences, helping to improve the overall service quality.
    - **Control Over In-Ride Modifications**: Users should have a transparent process for requesting changes like a new destination during a ride. The BPP's processing of the `/update` and sending an `on_update` with clear implications (new fare, ETA) allows the BAP to present this to the user for explicit confirmation before changes are finalized.
    - **Informed and Controlled In-Ride Experience (Rider)**: Riders should have access to real-time updates about their ongoing ride, including current status (e.g., driver arriving, trip in progress), driver/vehicle information, live location on a map, and accurate ETAs. This is enabled by the BAP polling the BPP with `/status` and the BPP responding with `on_status`.
    - **Opportunities for Skill Enhancement and Continuous Learning**: Drivers should have access to learning modules that help them improve their skills and stay updated with industry trends.
    - **Locally Adapted Application Experience**: The driver application should adapt its behavior and UI based on city-specific configurations, ensuring compliance with local rules and providing a relevant operational experience.
    - **Flexible Meter Ride Management**: Drivers should have easy-to-use tools for managing meter-based rides, including updating destinations if needed and ensuring riders receive receipts.
    - **Streamlined Access to Operational Support (Hubs)**: Drivers should have a clear and easy way to find relevant operation hubs, submit requests for various services (e.g., maintenance, document issues), and track the progress of these requests.
    - **Understanding Fare Components**: Drivers should be able to clearly see how the final fare for each ride is constructed, including base rates, per-distance/time charges, and any additional fees or surcharges.
    - **Engaging and Informative Content Delivery (Reels)**: Drivers should have access to engaging short video content (Reels) for quick updates, tips, and announcements, improving information retention and platform engagement.
    - **Awareness of Special Operational Zones**: Drivers should be aware of designated special zones where they need to operate with specific rules or requirements.
    - **Effective Coordination at Special Locations (for "Warriors")**: Drivers should have clear guidelines and procedures for operating in designated special zones, including understanding the role of "Warriors" and how to interact with them.
    - **Efficient Fleet/Managed Service Operations (WMB)**: Drivers operating under a fleet or managed service model should have clear tools to discover assigned routes/tasks, manage their specific trips, communicate operational needs, and access fleet-related configurations and information.
    - **Reliable Communication with Customers**: // ... (existing content, supported by detailed call event logging for troubleshooting) ...
// ... (All other User Experience Goals, ensuring they are preserved) ...

_This document will evolve as a deeper understanding of the specific functionalities within each service is gained._ 