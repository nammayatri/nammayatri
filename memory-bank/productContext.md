# Product Context

_This document explains why this project exists, the problems it solves, how it should work, and user experience goals._ 

## Core Problem Statement
To create a reliable, efficient, and open platform for connecting users with transportation providers and related services, centered around a core `rider-app` backend that orchestrates the entire rider journey and supports internal operational needs like processing journey stop events. This includes secure authentication (with options like OTP, social login, and Aadhaar-based eKYC for identity verification), comprehensive search, clear quote presentation, decisive selection, robust payment processing, final booking confirmation, in-ride management, comprehensive post-ride feedback, easy access to financial records and personal usage statistics, personalized user profile management with safety features, a robust SOS system, effective grievance management, direct support channels, detailed logging of call events, clear cancellation processes (supported by standardized cancellation reasons), multilingual support via a translation proxy, dynamically configurable UI elements, various utility endpoints, and diverse integrations – all supported by a robust event-driven architecture, specialized shared libraries, and rich integrations.

## Key Problems Solved
- **Secure User Onboarding and Access**: The `rider-app` (via `API.UI.Registration` and `Domain.Action.UI.Registration`) provides a secure and multi-faceted user authentication system:
    - **Initial Authentication**: Users can initiate authentication using an identifier (e.g., phone number). The system generates and sends an OTP via SMS (or a similar mechanism). A `RegistrationToken` is created server-side to manage the state of this attempt, including the OTP, its expiry, and the user identifier. This flow also supports an alternative signature-based authentication for requests originating from trusted SDKs, providing an additional layer of security.
    - **Client Contextualization**: Information like client application version, bundle version, OS, device type, and IP address are captured during authentication initiation, allowing for tailored responses, analytics, or fraud detection.
    - **Verification**: Users verify their identity by submitting the OTP. The system validates this against the stored `RegistrationToken`. Upon success, a session token (e.g., JWT via Passetto) is generated, establishing an authenticated session.
    - **Session Management**: Authenticated sessions are managed using these tokens, enabling secure access to protected rider functionalities. Invalid or expired tokens are rejected.
    - **OTP Resend**: A mechanism to resend OTPs is provided, likely with rate limiting or other controls to prevent abuse.
    - **Logout**: Users can securely terminate their sessions, which invalidates their current session token.
    This multi-step process with OTPs and robust session management ensures that only legitimate users can access the platform and their personal data, while also providing flexibility for different client types (app vs. SDK).
- **Comprehensive Ride Discovery**: The `rider-app` (via `API.UI.Search` and `Domain.Action.UI.Search`) provides powerful search capabilities for various transport needs:
    - **Standard Ride Search**: Allows users to search for conventional ride-hailing options (e.g., taxis). The platform then broadcasts Beckn search requests to BPPs to gather quotes. The system attempts to manage and cancel prior conflicting search requests to improve user experience if configured.
    - **Multimodal Search**: Enables users to find journeys combining different transport modes, including public transport (buses, subways). This involves integrating with external multimodal routing engines (e.g., OTP-like services), processing complex journey legs (using `Lib.JourneyModule`), and specific integrations like CRIS for Indian Railways subway information (fares, SDK tokens). User preferences for modes, walk distances, and sorting types (e.g., fastest) are considered to tailor results.
    - **Rate Limiting**: Protects the search functionality from abuse by applying user-specific limits, with operational alerts (Slack) for excessive use.
    - **Client Contextualization**: Captures client version and device details during search for analytics and compatibility.
    - **IMEI Handling (Multimodal)**: For multimodal searches involving specific providers like CRIS, IMEI (encrypted) might be used as part of the request parameters, highlighting a specific integration requirement.
    This dual-search capability aims to provide riders with a wide array of transport solutions, from simple point-to-point taxi rides to complex, multi-leg public transit journeys.
- **Presentation of Available Ride Options (Quotes)**: After a search is initiated (which may involve asynchronous calls to multiple BPPs for standard search, or complex route processing for multimodal search), the `rider-app` (via `API.UI.Quote` and `Domain.Action.UI.Quote`) provides a mechanism for the user to retrieve and view the consolidated list of available ride offers (quotes). 
    - **Receiving Quotes from Providers (Beckn `on_search`)**: After `rider-app` (as a BAP) broadcasts a search request to BPPs, it receives their responses via the Beckn `on_search` callback endpoint (`API.Beckn.OnSearch`).
        - Each `on_search` message from a BPP contains their catalog of offers/quotes for the requested search (`transactionId` links it to the original `SearchRequest`).
        - `rider-app` validates the context of this callback against the original search and transforms the Beckn payload into internal domain objects.
        - It employs Redis locks to handle potential duplicate messages from BPPs and processes the offers asynchronously.
        - Validated offers are then stored (e.g., as `Estimate` entities), making them available for the rider to retrieve via the `API.UI.Quote` endpoint (`GET /v2/rideSearch/{searchId}/results`).
        - This ensures that the user receives a comprehensive set of quotes from participating providers to compare and choose from.
    - **Functionality**: The `GET /v2/rideSearch/{searchId}/results` endpoint allows the client to poll for these results using the `searchId` obtained from the search initiation.
    - **Data Aggregation**: The backend logic in `Domain.Action.UI.Quote.getQuotes` is responsible for querying all relevant stored quotes (e.g., `Estimate` entities) linked to the `searchId`, which represent responses from BPPs or processed multimodal journey options.
    - **Filtering & Presentation**: It may apply filtering (e.g., based on the `allowMultiple` query parameter, which could influence whether multiple offers from a single provider are shown) or ranking before returning the list of `DQuote.OfferRes` objects. This allows the user to compare options based on critical factors like fare, ETA, vehicle type, and provider before making a selection.
    - **Quote Validity**: The system needs to ensure that only currently valid (non-expired) quotes are presented to the user.
- **Clear Ride Option Selection & Provider Acknowledgment**: After reviewing quotes, the `rider-app` (via `API.UI.Select` and domain actions `Domain.Action.UI.Select`/`Domain.Action.UI.Cancel`) facilitates a crucial intermediate step before final booking:
    - **User Choice**: Riders can explicitly select their preferred ride option (an `Estimate` identified by `estimateId`) using `POST /v2/estimate/{estimateId}/select` (or `/select2`).
    - **Communication with Provider (Beckn "select")**: This selection is communicated to the chosen BPP by sending a Beckn "select" message. This informs the provider of the user's intent and allows the BPP to prepare for a potential booking (e.g., by provisionally holding the resource).
    - **Response Management**: The system provides a `selectTtl` (Time-To-Live) calculated from `BecknConfig`, guiding the client on how long to await the BPP's `on_select` response (which would confirm or update the terms of the selected offer).
    - **Polling for Selection Outcome**: Riders can use `GET /v2/estimate/{estimateId}/results` to poll for the BPP's `on_select` response (e.g., updated quote, confirmation of terms).
    - **Flexibility to Cancel**: Users can cancel their selection (`POST /v2/estimate/{estimateId}/cancel`) before proceeding to final booking. This may also trigger a cancellation message to the BPP.
    - **Handling Special Cases**: A specific flow for `POST /v2/estimate/{estimateId}/rejectUpgrade` allows users to decline a provider's proposed upgrade, and this action is tagged using `Yudhishthira` for tracking or future rule application.
    - **Provider Confirmation of Selection (Beckn `on_select`)**: After the rider makes a selection and `rider-app` sends a "select" message to the BPP, the BPP responds with an `on_select` callback. `API.Beckn.OnSelect` in `rider-app` handles this:
        - It validates the `on_select` message (context, signature) and transforms it into an internal domain object.
        - It uses Redis locks for idempotent processing of the callback.
        - Crucially, `Domain.Action.Beckn.OnSelect.validateRequest` checks if the BPP's confirmed offer in `on_select` matches or is acceptable relative to the original quote the user selected. This might involve comparing price, terms, and other critical details.
        - If valid, `Domain.Action.Beckn.OnSelect.onSelect` updates the stored `Estimate` (the selected quote) with the BPP-confirmed final details. This makes the offer ready for the user to proceed to the next step: formalizing their booking intent (Beckn "init" via `API.UI.Confirm`).
        - The raw `on_select` is logged for audit (e.g., ONDC).
    This ensures that before the user formally initiates the booking, the BPP has re-confirmed the terms of the selected offer.

- **Formalizing Booking Intent (Beckn Init)**: After a user selects a quote, the `rider-app` (via `API.UI.Confirm` and `Domain.Action.UI.Confirm`) allows the user to confirm their intent to book. This action:
    - Triggers a Beckn "init" message to the chosen BPP, signaling the user's commitment.
    - Creates a preliminary booking record in the system.
    - Provides the client with a Time-To-Live (TTL) for the BPP's response (`on_init`), setting expectations for the next step towards final booking confirmation.
    - Includes error handling to attempt cancellation of the preliminary booking if the "init" call to the BPP fails.
    - **Provider Acknowledgment & Auto-Confirmation Step (Beckn `on_init` leading to BAP `confirm`)**: After the BAP (`rider-app`) sends an "init" message, the BPP responds with an `on_init` callback. `API.Beckn.OnInit` in `rider-app` handles this crucial step:
        - It validates the `on_init` message (context, signature) and transforms the BPP's finalized order details (price, terms, etc.) into internal domain objects.
        - It uses Redis locks for idempotent processing.
        - `Domain.Action.Beckn.OnInit.onInit` processes these details, updating the local preliminary booking record with the BPP-confirmed information.
        - **Crucially, upon successful processing of `on_init`, `rider-app` automatically constructs and sends a Beckn "confirm" message to the BPP.** This action signifies the BAP's final commitment to the booking based on the `on_init` details.
        - **Robust Error Handling**: If sending this automatic Beckn "confirm" message fails, the system attempts to roll back by initiating a cancellation of the booking (both locally and by sending a Beckn "cancel" to the BPP). This aims to prevent inconsistent states where the BAP thinks it has confirmed but the BPP hasn't received it.
        - The raw `on_init` is logged for audit.
    This automated step streamlines the booking process from the BAP's perspective once the BPP provides its final terms in `on_init`, moving directly to confirm the booking.

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

- **End-to-End Rider Journey Management**: `rider-app` manages the complete lifecycle.
- **Accessibility of Transport**: Language translation support further enhances accessibility for a wider audience.
- **Rider Experience**: Accurate tracking of stop events in multimodal or multi-stop rides leads to a smoother and more predictable user experience.
- **Payment Facilitation**: `rider-app` integrates with the `payment` library, handles Juspay webhooks, manages payment methods/intents, executes final payment orders, facilitates BBPS payments, and processes payments for public transport tickets (including FRFS).
- **Real-time Event Handling**: Stop events are a key type of real-time event in journey management.
- **Location and ETA**: Processing stop events is essential for updating ETAs during a trip.
- **Geographically-Aware Operations**: Applies rules from `special-zone` via logic invoked through its various API flows.
- **Safety and Support**: `rider-app` integrates safety features (SOS, emergency contacts in profile), offers various issue management channels (including Beckn IGM and direct support), detailed call event logs, and further enhances support through Kapture CRM integration.
- **Operational Communication & Monitoring**: Stop event data aids in monitoring trip progress and operational efficiency.
- **Consistent Policy Enforcement & Decision Making**: `rider-app` calls `yudhishthira` when processing API requests.
- **Integration with Broader Networks & Standardization**: `rider-app` uses `beckn-services` via its `API.Beckn` modules for BAP outbound calls and handles all inbound Beckn callbacks, adhering to the protocol. It also integrates with national payment systems like BBPS and transit systems like CRIS, and supports FRFS data models for public transport ticketing.
- **Easy and Accurate Location Input**: Users can easily find and select specific points of interest or addresses for their pickup and drop-off locations using a targeted place search, reducing errors and improving the accuracy of ride requests.
- **Reduced Waiting Anxiety for Public Transport**: By seeing the live location of their bus or other public transport vehicle on a specific route, users can better estimate its arrival, reducing uncertainty and making the PT experience more pleasant and predictable.
- **Clear Visual Guidance for Journeys**: Users can see the planned path for their rides (including driver-to-pickup and main trip segments) on a map, providing clarity and confidence in the service.
- **Early Feedback on Service Availability**: Users are informed upfront if their desired pickup or drop-off locations are serviceable, or if their trip is considered intercity, preventing frustration from failed searches and setting correct expectations about service scope and potential pricing differences.
- **Intuitive and Accurate Location Input**: Users should find it extremely easy to specify their pickup and drop-off locations through features like reliable address autocomplete, the ability to get details for a chosen place, and accurate reverse geocoding when interacting with a map.
- **Responsive and Context-Aware UI**: The application should feel intelligent and adapt to the user's current state or progress in a flow, providing relevant information and actions. Querying flow status enables this.
- **Continuous Improvement through Usage Insights**: Tracking key frontend events helps the platform team understand user behavior, identify areas for improvement in the UI/UX, and measure the effectiveness of features.
- **Clear and Controlled In-Ride Modifications**: Users should have a transparent process for requesting changes like a new destination during a ride, understanding any fare implications, and explicitly confirming these changes before they are applied.
- **Convenient and Secure Rider-Driver Communication**: // ... (existing detailed content) ...
    - **Improved Debugging and Analytics for Calls**: // ... (existing detailed content) ...
- **Seamless Ride Lifecycle**: Orchestrated by `rider-app`.
- **Comprehensive Support**: // ... (existing content) ...
- **Real-time Financial Clarity**: // ... (existing content) ...
- **Rich Feature Set**: // ... (existing content) ...
- **Personalization**: Including the ability to specify accessibility requirements.
- **Reliability & Consistency**: // ... (existing content, including reliable FRFS B2B services) ...

_This document will evolve as a deeper understanding of the specific functionalities within each service is gained._ 