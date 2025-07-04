# Product Context: Namma Yatri

## 1. Problem Statement
Mobility is critical to economic growth, social progress, and individual well-being. Current mobility systems can be inefficient, unsustainable, and inaccessible to the masses. Namma Yatri aims to address these issues by creating a community-driven, open, tech-enabled, and shared mobility platform. It specifically focuses on empowering service providers (e.g., auto drivers) who are central to urban transport but often face challenges with existing platforms.

## 2. Vision & Mission
**Vision:**
Namma Yatri aims to empower service providers with a high-tech, cost-effective app and open data platform based on principles of:
1.  **Zero Commission**: Fair earnings for drivers.
2.  **Open**: Transparency and collaboration via open data, code, and networks.
3.  **Optimize**: Population scale growth with utility-like pricing through cost optimization.
4.  **Multimodal**: Support for various transport modes.
5.  **Shared Transport**: Promote shared mobility for sustainability.

**Mission (implied):**
To enable the careers of service providers like auto drivers by providing them with tools and a platform that respects their work and ensures fair compensation, while offering reliable and affordable mobility solutions for citizens. To build a mobility platform owned and driven by the community.

## 3. Target Audience & User Personas
-   **Primary Target Audience:**
    -   **Service Providers (Drivers):** Individual auto-rickshaw drivers, taxi drivers, and potentially drivers of other vehicle categories (bikes, delivery vehicles, buses, ambulances based on `VehicleCategory` and `ServiceTierType` enums). They seek fair earnings, transparent operations, good technology, and potentially subscription plans for platform access.
    -   **Citizens (Riders/Customers):** Urban and potentially inter-city travelers needing reliable, affordable, and accessible transportation for personal mobility and parcel delivery. They value fair pricing, safety features, and choice of service tiers.
    -   **Fleet Owners/Merchants:** Businesses or individuals managing multiple vehicles and drivers. They require tools for fleet management, driver onboarding, vehicle tracking, earnings management, and operational configuration.
    -   **Operators:** Entities responsible for managing specific routes or services, possibly in a public transport or fleet context.
-   **Secondary Target Audience:**
    -   **Platform Administrators/Support Teams:** Users of internal dashboards for managing drivers, merchants, configurations, resolving issues, and monitoring platform health.
    -   **Community Contributors:** Developers, designers, testers, and domain experts.
    -   **Local Authorities/City Planners:** Interested in sustainable and efficient urban mobility solutions, potentially leveraging open data.

*(User personas can be developed in more detail later if needed)*

## 4. Core Product Features
-   **Ride Hailing:**
    -   Driver and Rider mobile applications.
    -   Real-time ride matching and tracking.
    -   Dynamic Pricing: Incorporates supply/demand, QAR, congestion, and potentially A/B tested models. Surge pricing capabilities.
    -   Multiple Vehicle Categories & Service Tiers: Support for auto-rickshaws, taxis, SUVs, bikes, potentially ambulances, buses, with options like AC, Safety+, Pet-Friendly.
    -   Fare Estimation: Detailed estimates considering various factors.
    -   Scheduled Rides & Advance Bookings.
    -   Parcel Delivery: Support for sending and receiving parcels.
    -   In-app Communication: Likely includes call masking/VoIP features.
-   **Driver Empowerment & Management:**
    -   Zero-commission model (primary goal).
    -   Driver Onboarding & Verification: Includes Aadhaar, PAN, DL, RC, and potentially face verification.
    -   Subscription Plans: Drivers/Merchants can subscribe to plans with varying payment modes (manual, autopay) and frequencies.
    -   Driver Earnings & Payouts: Tracking earnings, bonuses, referral rewards, and managing payouts.
    -   Driver Stats & Performance: Tracking rides, distance, earnings, ratings, cancellation rates.
    -   Driver Go Home Feature.
    -   Intelligent Driver Pooling & Dispatch: Sophisticated algorithms for matching drivers to requests.
    -   Driver Support & Issue Management.
-   **Fleet Management (for Merchants/Fleet Owners):**
    -   Dashboard for managing drivers, vehicles, and fleet operations.
    -   Bulk onboarding for drivers and vehicles.
    -   Driver-vehicle association management.
    -   Fleet earnings tracking and reporting.
    -   Route and trip planning tools (especially for "WMB" context).
-   **Platform & Operational Features:**
    -   Open Data & Beckn Integration: Adherence to open network principles.
    -   Highly Configurable System: Business rules, pricing, service parameters, third-party integrations are configurable per merchant/city via database.
    -   Multi-City Operations: Support for different configurations and operations in various cities (`MerchantOperatingCity`).
    -   Communication System: Templated SMS, WhatsApp, Push Notifications, and In-App Overlays.
    -   Payment Integration: Support for various payment methods, auto-pay mandates, and payout processing.
    -   Security: Paseto tokens for auth, encryption for sensitive data.
    -   LLM Integration: For features like driver profile generation and support.
    -   Referral Program: For drivers and potentially fleet owners/operators.
    -   Loyalty/Coin System: Drivers can earn and use coins.
-   **Safety Features:**
    -   SOS functionality.
    -   Route deviation monitoring (especially for night safety).
    -   Safety+ ride options.
    -   Document verification for drivers and vehicles.

## 5. User Experience (UX) Goals
-   **For Drivers:**
    -   Empowerment and fair earnings.
    -   Easy-to-use and reliable application.
    -   Transparent information about earnings and platform operations.
-   **For Riders:**
    -   Reliable and accessible transportation.
    -   Fair and transparent pricing.
    -   Safe and efficient rides.
    -   Easy-to-use application.
-   **Overall:**
    -   Foster a sense of community ownership and participation.
    -   High reliability of the service.
    -   Rich user experience despite cost optimizations.
    -   Empathy and support for both citizens and drivers.

## 6. Competitive Landscape
**Competitors:**
-   Established ride-hailing platforms (e.g., Ola, Uber in the Indian context).
-   Other local or regional mobility applications.

**Namma Yatri's Unique Selling Propositions (USPs):**
-   **Zero Commission for Drivers:** A fundamental differentiator ensuring better earnings for drivers.
-   **Openness:** Open data, open code, and open networks foster trust and community involvement.
-   **Community-First Approach:** Focus on building a platform owned by citizens and drivers.
-   **Sustainable Growth:** Avoids unsustainable tactics like deep discounts, focusing on long-term viability.
-   **Cost Optimization for Utility-like Pricing:** Aims to provide affordable services through efficiency.
-   **Focus on Empowering Service Providers:** Directly addresses the needs and economic well-being of drivers.

## NY Regular Subscriptions

### Feature Overview

The "NY Regular" feature allows users to set up recurring ride subscriptions. This is designed for users who have a regular commute, such as from home to work, and want to automate the process of booking a ride.

### Key Concepts

- **Subscription:** A user-defined schedule for a recurring ride. It includes pickup and drop-off locations, time of day, and the days of the week the ride is needed.
- **Instance:** A single occurrence of a ride within a subscription. For example, if a user has a subscription for every weekday, Monday's ride is one instance, Tuesday's is another, and so on.
- **Automation:** The system will automatically initiate a search for a ride and book it based on the subscription schedule.

### Data Models

#### `NyRegularSubscription`

This table stores the core information about a user's subscription.

- **Key Fields:**
    - `id`: Unique identifier for the subscription.
    - `userId`: The user who created the subscription.
    - `pickupLocation`, `dropoffLocation`: The start and end points of the ride.
    - `startDatetime`: When the subscription becomes active.
    - `recurrenceRuleDays`: The days of the week the subscription is active (e.g., Monday, Wednesday, Friday).
    - `scheduledTimeOfDay`: The time the ride should be scheduled for.
    - `recurrenceEndDate`: When the subscription expires.
    - `status`: The current state of the subscription (e.g., `ACTIVE`, `PAUSED`, `CANCELLED`).

#### `NyRegularInstanceLog`

This table logs each automated attempt to book a ride for a subscription.

- **Key Fields:**
    - `instanceTransactionId`: Unique identifier for this specific ride instance. (This ID is also used as `context.transaction_id` for the Beckn flow of this instance).
    - `nyRegularSubscriptionId`: A foreign key linking to the `NyRegularSubscription`.
    - `scheduledPickupTime`: The specific date and time this instance was scheduled for.
    - `automationStatus`: The outcome of the automated booking attempt (e.g., `PENDING`, `SEARCH_SENT`, `BOOKING_INITIATED`, `CONFIRMED`, `FAILED_NO_OFFER`, `FAILED_BPP_ERROR`).

### Automated Booking Flow for NYRegular Instances

The system employs a Master/Child scheduler pattern to automate bookings:

1.  **Master Scheduler Job (`NyRegularMasterSchedulerJob` - daily):**
    *   Identifies all active `NyRegularSubscription`s.
    *   For each subscription, calculates all individual ride instances due for the upcoming day (or a defined look-ahead period).
    *   Enqueues a "Child Job" for each distinct ride instance into a persistent job queue (e.g., `RegularRideInstanceJobs` table). The payload includes subscription details, target pickup time, fixed price, and target BPP.
    *   Updates the parent `NyRegularSubscription`'s `nextScheduledTriggerAt` to prevent re-processing for the current cycle.

2.  **Child Job (`ProcessSingleNyRegularInstanceJob` - triggered at `targetExecutionTime - buffer`):**
    *   Retrieves details for a single ride instance from the job queue.
    *   Generates a unique `instance_transaction_id` (which will be the `SearchRequest.id` and Beckn `transaction_id`).
    *   Logs an entry in `ny_regular_instance_log` with status `PENDING` or `SEARCH_SENT`.
    *   **Initiates Beckn `/search`:**
        *   Constructs a Beckn `/search` message.
        *   **Crucially, injects:**
            *   `context.transaction_id = instance_transaction_id`.
            *   The agreed `fixedPrice` details from the `NyRegularSubscription`.
            *   Special tags (e.g., `NYREGULAR_INSTANCE = true`, `NYREGULAR_FIXED_PRICE = <price>`) to inform the BPP.
        *   Sends this tagged `/search` to the specific BPP stored in the `NyRegularSubscription` (using `SharedLogic.CallBPP.searchV2`). This creates a standard `SearchRequest` record in the BAP.

3.  **BPP Handles Tagged `/search`:**
    *   The target BPP (`dynamic-offer-driver-app`) must be modified to:
        *   Recognize the NYRegular tags and fixed price in the incoming `/search`.
        *   Respond with an `/on_search` message containing an offer that honors this fixed price.

4.  **BAP's `on_search` Handler Automation (`rider-app/.../Domain.Action.Beckn.OnSearch.hs`):**
    *   When processing the BPP's `/on_search` response:
        *   Identifies that this `on_search` corresponds to an NYRegular instance (e.g., by checking a flag on the `SearchRequest` linked to the `instance_transaction_id`, or by BPP echoing tags).
        *   If the BPP's offer matches the expected fixed price:
            *   **Automatically triggers the Beckn `/select` flow.** It does this by programmatically calling the equivalent of `Domain.Action.UI.Select.select2` with the BPP's offer.
        *   If the price doesn't match or no suitable offer, it logs a failure in `ny_regular_instance_log` and notifies the user.

5.  **Standard Beckn Flow Continuation:**
    *   Once the `/select` is auto-triggered by the `on_search` handler, the rest of the Beckn flow (`select` to BPP -> `on_select` from BPP -> `init` to BPP -> `on_init` from BPP -> `confirm` to BPP -> `on_confirm` from BPP) proceeds as per the standard, existing handlers in both BAP and BPP.
    *   The BAP's existing Beckn handlers will update the `Booking` and `Ride` entities as usual.

6.  **Outcome Logging & Notification:**
    *   The Child Job (or a monitoring mechanism listening to booking events) updates the `ny_regular_instance_log.automationStatus` to `CONFIRMED` or an appropriate `FAILED_` status.
    *   The user is notified of the success or failure of their scheduled ride booking.
    *   The parent `NyRegularSubscription`'s `lastBookingId` and `lastBookingStatus` are updated.
