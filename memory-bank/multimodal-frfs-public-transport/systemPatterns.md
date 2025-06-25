# Multimodal Public Transport (FRFS) Flow: System Patterns

This document outlines the key system patterns observed in the Multimodal Public Transport (FRFS) booking system.

## System Patterns

1.  **Token-Based Authentication:** The flow starts with an `authId` for initial authentication, followed by an OTP verification to obtain a `token`. This `token` is then used as an API key in the header for all subsequent authenticated requests, indicating a stateless, secure authentication mechanism (likely JWT).

2.  **Journey-Centric Design:** The core of the system revolves around a "Journey" concept, identified by a `journeyId`. Most operations (initiate, confirm, info, payment, location update, extend, switch, cancel, skip, add skipped leg) are performed on a specific journey. The `FRFSSearch` and `FRFSTicketBooking` domain models are central to this, storing `journeyLegInfo` and `journeyRouteDetails` for multimodal trips.

3.  **Multimodal Orchestration & Beckn Integration:**
    *   The `platformType: "MULTIMODAL"` and the various search and modification endpoints (e.g., `Switch Mode`, `Extend Leg`) clearly indicate support for journeys involving multiple modes of transport (e.g., walk, bus, metro, taxi).
    *   The system integrates with external Beckn Provider Platforms (BPPs) using the Beckn protocol. This involves `search`, `select`, `init`, `confirm`, `status`, `cancel`, and `verifyTicket` calls.
    *   **Crucially, the system supports concurrent integration with **Multiple IntegratedBPPConfig** instances for a given `vehicleType` in a `city` and `Platform`. This allows the system to query and aggregate results from multiple providers for the same transport mode (e.g., multiple bus operators) in parallel, offering a richer set of options to the user.**

4.  **State Management via IDs:** The system relies heavily on passing IDs (`authId`, `searchId`, `journeyId`, `bookingUpdateRequestId`) between API calls. These IDs are managed as environment variables in Postman, suggesting a backend that maintains state associated with these IDs, allowing for a conversational or stateful interaction from the client's perspective, even if the individual API calls are stateless.

5.  **Pre-request/Test Scripts for Workflow Automation:** The Postman collection extensively uses pre-request and test scripts to dynamically set environment variables (like `authId`, `token`, `searchId`, `journeyId`, `source-lat`, `source-lon`, etc.) based on previous responses or predefined values. This is a common pattern for automating API workflows and demonstrating dependencies between calls.

6.  **Location-Based Services:** The frequent use of `lat` and `lon` coordinates in search and location update requests highlights the importance of location-based services for public transport and multimodal routing. `Tools.Maps` is used for distance calculations.

7.  **Leg-Based Journey Structure:** Journeys are composed of "legs," allowing for granular control over individual segments of a multimodal trip (e.g., skipping a leg, extending a leg, switching mode for a leg). `Lib.JourneyLeg.Types` defines the core data structures for this.

8.  **Fare Calculation and Updates:** There are explicit calls for getting fare estimates (`Extend Leg - Get Fare`) and confirming journey details with fare information, indicating a robust pricing and payment integration.

9.  **Real-time Updates:** The "Journey Info" and "Journey Rider Location Update" endpoints suggest a need for real-time or near real-time updates on journey status and rider location, crucial for a public transport application.

10. **Data Fallback and Caching:**
    *   The `OTPRest` client (Nandi) prioritizes fetching real-time GTFS data from an external in-memory server.
    *   If the external service fails or lacks data, the system gracefully falls back to querying its local database (`Storage.Queries.Route`, `Storage.Queries.Station`, `Storage.Queries.RouteStopMapping`).
    *   Caching mechanisms (e.g., Redis via `Hedis` in `OTPRest`) are employed to store frequently accessed data, improving performance and reducing reliance on external calls.

11. **Provider-Specific Integrations:** Each external transport provider (e.g., CRIS for Subway, EBIX for Bus, CMRL for Metro) has its own dedicated sub-module within `ExternalBPP.ExternalAPI`. These modules encapsulate provider-specific API definitions, request/response schemas, data transformations, and security mechanisms (e.g., encryption for CRIS). This modularity ensures that changes to one provider's API do not impact others, adhering to the **Open/Closed Principle**.
