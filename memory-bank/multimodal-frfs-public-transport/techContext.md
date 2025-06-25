# Multimodal Public Transport (FRFS) Flow: Technical Context

This document provides an in-depth technical analysis of the modules involved in the Multimodal Public Transport (FRFS) flow, detailing their responsibilities and cross-file usages.

## Technical Context: Module Breakdown

### 1. FRFSTicketService Module

*   **Core File:** `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/FRFSTicketService.hs`
*   **API Specification:** `Backend/app/rider-platform/rider-app/Main/spec/API/FrfsTicket.yaml`
*   **Storage Specification:** `Backend/app/rider-platform/rider-app/Main/spec/Storage/FrfsTicket.yaml` (defines underlying data models like `FRFSSearch`, `FRFSQuote`, `FRFSTicketBooking` that `FRFSTicketService` operates on)

*   **In-Depth Role:** This module serves as the primary interface between the UI and the backend's FRFS (Free-Roaming Floating Share) public transport logic. It translates UI requests into internal domain actions and orchestrates the flow, often involving multiple sub-systems. It's responsible for data transformation between UI-specific API types and internal domain types.

*   **Key Functions & Their Usages:**
    *   **`getFrfsRoutes`, `getFrfsStations`, `getFrfsRoute`, `getFrfsAutocomplete`:** These functions handle fetching route and station information.
        *   **Usage:** Primarily called by UI-facing endpoints to populate maps, search suggestions, and route details. They query `Storage.CachedQueries.OTPRest.OTPRest` and `Storage.Queries.Route`/`RouteStopMapping` for data, and `Tools.Maps` for distance calculations.
    *   **`postFrfsSearch`, `postFrfsDiscoverySearch`:** Initiate the search for public transport options.
        *   **Usage:** Called when a user performs a search in the UI. They delegate the actual search to `ExternalBPP.CallAPI.search` and `ExternalBPP.CallAPI.discoverySearch`.
    *   **`postFrfsQuoteV2Confirm`, `postFrfsQuoteConfirm`, `postFrfsQuoteConfirmPlatformType`:** Handle the confirmation of a selected quote and initiation of the booking process.
        *   **Usage:** Triggered when a user confirms a booking. These functions interact with `Storage.Queries.FRFSQuote`, `Storage.Queries.FRFSTicketBooking`, and `Lib.Payment.Domain.Action` for payment order creation. They also make the `init` call to external BPPs via `ExternalBPP.CallAPI.init`.
    *   **`getFrfsBookingStatus`, `frfsBookingStatus`:** Retrieve the current status of a booking.
        *   **Usage:** Used to display real-time booking status to the user. They query `Storage.Queries.FRFSTicketBooking`, `Storage.Queries.FRFSTicketBookingPayment`, and `Lib.Payment.Storage.Queries.PaymentOrder`/`PaymentTransaction`. They also make `status` calls to external BPPs via `ExternalBPP.CallAPI.status`.
    *   **`cancelFRFSTicketBooking`, `postFrfsBookingCanCancel`, `getFrfsBookingCanCancelStatus`, `postFrfsBookingCancel`, `getFrfsBookingCancelStatus`:** Manage the cancellation flow.
        *   **Usage:** Called when a user attempts to cancel a booking. They interact with `Storage.Queries.FRFSTicketBooking` and `ExternalBPP.CallAPI.cancel`.
    *   **`postFrfsTicketVerify`:** Verifies a ticket using QR data.
        *   **Usage:** Used for ticket validation, typically by a conductor or scanning system. It calls `ExternalBPP.CallAPI.verifyTicket`.
    *   **`getFrfsConfig`:** Retrieves FRFS-specific configuration.
        *   **Usage:** Used by the UI to configure behavior based on city-specific FRFS settings. It queries `Storage.CachedQueries.FRFSConfig`.
    *   **Data Transformation:** Transforms internal domain models into UI-friendly API responses and vice-versa.

*   **Files Importing `Domain.Action.UI.FRFSTicketService` or `API.Types.UI.FRFSTicketService`:**
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/FRFS/OnSearch.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/FRFS/OnSelect.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/FRFS/OnUpdate.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Internal/FRFS.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/PartnerOrganizationFRFS.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/MultimodalConfirm.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Payment.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/CallAPI.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/FRFSUtils.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Subway/CRIS/BookJourney.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Direct/Order.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Bus/EBIX/Order.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/Lib/JourneyLeg/Common/FRFS.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/API/UI/PartnerOrganizationFRFS.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/Lib/JourneyModule/Types.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/Storage/Queries/StationExtra.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/Lib/JourneyLeg/Metro.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Quote.hs`

### 2. ExternalBPP Module

*   **Core Files:**
    *   `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/Flow.hs`
    *   `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/CallAPI.hs`

*   **In-Depth Role:** This module is the dedicated layer for interacting with external Beckn Provider Platforms (BPPs). It encapsulates the logic for making Beckn-compliant API calls and processing their responses, acting as a crucial intermediary between the internal application logic and the external Beckn network. It handles the complexities of different provider configurations (e.g., ONDC vs. direct integrations) and supports multiple providers for the same service type.

*   **Key Functions & Their Usages:**
    *   **`ExternalBPP.Flow.hs`:**
        *   **`search`, `select`, `init`, `confirm`, `status`, `verifyTicket`:** These functions implement the core Beckn protocol flows. They orchestrate the calls to `ExternalBPP.CallAPI` and then delegate the processing of `on_` responses to the respective `Domain.Action.Beckn.FRFS.On*` modules (e.g., `DOnSearch.onSearch`).
        *   **Multi-Provider Support:** The `search` function explicitly accepts a *list* of `IntegratedBPPConfig` and iterates through them, potentially calling provider-specific internal search logic concurrently (e.g., via `OTPRest`).
        *   **Usage:** Primarily called by `Domain.Action.UI.FRFSTicketService` to perform external Beckn operations. Also used by `Domain.Action.UI.NearbyBuses` for certain search functionalities.
    *   **`ExternalBPP.CallAPI.hs`:**
        *   **`discoverySearch`, `search`, `select`, `init`, `confirm`, `status`, `verifyTicket`, `getFares`, `getPaymentDetails`, `createOrder`, `getTicketStatus`:** These functions are responsible for constructing the actual HTTP requests to external BPPs and parsing their raw responses. They use `SharedLogic.CallFRFSBPP` for the underlying HTTP client functionality. They also dispatch to specific provider integration modules (e.g., `ExternalBPP.ExternalAPI.Subway.CRIS.BookJourney.createOrder`) based on the `IntegratedBPPConfig`.
        *   **Usage:** Called exclusively by `ExternalBPP.Flow.hs` to perform the low-level network interactions with BPPs.

*   **Files Importing `ExternalBPP.Flow` or `ExternalBPP.CallAPI`:**
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/PartnerOrganizationFRFS.hs` (imports `CallExternalBPP`)
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/NearbyBuses.hs` (imports `ExternalBPP.Flow`)
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/Beckn/FRFS/OnConfirm.hs` (imports `ExternalBPP.CallAPI`)
    *   `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/FRFSTicketService.hs` (imports `CallExternalBPP`)
    *   `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/CallAPI.hs` (imports `ExternalBPP.Flow` for internal calls)
    *   `Backend/app/rider-platform/rider-app/Main/src/Lib/JourneyLeg/Common/FRFS.hs` (imports `CallExternalBPP` and `ExternalBPP.Flow`)

### 3. FRFS Beckn Module (Conceptual)

*   **Core Components:** This is a conceptual module, implemented through a collection of files that collectively handle the Beckn protocol for FRFS.
    *   **ACL (Access Control Layer) Modules:** `Beckn.ACL.FRFS.Search`, `Beckn.ACL.FRFS.Select`, `Beckn.ACL.FRFS.Init`, `Beckn.ACL.FRFS.Confirm`, `Beckn.ACL.FRFS.Status`, `Beckn.ACL.FRFS.Cancel`, `Beckn.ACL.FRFS.OnUpdate`, `Beckn.ACL.FRFS.OnCancel`, `Beckn.ACL.FRFS.OnConfirm`, `Beckn.ACL.FRFS.OnInit`, `Beckn.ACL.FRFS.OnSearch`, `Beckn.ACL.FRFS.OnStatus`, `Beckn.ACL.FRFS.Utils`.
        *   **Role:** These modules are responsible for constructing the outgoing Beckn-compliant request payloads (e.g., `buildSearchReq`, `buildInitReq`). They ensure that the requests adhere to the Beckn schema and context.
        *   **Usage:** Primarily imported and used by `ExternalBPP.CallAPI.hs` to build requests before sending them to BPPs.
    *   **`on_` Action Modules:** `Domain.Action.Beckn.FRFS.OnSearch`, `Domain.Action.Beckn.FRFS.OnSelect`, `Domain.Action.Beckn.FRFS.OnInit`, `Domain.Action.Beckn.FRFS.OnConfirm`, `Domain.Action.Beckn.FRFS.OnStatus`, `Domain.Action.Beckn.FRFS.OnCancel`, `Domain.Action.Beckn.FRFS.OnUpdate`.
        *   **Role:** These modules process the incoming `on_` responses from BPPs. They validate the responses, extract relevant data, and update the internal application state (e.g., persisting quotes, updating booking statuses).
        *   **Usage:** Called by `ExternalBPP.Flow.hs` after receiving responses from `ExternalBPP.CallAPI.hs`.
    *   **Common Utilities:** `Domain.Action.Beckn.FRFS.Common.hs`, `BecknV2.FRFS.Enums.hs`, `BecknV2.FRFS.Utils.hs`.
        *   **Role:** Provide shared data types, enumerations (like `VehicleCategory`), and utility functions used across the Beckn integration.
        *   **Usage:** Widely imported across almost all Beckn-related modules, `FRFSTicketService`, and other domain modules.
    *   **Beckn HTTP Client:** `SharedLogic.CallFRFSBPP.hs`.
        *   **Role:** Provides the low-level HTTP client functionality for making calls to Beckn gateways and BPPs.
        *   **Usage:** Imported and used by `ExternalBPP.CallAPI.hs` to execute the actual network requests.

*   **Files Importing Beckn Components:**
    *   **`BecknV2.FRFS.Enums` / `BecknV2.FRFS.Utils`:** Used in almost all modules related to FRFS, including `SharedLogic`, `ExternalBPP`, `Domain.Action.UI`, `Domain.Action.Beckn.FRFS`, `Lib.JourneyLeg`, `Storage.Queries`, `Storage.CachedQueries`, and `API.Beckn.FRFS`.
    *   **`Beckn.ACL.FRFS.*`:** Primarily imported by `ExternalBPP.CallAPI.hs` and `API.Beckn.FRFS.*` (which define the API endpoints for `on_` calls).
    *   **`Domain.Action.Beckn.FRFS.*`:** Imported by `ExternalBPP.Flow.hs` (for processing `on_` responses), `ExternalBPP.CallAPI.hs` (for type definitions), and other `Domain.Action.UI` modules (e.g., `Domain.Action.UI.Miscellaneous` imports `OnConfirm`).
    *   **`SharedLogic.CallFRFSBPP`:** Imported by `ExternalBPP.CallAPI.hs` and `Domain.Action.Beckn.FRFS.OnSelect.hs`.

### 4. Journey Module & Multimodal Module (Conceptual)

*   **Core Components:** These are fundamental domain concepts rather than isolated modules, with their logic and data structures integrated across various layers.
    *   **Domain Types:**
        *   `Domain.Types.FRFSSearch.hs`: Represents a search request for a journey, including origin, destination, and `journeyLegInfo` (type `JourneySearchData`).
        *   `Domain.Types.FRFSTicketBooking.hs`: Represents a confirmed booking, which can be a multimodal journey, containing `journeyLegOrder`, `journeyId`, and `journeyRouteDetails` (type `MultiModalJourneyRouteDetails`).
        *   `Lib.JourneyLeg.Types.hs`: Defines core types like `JourneySearchData` (details about the overall journey search, including `journeyId`) and `MultiModalJourneyRouteDetails` (details for each segment/leg of a multimodal journey). It also defines `JourneyLegStatus`.
        *   `Lib.JourneyLeg.Types.Bus.hs`, `Lib.JourneyLeg.Types.Metro.hs`, `Lib.JourneyLeg.Types.Subway.hs`, `Lib.JourneyLeg.Types.Taxi.hs`, `Lib.JourneyLeg.Types.Walk.hs`: Define specific data structures and logic for different transport modes within a journey.
    *   **Storage Specifications:**
        *   `Backend/app/rider-platform/rider-app/Main/spec/Storage/FrfsTicket.yaml`: Defines the database tables for `FRFSSearch`, `JourneyRouteDetails`, and `FRFSTicketBooking`, explicitly including fields for `journeyLegInfo`, `journeyLegStatus`, and `journeyRouteDetails`. It also defines `Route`, `Station`, and `RouteStopMapping` which are foundational for journey planning.
    *   **Query Modules:**
        *   `Storage.Queries.Journey.hs`: (If present, would manage the `Journey` entity).
        *   `Storage.Queries.Route.hs` (`src-read-only/Storage/Queries/Route.hs`): Provides functions to query route information.
        *   `Storage.Queries.RouteStopMapping.hs` (`src-read-only/Storage/Queries/RouteStopMapping.hs`): Provides functions to query mappings between routes and stops.
        *   `Storage.Queries.FRFSSearchExtra.hs`, `Storage.Queries.FRFSTicketBookingExtra.hs`, `Storage.Queries.JourneyExtra.hs`, `Storage.Queries.JourneyLegExtra.hs`, `Storage.Queries.JourneyRouteDetails.hs`: These "Extra" query files extend the basic query capabilities for these core journey-related entities.
    *   **Service Logic & Utilities:**
        *   `Lib.JourneyLeg.Common.FRFS.hs`: Contains common logic for FRFS journey legs, including status updates and transformations.
        *   `Lib.JourneyModule.Base.hs`: Provides foundational logic for the overall journey module, including handling different leg types.
        *   `Lib.JourneyModule.Location.hs`: Deals with location-related aspects of journeys.
        *   `Lib.JourneyModule.Utils.hs`: Contains general utilities for the journey module.
        *   `Storage.Queries.Transformers.MultiModal.hs`, `Storage.Queries.Transformers.RouteDetails.hs`, `Storage.Queries.Transformers.SearchRequest.hs`, `Storage.Queries.Transformers.FRFSSearch.hs`: These modules handle transformations of data related to multimodal journeys and search requests for storage and retrieval.

*   **Files Importing Journey/Multimodal Components:**
    *   **`Domain.Types.FRFSSearch` / `Domain.Types.FRFSTicketBooking`:** Widely imported across `SharedLogic`, `ExternalBPP`, `Domain.Action.UI`, `Domain.Action.Beckn.FRFS`, `Lib.JourneyModule`, and `Storage.Queries`.
    *   **`Lib.JourneyLeg.Types` / `JourneySearchData` / `MultiModalJourneyRouteDetails`:** Imported by `SharedLogic.CreateFareForMultiModal`, `SharedLogic.Ride`, `Domain.Action.Internal.DriverArrivalNotf`, `API.UI.PartnerOrganizationFRFS`, `Lib.JourneyLeg.Common.FRFS`, `Domain.Action.Beckn.FRFS.OnSearch`, `Lib.JourneyLeg.Taxi`, `Lib.JourneyLeg.Walk`, `Lib.JourneyModule.Base`, `Domain.Action.UI.Search`, `Lib.JourneyModule.Location`, `Storage.Queries.Transformers.*`, and `Lib.JourneyModule.Types`.
    *   **`Storage.Queries.Journey` / `Storage.Queries.Route` / `Storage.Queries.RouteStopMapping`:** Imported by `Domain.Action.UI.FRFSTicketService`, `Domain.Action.UI.MultimodalConfirm`, `Domain.Action.UI.Booking`, `Domain.Action.UI.Frontend`, `Domain.Action.UI.Quote`, `Domain.Action.Dashboard.FRFSTicket`, `ExternalBPP.Flow`, `ExternalBPP.ExternalAPI.Direct/Bus/Metro/Subway/CallAPI`, `Lib.JourneyLeg.Taxi`, `Lib.JourneyLeg.Walk`, `Lib.JourneyModule.Base`, `Lib.JourneyModule.Utils`, `Storage.CachedQueries.OTPRest.OTPRest`.

### 5. External Provider Deep Dive

From the analysis of `ExternalBPP.ExternalAPI.Subway.CRIS.BookJourney.hs` and `ExternalBPP.ExternalAPI.Bus.EBIX.Order.hs`, and the presence of other files like `ExternalBPP.ExternalAPI.Metro.CMRL.*`, the system integrates with multiple external transport providers.

*   **Provider Integration Pattern:**
    *   Each provider (e.g., CRIS for Subway, EBIX for Bus, CMRL for Metro) has its own dedicated sub-module within `ExternalBPP.ExternalAPI`.
    *   These sub-modules define:
        *   **Specific API Endpoints:** Using Servant API types (e.g., `BookJourneyAPI`, `SaveMobTicketAPI`).
        *   **Provider-Specific Request/Response Data Structures:** (e.g., `CRISBookingRequest`, `SaveMobTicketReq`). These are tailored to the external provider's API contract.
        *   **Core Interaction Functions:** Functions like `getBookJourney` (for CRIS) or `createOrder` (for EBIX) encapsulate the logic for making calls to that specific provider.
        *   **Data Mapping:** These functions are responsible for mapping the internal domain models (like `FRFSTicketBooking`, `FRFSQuote`) to the provider's required request format and mapping the provider's response back to internal types (e.g., `ProviderOrder`, `ProviderTicket`).
        *   **Authentication/Security:** Some providers might require specific authentication mechanisms (e.g., CRIS uses encrypted payloads and an `Auth` module for token retrieval).
        *   **Error Handling:** Each integration handles errors specific to the external API.

*   **Abstraction by `ExternalBPP.CallAPI` and `ExternalBPP.Flow`:**
    *   The `ExternalBPP.CallAPI.hs` module acts as a facade over these individual provider integrations. It contains generic functions (`createOrder`, `getTicketStatus`, `verifyTicket`, `getFares`) that internally dispatch to the correct provider-specific implementation based on the `IntegratedBPPConfig` (which holds provider-specific configuration like `CRISConfig` or `EBIXConfig`).
    *   This design allows the higher-level `ExternalBPP.Flow.hs` and `Domain.Action.UI.FRFSTicketService.hs` modules to interact with external providers in a uniform manner, without needing to know the specifics of each provider's API.

### 6. Journey Planning Deep Dive

Journey planning in this system involves several layers, from fetching raw route/stop data to calculating optimal paths and presenting them to the user.

*   **Data Sources:**
    *   **OTPRest Client (Nandi):** As detailed below, this is a primary source for real-time or cached GTFS (General Transit Feed Specification) data (routes, stops, route-stop mappings).
    *   **Database Queries:** If OTPRest (Nandi) fails or doesn't have the data, the system falls back to querying its own database (e.g., `Storage.Queries.Route`, `Storage.Queries.RouteStopMapping`, `Storage.Queries.Station`). This indicates a caching or data redundancy strategy.
    *   **External Maps Service (`Tools.Maps`):** Used for calculating distances between locations, which is crucial for features like `getFrfsAutocomplete` (finding nearby stops) and fare calculation.

*   **Core Logic Flow (within `Domain.Action.UI.FRFSTicketService.hs`):**
    *   **`getFrfsRoutes`:** Retrieves possible routes between two stops or all routes for a vehicle type. It uses `OTPRest.getPossibleRoutesBetweenTwoStops` or `OTPRest.getRoutesByVehicleType`.
    *   **`getFrfsStations`:** Fetches stations based on various criteria (e.g., all stops, stops on a route, possible start/end stops). It heavily uses `OTPRest` functions like `getRouteStopMappingByStopCode`, `getRouteStopMappingByRouteCode`, `findAllByVehicleType`, and `findByStationCodeAndIntegratedBPPConfigId`. It also uses `Tools.Maps.tryStationsAPIWithOSRMDistances` to add distance information.
    *   **`getFrfsAutocomplete`:** Provides suggestions for routes and stops based on user input or proximity. It uses `OTPRest.findAllMatchingStations` and `OTPRest.getRouteByFuzzySearch` for fuzzy matching and `Tools.Maps` for distance filtering.
    *   **Data Transformation:** Converts raw route/station/stop data into `FRFSRouteAPI` and `FRFSStationAPI` types for the UI.

*   **Multimodal Aspects (`Lib.JourneyLeg.Types`, `Lib.JourneyModule.Base`, etc.):**
    *   The concept of `JourneySearchData` and `MultiModalJourneyRouteDetails` allows the system to represent complex journeys composed of multiple legs (e.g., walk, bus, metro, taxi).
    *   Functions in `Lib.JourneyLeg.Common.FRFS` and `Lib.JourneyModule.Base` likely handle the aggregation and processing of data from individual legs to form a complete multimodal journey.
    *   The `FRFSSearch` and `FRFSTicketBooking` domain models store these multimodal journey details, enabling the system to track and manage the entire trip.

### 7. OTPRest Client Deep Dive

*   **Core File:** `Backend/app/rider-platform/rider-app/Main/src/Storage/CachedQueries/OTPRest/OTPRest.hs`
*   **Underlying External System:** The comments and function names (e.g., `Nandi`) suggest that OTPRest is a client for an "in-memory server" or a service that provides GTFS (General Transit Feed Specification) data, likely for OpenTripPlanner (OTP) or a similar routing engine. This service seems to be named "Nandi" (`SharedLogic.External.Nandi.Flow`, `SharedLogic.External.Nandi.Types`).

*   **Purpose:** To provide cached and real-time access to public transport data (routes, stops, schedules) for journey planning. It acts as a data abstraction layer, attempting to fetch data from the external OTP/Nandi service first and falling back to the local database if the external call fails or returns no data.

*   **Key Functions:**
    *   **`executeFlowPerFeed`:** A generic helper to execute a Nandi flow for each `IntegratedBPPConfig` and aggregate the results. This is crucial for supporting multiple GTFS feeds/providers concurrently.
    *   **`getRoutesByRouteId`, `getRoutesByFuzzySearch`, `getRoutesByGtfsIds`, `getRoutesByVehicleType`:** Functions to retrieve route information. They all attempt to fetch from Nandi first and then fall back to `QRoute` queries.
    *   **`getRouteStopMappingsByRouteCode`, `getRouteStopMappingsByStopCode`, `getRouteStopMappingsByStopCodeAndRouteCode`:** Functions to retrieve route-stop mapping information. They query Nandi first and then fall back to `QRSM` queries.
    *   **`getStationsByGtfsIds`, `getStationsByGtfsIdAndStopCode`, `findByStationCodeAndIntegratedBPPConfigId`, `findAllByBppConfigId`, `findAllByVehicleType`, `findAllMatchingStations`:** Functions to retrieve station information. They follow the same pattern of querying Nandi first and falling back to `QStation` or `CQStation` queries.
    *   **`parseStationsFromInMemoryServer`, `parseRouteFromInMemoryServer`, `parseRouteStopMappingInMemoryServer`:** Helper functions to transform the data received from the Nandi (in-memory server) into the internal domain models.
    *   **Caching (`cacheAllRouteStopMapping`, `cacheByRouteId`):** The module includes functions for caching route stop mappings in Redis (`Hedis`), indicating a performance optimization strategy.

*   **Interaction:**
    *   `OTPRest.hs` primarily interacts with `SharedLogic.External.Nandi.Flow.hs` (the actual client for the Nandi service) and various `Storage.Queries` modules for database fallback.
    *   It is heavily used by `Domain.Action.UI.FRFSTicketService.hs` for all UI-facing route and station data retrieval.
    *   It's also used by provider-specific modules like `ExternalBPP.ExternalAPI.Bus.EBIX.Order.hs` to fetch route/stop details needed for constructing provider-specific requests.

### Comprehensive Flow Diagram (Updated to reflect Multi-Provider Support)

```mermaid
graph TD
    subgraph UI Layer
        UI[User Interface]
    end

    subgraph Backend Services
        AuthSvc[Authentication Service]
        FRFSTicketSvc[FRFSTicketService Module]
        MultimodalConfirmSvc[MultimodalConfirm Service]
        PaymentSvc[Payment Service]
        RiderLocationSvc[Rider Location Service]
    end

    subgraph External BPP Integration
        ExternalBPPFlow[ExternalBPP.Flow]
        ExternalBPPCallAPI[ExternalBPP.CallAPI]
        BecknACL[Beckn.ACL.FRFS.*]
        OnActionHandlers[Domain.Action.Beckn.FRFS.On*]
        CallFRFSBPP[SharedLogic.CallFRFSBPP]
        ProviderIntegrations[ExternalBPP.ExternalAPI.* (CRIS, EBIX, CMRL)]
    end

    subgraph Data & External Systems
        DB[Database (FRFSSearch, FRFSQuote, FRFSTicketBooking, IntegratedBPPConfig, etc.)]
        OTPRestClient[OTPRest Client (Nandi)]
        GTFSData[External GTFS/OTP Service (Nandi)]
        PaymentGateway[External Payment Gateway]
        MappingService[External Mapping Service (OSRM)]
    end

    UI -- 1. Mobile Number --> AuthSvc
    AuthSvc -- 2. authId --> UI
    UI -- 3. OTP --> AuthSvc
    AuthSvc -- 4. Token --> UI

    UI -- 5. Search Request (Origin/Dest, VehicleType, City, Platform) --> FRFSTicketSvc
    FRFSTicketSvc -- 5a. Retrieve ALL relevant IntegratedBPPConfig (for VehicleType, City, Platform) --> DB
    FRFSTicketSvc -- 5b. Create FRFSSearch --> DB
    FRFSTicketSvc -- 5c. Delegate Search (with LIST of IntegratedBPPConfig) --> ExternalBPPCallAPI

    ExternalBPPCallAPI -- 5d. Build Beckn Search Req (via BecknACL) / Call ExternalBPP.Flow.search (with LIST) --> CallFRFSBPP
    CallFRFSBPP -- 5e. Send Search Req --> BecknGateway[Beckn Gateway]
    BecknGateway -- 5f. Forward Search Req --> BPPs[External BPPs]

    ExternalBPPFlow -- 5g. Concurrent Search (via OTPRestClient/ProviderIntegrations for each IntegratedBPPConfig in LIST) --> OTPRestClient / ProviderIntegrations
    OTPRestClient -- Data Fetch (Routes, Stops) --> GTFSData
    OTPRestClient -- Data Fallback --> DB
    ProviderIntegrations -- Direct API Calls --> External BPPs (non-Beckn)

    BPPs -- 6a. on_search Callback --> BecknGateway
    BecknGateway -- 6b. Forward on_search --> OnActionHandlers
    OnActionHandlers -- 6c. Process on_search, Persist FRFSQuote (linked to IntegratedBPPConfig) --> DB

    FRFSTicketSvc -- 7. Get FRFSQuote from DB --> DB
    DB -- 7a. Return FRFSQuote --> FRFSTicketSvc
    FRFSTicketSvc -- 8. Return Quotes --> UI

    UI -- 9. Select Quote, Initiate Journey --> FRFSTicketSvc / MultimodalConfirmSvc
    FRFSTicketSvc / MultimodalConfirmSvc -- 9a. Create FRFSTicketBooking (linked to SELECTED IntegratedBPPConfig) --> DB
    FRFSTicketSvc / MultimodalConfirmSvc -- 9b. Delegate Init (with SELECTED IntegratedBPPConfig) --> ExternalBPPCallAPI

    ExternalBPPCallAPI -- 9c. Build Beckn Init Req (via BecknACL) / Call ExternalBPP.Flow.init (with SELECTED) --> CallFRFSBPP
    CallFRFSBPP -- 9d. Send Init Req --> BecknGateway
    BecknGateway -- 9e. Forward Init Req --> BPPs

    BPPs -- 10a. on_init Callback --> BecknGateway
    BecknGateway -- 10b. Forward on_init --> OnActionHandlers
    OnActionHandlers -- 10c. Process on_init, Update FRFSTicketBooking Status --> DB

    UI -- 11. Confirm Booking --> FRFSTicketSvc / MultimodalConfirmSvc
    FRFSTicketSvc / MultimodalConfirmSvc -- 11a. Initiate Payment (if needed) --> PaymentSvc
    PaymentSvc -- 11b. Interact --> PaymentGateway
    PaymentGateway -- 11c. Payment Status --> PaymentSvc
    FRFSTicketSvc / MultimodalConfirmSvc -- 11d. Delegate Confirm (with SELECTED IntegratedBPPConfig) --> ExternalBPPCallAPI

    ExternalBPPCallAPI -- 11e. Build Beckn Confirm Req (via BecknACL) / Call ExternalBPP.Flow.confirm (with SELECTED) --> CallFRFSBPP
    CallFRFSBPP -- 11f. Send Confirm Req --> BecknGateway
    BecknGateway -- 11g. Forward Confirm Req --> BPPs

    BPPs -- 12a. on_confirm Callback --> BecknGateway
    BecknGateway -- 12b. Forward on_confirm --> OnActionHandlers
    OnActionHandlers -- 12c. Process on_confirm, Update FRFSTicketBooking, Create FRFSTicket --> DB

    UI -- 13. View Journey Status / Location Update --> FRFSTicketSvc / RiderLocationSvc
    FRFSTicketSvc / RiderLocationSvc -- 13a. Delegate Status (with SELECTED IntegratedBPPConfig) --> ExternalBPPCallAPI

    ExternalBPPCallAPI -- 13b. Build Beckn Status Req (via BecknACL) / Call ExternalBPP.Flow.status (with SELECTED) --> CallFRFSBPP
    CallFRFSBPP -- 13c. Send Status Req --> BecknGateway
    BecknGateway -- 13d. Forward Status Req --> BPPs

    BPPs -- 14a. on_status Callback --> BecknGateway
    BecknGateway -- 14b. Forward on_status --> OnActionHandlers
    OnActionHandlers -- 14c. Process on_status, Update FRFSTicketBooking/FRFSTicket Status --> DB

    UI -- 15. Modify/Cancel Journey --> FRFSTicketSvc / MultimodalConfirmSvc
    FRFSTicketSvc / MultimodalConfirmSvc -- 15a. Delegate Cancel/Update (with SELECTED IntegratedBPPConfig) --> ExternalBPPCallAPI

    ExternalBPPCallAPI -- 15b. Build Beckn Cancel/Update Req (via BecknACL) / Call ExternalBPP.Flow.cancel/update (with SELECTED) --> CallFRFSBPP
    CallFRFSBPP -- 15c. Send Cancel/Update Req --> BecknGateway
    BecknGateway -- 15d. Forward Cancel/Update Req --> BPPs

    BPPs -- 16a. on_cancel/on_update Callback --> BecknGateway
    BecknGateway -- 16b. Forward on_cancel/on_update --> OnActionHandlers
    OnActionHandlers -- 16c. Process on_cancel/on_update, Update FRFSTicketBooking Status --> DB

    FRFSTicketSvc -- Uses --> MappingService
```

This updated explanation now explicitly details the support for multiple `IntegratedBPPConfig` instances, highlighting how the system leverages this capability for concurrent searches and precise routing of subsequent booking and management operations to the correct provider.