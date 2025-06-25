# NammaYatri Complete Ride Flow Documentation

## Overview

NammaYatri is an open mobility platform that connects riders with transportation providers through the Beckn protocol. This document provides a comprehensive overview of how the entire ride flow works, from search to completion, including detailed backend interactions and technical implementation.

## Architecture Overview

The platform consists of:
- **BAP (Beckn Application Platform)**: `rider-app` - handles rider-facing interactions
- **BPP (Beckn Provider Platform)**: `dynamic-offer-driver-app` - handles driver/provider operations
- **Frontend**: PureScript-based mobile applications for riders and drivers
- **Beckn Protocol**: Standardized API for discovery and transaction flows

### Core Technologies

The NammaYatri backend is built primarily with **Haskell**, ensuring a strongly-typed and robust system:

- **Haskell**: Primary language for backend services with strong type safety
- **PostgreSQL**: Persistent data storage managed via Beam ORM
- **Redis**: Caching, transient data (OTPs, rate limiting), distributed locks, geospatial queries
- **Apache Kafka**: Asynchronous event streaming between services
- **Nix Flakes**: Reproducible builds and dependency management
- **Namma DSL (`alchemist`)**: YAML-based DSL for API endpoints, data models, and database schemas
- **Beckn Protocol**: Open-source protocol for seamless network participant communication
- **`yudhishthira`**: Central rule engine for complex business rules and dynamic pricing
- **`external` library**: Third-party service integrations (SMS, FCM, maps, payments)
- **`location-updates` library**: Real-time driver location data processing
- **`special-zone` library**: Geographic zone configurations affecting pricing and availability

## Status Definitions

### Booking Status (Rider App)
```haskell
data BookingStatus
  = NEW                    -- Initial booking created
  | CONFIRMED             -- Booking confirmed, waiting for driver assignment
  | AWAITING_REASSIGNMENT -- Waiting for driver reassignment
  | REALLOCATED          -- Booking reallocated to different driver
  | COMPLETED            -- Booking completed successfully
  | CANCELLED            -- Booking cancelled
  | TRIP_ASSIGNED        -- Driver assigned to the trip
```

### Ride Status (Provider App)
```haskell
data RideStatus 
  = UPCOMING    -- Scheduled ride not yet started
  | NEW         -- Ride created, driver assigned
  | INPROGRESS  -- Ride in progress
  | COMPLETED   -- Ride completed
  | CANCELLED   -- Ride cancelled
```

### Journey Leg Status (Multimodal)
```haskell
-- Backend/app/rider-platform/rider-app/Main/src/Lib/JourneyModule/Types.hs
data JourneyLegStatus
  = InPlan           -- Planned but not yet booked
  | Assigning        -- Looking for driver/provider
  | Booked           -- Successfully booked
  | AtRiskOfMissing  -- At risk of missing the service
  | Missed           -- Service missed
  | Delayed          -- Service delayed
  | Arriving         -- Driver/vehicle arriving
  | Arrived          -- Driver/vehicle has arrived
  | OnTheWay         -- Driver on the way to pickup
  | Skipped          -- Leg skipped
  | Ongoing          -- Currently in progress
  | Finishing        -- About to complete
  | Cancelled        -- Cancelled
  | Completed        -- Successfully completed
```

## Complete Ride Flow (8 Phases)

### Phase 1: Search & Discovery

#### 1.1 Rider Initiates Search
**Frontend (Rider App)**
```purescript
-- User inputs origin, destination, preferences
-- Triggers ride search request
```

**Backend (rider-app - BAP)**
```haskell
-- API.UI.Search
POST /v2/rideSearch (standard taxi rides)
POST /v2/multimodalSearch (public transport options)
-- Domain.Action.UI.Search.search
-- Creates SearchRequest entity
-- Uses TaxiACL.buildSearchReqV2 to construct Beckn search request
```

#### 1.2 BAP Broadcasts Search
**Backend (rider-app)**
```haskell
-- CallBPP.searchV2 sends Beckn /search to BPPs
-- Asynchronous broadcast to multiple providers
-- Includes rider preferences and location context
```

#### 1.3 BPP Processes Search
**Backend (dynamic-offer-driver-app - BPP)**
```haskell
-- API.Beckn.Search receives /search request
POST /v2/beckn/{merchantId}/search
-- SignatureAuth validates digital signature
-- Domain.Action.Beckn.Search.handler processes request
-- Integrates with yudhishthira rule engine for dynamic pricing
-- Uses Redis locks for idempotency (searchLockKey, searchProcessingLockKey)
```

**Process:**
1. BPP receives search request
2. Finds available drivers near pickup location
3. Calculates fare estimates using fare policies
4. Creates driver quotes/estimates

#### 1.3 Driver Quote Generation
**Backend (dynamic-offer-driver-app)**
```haskell
-- Identifies available drivers based on location and preferences
-- Applies dynamic pricing rules using yudhishthira
-- Considers real-time factors: demand, supply, time, special zones
-- Generates multiple varied quotes/offers
```

#### 1.5 BPP Responds with Quotes
**Backend (dynamic-offer-driver-app)**
```haskell
-- Constructs Beckn on_search callback
-- Beckn.ACL.OnSearch.mkOnSearchRequest creates response
-- Callback.withCallback sends to BAP's callback URI
```

**Backend (rider-app)**
```haskell
-- API.Beckn.OnSearch receives quotes
-- Domain.Action.Beckn.OnSearch.onSearch
-- Validates context against original SearchRequest
-- Creates Estimate and Quote records asynchronously
```

### Phase 2: Quote Selection

#### 2.1 Rider Views Quotes
**Frontend (Rider App)**
```purescript
-- Display available quotes to user
-- User can compare prices, ETAs, driver ratings
```

**Backend (rider-app)**
```haskell
-- API.UI.Quote
GET /v2/rideSearch/{searchId}/results
-- Returns available quotes with fare breakdowns
-- Aggregated from multiple BPP responses
```

#### 2.2 Rider Selects Quote
**Frontend (Rider App)**
```purescript
-- User selects preferred quote
-- Triggers select action
```

**Backend (rider-app)**
```haskell
-- API.UI.Select
POST /v2/estimate/{estimateId}/select
-- Domain.Action.UI.Select.select
-- Sends Beckn /select to chosen BPP
```

#### 2.3 BPP Processes Selection
**Backend (dynamic-offer-driver-app)**
```haskell
-- API.Beckn.Select receives /select request
POST /v2/beckn/{merchantId}/select
-- Domain.Action.Beckn.Select.validateRequest validates offer
-- Domain.Action.Beckn.Select.handler confirms availability
-- May provisionally assign driver or lock resources
```

#### 2.4 On_Select Confirmation
**Backend (dynamic-offer-driver-app)**
```haskell
-- Sends on_select callback confirming selection
-- Beckn.ACL.OnSelect.mkOnSelectMessageV2
-- Provides finalized offer details
```

**Backend (rider-app)**
```haskell
-- API.Beckn.OnSelect processes confirmation
-- Domain.Action.Beckn.OnSelect.onSelect
-- Updates quote status to GOT_DRIVER_QUOTE
-- Creates driver offer data for user selection or auto-assignment
```

### Phase 3: Booking Initiation

#### 3.1 Rider Confirms Booking
**Frontend (Rider App)**
```purescript
-- User confirms they want to book the ride
-- May include payment method selection
```

**Backend (rider-app)**
```haskell
-- API.UI.Confirm (BAP confirm, not Beckn confirm)
POST /v2/rideSearch/quotes/{quoteId}/confirm
-- Domain.Action.UI.Confirm.confirm
-- Creates Booking entity on BAP side
-- Sends Beckn /init to BPP
```

#### 3.2 BPP Processes Init
**Backend (dynamic-offer-driver-app)**
```haskell
-- API.Beckn.Init receives /init request
POST /v2/beckn/{merchantId}/init
-- Uses Redis locks for idempotency (initLockKey, initProcessingLockKey)
-- Domain.Action.Beckn.Init.validateRequest validates against merchant
-- Domain.Action.Beckn.Init.handler creates preliminary Booking record
-- Finalizes fare and payment terms using fare policies
```

#### 3.3 On_Init Response
**Backend (dynamic-offer-driver-app)**
```haskell
-- Sends on_init callback with booking details
-- Beckn.ACL.OnInit.mkOnInitMessageV2
-- Includes finalized payment terms and fulfillment info
-- Callback.withCallback sends to BAP
```

**Backend (rider-app)**
```haskell
-- API.Beckn.OnInit processes response
-- Domain.Action.Beckn.OnInit.onInit
-- Updates Booking with BPP booking ID
-- Automatically sends Beckn /confirm (auto-confirm flow)
-- SharedLogic.CallBPP.confirmV2
```

### Phase 4: Booking Confirmation

#### 4.1 Automatic Confirm
**Backend (rider-app)**
```haskell
-- Automatically sends Beckn /confirm after on_init
-- SharedLogic.CallBPP.confirmV2
-- Final confirmation to complete booking
```

#### 4.2 BPP Finalizes Booking
**Backend (dynamic-offer-driver-app)**
```haskell
-- API.Beckn.Confirm receives /confirm request
POST /v2/beckn/{merchantId}/confirm
-- Uses Redis locks (confirmLockKey, confirmProcessingLockKey)
-- Domain.Action.Beckn.Confirm.validateRequest final validation
-- Domain.Action.Beckn.Confirm.handler finalizes Booking
-- Creates Ride entity and assigns driver
-- Updates booking status to TRIP_ASSIGNED
```

#### 4.3 On_Confirm Response
**Backend (dynamic-offer-driver-app)**
```haskell
-- Sends on_confirm with final booking details
-- Beckn.ACL.OnConfirm.mkOnConfirmMessageV2
-- Includes driver info, vehicle details, OTP
-- Confirms ride is ready to proceed
```

**Backend (rider-app)**
```haskell
-- API.Beckn.OnConfirm processes confirmation
-- Domain.Action.Beckn.OnConfirm.onConfirm
-- Updates booking status to CONFIRMED
-- Creates Ride record with driver details
-- Stores OTP and vehicle information
```

### Phase 5: Pre-Ride (Driver Assignment)

#### 5.1 Driver Assignment Complete
**Frontend (Rider App)**
```purescript
-- Shows driver details, vehicle info, OTP
-- Displays driver location on map
-- Enables contact driver functionality
```

**Status Updates:**
- Booking Status: `CONFIRMED` → `TRIP_ASSIGNED`
- Person Flow Status: `WAITING_FOR_DRIVER_ASSIGNMENT` → `RIDE_PICKUP`

#### 5.2 Driver Navigation to Pickup
**Backend (dynamic-offer-driver-app)**
```haskell
-- Driver app shows pickup location and rider details
-- Driver navigates to pickup point
-- Location updates sent via location-updates library
-- SharedLogic.External.LocationTrackingService
```

**Frontend (Driver App)**
```purescript
-- Driver sees pickup location and rider details
-- Can contact rider if needed via API.UI.Call
-- Updates status as approaching pickup
```

#### 5.3 Driver Arrival Notification
**Backend (dynamic-offer-driver-app)**
```haskell
-- Driver marks as arrived at pickup
-- API.UI.Driver (driver arrival actions)
-- Sends on_status update to BAP with arrival confirmation
```

**Backend (rider-app)**
```haskell
-- API.Beckn.OnStatus receives arrival update
-- Domain.Action.Beckn.OnStatus.onStatus
-- Updates person flow status to DRIVER_ARRIVED
-- Triggers rider notification
```

### Phase 6: Active Ride

#### 6.1 Ride Start
**Backend (dynamic-offer-driver-app)**
```haskell
-- Driver enters OTP to start ride
-- API.UI.Driver.rideStart
-- Validates OTP against stored value
-- Updates ride status to INPROGRESS
-- Sends on_status with RIDE_STARTED
```

**Status Updates:**
- Ride Status: `NEW` → `INPROGRESS`
- Person Flow Status: `RIDE_PICKUP` → `RIDE_STARTED`

#### 6.2 Real-time Tracking
**Backend (dynamic-offer-driver-app)**
```haskell
-- Continuous location updates via location-updates library
-- SharedLogic.External.LocationTrackingService
-- Distance and route tracking
-- Real-time ETA calculations
```

**Frontend (Rider App)**
```purescript
-- Live driver tracking on map
-- ETA updates based on real-time location
-- Route progress visualization
-- Contact driver option available
```

#### 6.3 Mid-Ride Updates
**Backend (dynamic-offer-driver-app)**
```haskell
-- Can send various on_update messages:
-- Route changes, ETA updates, fare adjustments
-- API.Beckn.Update for BPP-initiated changes
-- Destination edits, waiting time charges
```

**Backend (rider-app)**
```haskell
-- API.Beckn.OnUpdate handles mid-ride updates
-- Domain.Action.Beckn.OnUpdate.onUpdate
-- Updates booking/ride details as needed
-- Processes fare recalculations
```

### Phase 7: Ride Completion

#### 7.1 Destination Reached
**Backend (dynamic-offer-driver-app)**
```haskell
-- Driver marks ride as complete
-- API.UI.Driver.rideEnd
-- May require OTP verification or odometer reading
-- Captures final trip parameters
```

#### 7.2 Fare Calculation
**Backend (dynamic-offer-driver-app)**
```haskell
-- Calculate final fare based on:
-- Distance traveled, time taken, surge pricing
-- Tolls, waiting charges, other fees
-- SharedLogic.FareCalculator applies rate cards
-- yudhishthira rule engine for complex pricing rules
```

#### 7.3 Ride End Notification
**Backend (dynamic-offer-driver-app)**
```haskell
-- Send on_status with ride completion
-- Includes final fare breakdown
-- Updates ride status to COMPLETED
-- Beckn.ACL.OnStatus.mkOnStatusMessageV2
```

**Backend (rider-app)**
```haskell
-- API.Beckn.OnStatus processes completion
-- Domain.Action.Beckn.OnStatus.onStatus
-- Updates booking and ride status to COMPLETED
-- Triggers post-ride processing
```

### Phase 8: Post-Ride

#### 8.1 Payment Processing
**Backend (rider-app)**
```haskell
-- API.UI.Payment handles payment execution
-- Lib.Payment.API for payment gateway integration
-- Domain.Action.UI.Payment.createOrder
-- Processes final charges including any adjustments
```

#### 8.2 Rating and Feedback
**Frontend (Rider App)**
```purescript
-- Shows ride completion screen
-- Prompts for driver rating
-- Optional detailed feedback form
```

**Backend (rider-app)**
```haskell
-- API.UI.Rating for ride ratings
-- API.UI.FeedbackForm for detailed feedback
-- Domain.Action.UI.Feedback.submitFeedback
-- May trigger Beckn rating message to BPP
```

#### 8.3 Invoice Generation
**Backend (rider-app)**
```haskell
-- API.Action.UI.Invoice for ride receipts
-- API.Action.UI.PriceBreakup for fare details
-- Domain.Action.UI.Invoice.getInvoices
-- Generates detailed fare breakdown
```

## Error Handling and Edge Cases

### Cancellation Flow
**Rider-Initiated Cancellation:**
```haskell
-- API.UI.Cancel
POST /v2/rideBooking/{bookingId}/cancel
-- Domain.Action.UI.Cancel.cancel
-- Supports soft cancel (query fees) and hard cancel
-- Sends Beckn /cancel to BPP with reason and type
-- Calculates cancellation charges using yudhishthira
```

**Driver-Initiated Cancellation:**
```haskell
-- BPP sends on_cancel to BAP
-- API.Beckn.OnCancel processes cancellation
-- Domain.Action.Beckn.OnCancel.onCancel
-- Updates booking status to CANCELLED
-- Handles driver reallocation if needed
```

### Driver Reassignment
```haskell
-- If driver cancels after assignment:
-- Booking status: TRIP_ASSIGNED → AWAITING_REASSIGNMENT
-- System searches for new driver using allocation algorithms
-- May send REALLOCATED status when new driver found
-- SharedLogic.DriverPool.allocateDriver
```

### Payment Failures
```haskell
-- Payment retry mechanisms via Lib.Payment
-- Alternative payment methods support
-- Ride completion even with payment pending
-- Settlement processes for failed payments
```

### Emergency Handling
```haskell
-- API.Action.UI.Sos for emergency situations
-- API.UI.Sos for SOS functionality
-- Integration with emergency services
-- Real-time location sharing with emergency contacts
-- IVR integration for emergency calls
```

### Idempotency and Reliability
```haskell
-- Redis locks prevent duplicate processing:
-- searchLockKey, initLockKey, confirmLockKey
-- Kafka events ensure eventual consistency
-- Database transactions maintain data integrity
-- Retry mechanisms for external service calls
```

## Key Components and Data Flow

### Frontend State Management
```purescript
-- Flow.purs manages overall application flow
-- currentFlowStatus tracks ride progression
-- State transitions based on backend status
-- Polling mechanisms for real-time updates
```

### Backend Service Integration
- **Beckn Protocol**: Standardized mobility API with digital signatures
- **Payment Gateway**: Juspay integration with tokenization
- **Maps Service**: OSRM, Google Maps for routing and geocoding
- **SMS Service**: OTP delivery and notifications
- **Location Tracking**: Real-time driver location with geospatial queries
- **Push Notifications**: FCM for real-time alerts

### Database Entities
- **SearchRequest**: Initial ride search with preferences
- **Estimate**: Fare estimates from providers with dynamic pricing
- **Quote**: Driver-specific quotes with availability
- **Booking**: Confirmed ride booking with payment details
- **Ride**: Active ride with driver assignment and tracking
- **Person**: Rider profile, preferences, and history

### Caching and Performance
- **Redis**: Session management, distributed locks, geospatial caching
- **Kafka**: Event streaming for async processing and service communication
- **Database Replicas**: Read operations optimization
- **Connection Pooling**: Efficient database connection management

## Configuration and Customization

### Dynamic Pricing (yudhishthira)
```haskell
-- Rule-based pricing engine
-- Real-time demand-supply analysis
-- Time-based pricing variations
-- Special zone pricing rules
-- Surge pricing algorithms
```

### Fare Policies
```haskell
-- Configurable fare calculation rules
-- Base fare, distance rates, time charges
-- Surge pricing multipliers, tolls, waiting charges
-- Cancellation fee structures
-- Rate cards per vehicle type and city
```

### Service Areas
```haskell
-- Geofenced service areas with polygon definitions
-- City-specific configurations
-- Special zones (airports, stations, malls)
-- Pickup/drop restrictions
```

### Driver Pool Management
```haskell
-- Driver availability algorithms
-- Proximity-based assignment with radius limits
-- Load balancing across drivers
-- Driver scoring and preference systems
```

### Business Rules (yudhishthira Integration)
```haskell
-- Cancellation policies with time-based rules
-- Surge pricing trigger conditions
-- Driver incentive program rules
-- Rider loyalty program logic
-- Dynamic offer generation rules
```

## Monitoring and Analytics

### Metrics Collection
- Ride completion rates and success metrics
- Driver response times and acceptance rates
- Customer satisfaction scores and ratings
- Revenue tracking and fare analysis
- Search-to-booking conversion rates

### Error Tracking
- Failed API calls with detailed error codes
- Payment failures and retry attempts
- Driver no-shows and cancellation patterns
- Customer complaints and issue resolution
- Beckn protocol message failures

### Performance Monitoring
- API response times and latency percentiles
- Database query performance and optimization
- External API latency (maps, payments, SMS)
- Mobile app crashes and error rates
- Real-time location tracking accuracy

### Business Intelligence
- Demand pattern analysis
- Driver utilization and earnings
- Route optimization insights
- Pricing effectiveness analysis
- User behavior and retention metrics

## Technical Implementation Details

### Beckn Protocol Flow
1. **Search Request**: Rider → BAP → Gateway → BPP → Driver Pool
2. **Quote Response**: Drivers → BPP → Gateway → BAP → Rider
3. **Selection**: Rider → BAP → Gateway → BPP (with resource locking)
4. **Booking Flow**: BAP ↔ BPP (init/confirm handshake with validation)
5. **Assignment**: BPP assigns driver → notifies BAP via on_confirm
6. **Active Ride**: Continuous status sync between BPP and BAP
7. **Completion**: BPP completes ride → notifies BAP → post-ride processing

### Auto-Assignment vs Manual Selection
- **Auto-Assignment**: First available quote automatically selected, immediate init/confirm
- **Manual Selection**: Multiple quotes presented, user chooses, then init/confirm
- **Configuration**: Merchant-level settings control assignment behavior

### Real-time Communication
- **Push Notifications**: FCM for immediate alerts
- **WebSocket**: Live tracking and status updates
- **Polling**: Fallback mechanism for status synchronization
- **Beckn Callbacks**: Asynchronous status updates between BAP/BPP

This comprehensive architecture ensures a reliable, efficient, and user-friendly ride booking experience while maintaining compliance with open mobility standards through the Beckn protocol and supporting dynamic pricing through the yudhishthira rule engine. 