# Reach on Time - Detailed Implementation Plan

**Version**: 2.0 (Post-Review Revision)
**Date**: 2026-03-13
**Scope**: All Nammayatri Platform Repositories

---

## 1. Implementation Order (Critical Path)

```
Phase 0: Data Foundation (Starts Day 1, parallel)
  └─ 0.1 GTFS Data Quality Monitoring Pipeline (gtfs-inmemory-server-rust)
  └─ 0.2 Schedule Adherence Logging (nammayatri/rider-app)

Phase 1: Backend Foundation (Weeks 1-2)
  ├─ 1.1 GTFS Time-based Query APIs (gtfs-inmemory-server-rust)
  ├─ 1.2 Journey Time Constraint Types (shared-kernel)
  └─ 1.3 Time-Aware Search API (nammayatri/rider-app)

Phase 2: Computation Engine (Weeks 3-4)
  ├─ 2.1 OpenTripPlanner Integration for Arrive By (NEW - per review)
  ├─ 2.2 Schedule-Aware Route Planner (nammayatri/rider-app)
  └─ 2.3 Risk/Buffer/Crowding Calculation Engine (nammayatri/rider-app)

Phase 3: Frontend - Core UI (Weeks 5-6)
  ├─ 3.1 Time Mode Selector Component (ny-react-native)
  ├─ 3.2 Time Picker Modal (ny-react-native)
  └─ 3.3 Enhanced Route Cards (Frontend/ui-customer PureScript)

Phase 4: Frontend - Advanced UI (Weeks 7-8)
  ├─ 4.1 Departure Advisor View (Frontend/ui-customer)
  ├─ 4.2 Journey Timeline Enhancements (Frontend/ui-customer)
  └─ 4.3 Saved Trips UI (Frontend/ui-customer + ny-react-native)

Phase 5: Notifications & Intelligence (Weeks 9-10)
  ├─ 5.1 Departure Reminder Service (nammayatri/rider-app)
  ├─ 5.2 Push Notification Integration (nammayatri/rider-app)
  └─ 5.3 Saved/Recurring Trip Backend (nammayatri/rider-app)

Phase 6: Real-time & Polish (Weeks 11-12)
  ├─ 6.1 Live Data Overlay (location-tracking-service + gps-server)
  ├─ 6.2 ETA Refinement with Live Data (eta-compute)
  └─ 6.3 End-to-End Testing & Launch (all repos)
```

---

## 2. Repository-by-Repository Implementation

---

### 2.1 `gtfs-inmemory-server-rust` — GTFS Schedule Server

**Purpose**: Add time-based schedule querying capability.

#### 2.1.1 New API Endpoints

**File**: `src/handlers/routes.rs`
**Changes**: Add new route registrations

```rust
// Add to create_routes function:
.route(
    "/schedule/departures/{gtfs_id}/{stop_code}",
    web::get().to(get_departures_at_stop),
)
.route(
    "/schedule/arrivals/{gtfs_id}/{stop_code}",
    web::get().to(get_arrivals_at_stop),
)
.route(
    "/schedule/trips-between/{gtfs_id}",
    web::post().to(get_trips_between_stops),
)
.route(
    "/schedule/next-services/{gtfs_id}",
    web::post().to(get_next_services),
)
```

**New Handler File**: `src/handlers/schedule.rs`

```rust
use serde::{Deserialize, Serialize};
use chrono::{NaiveTime, NaiveDate};

#[derive(Debug, Deserialize)]
pub struct DepartureQuery {
    pub time: NaiveTime,           // "08:30:00"
    pub date: NaiveDate,           // "2026-03-13"
    pub time_window_minutes: i32,  // 60 = show departures within next hour
    pub limit: Option<i32>,        // max results
}

#[derive(Debug, Deserialize)]
pub struct TripsBetweenRequest {
    pub origin_stop_code: String,
    pub destination_stop_code: String,
    pub date: NaiveDate,
    pub depart_after: Option<NaiveTime>,   // For "Depart At" / "Leave Now"
    pub arrive_before: Option<NaiveTime>,  // For "Arrive By"
    pub time_window_minutes: i32,
    pub include_transfers: bool,
}

#[derive(Debug, Serialize)]
pub struct ScheduledDeparture {
    pub trip_id: String,
    pub route_id: String,
    pub route_short_name: String,
    pub departure_time: NaiveTime,
    pub arrival_time: NaiveTime,    // at destination (if applicable)
    pub service_type: String,       // "ORDINARY", "EXPRESS", etc.
    pub headsign: String,
    pub is_realtime_available: bool,
    pub realtime_delay_seconds: Option<i32>,
}

#[derive(Debug, Serialize)]
pub struct ScheduledTrip {
    pub trip_id: String,
    pub route_id: String,
    pub route_short_name: String,
    pub origin_departure: NaiveTime,
    pub destination_arrival: NaiveTime,
    pub duration_minutes: i32,
    pub stops_count: i32,
    pub service_type: String,
    pub transfer_points: Vec<TransferPoint>,  // if includes_transfers
}
```

**New Service Layer**: `src/services/schedule.rs`

Core schedule query logic using in-memory GTFS data:

```rust
pub struct ScheduleService {
    gtfs_data: Arc<GtfsData>,
}

impl ScheduleService {
    /// Find all departures from a stop within a time window
    pub fn departures_at_stop(
        &self,
        stop_code: &str,
        date: NaiveDate,
        time: NaiveTime,
        window_minutes: i32,
    ) -> Vec<ScheduledDeparture> {
        // 1. Get active service_ids for the given date (calendar + calendar_dates)
        // 2. Filter stop_times where stop_id matches and departure_time in window
        // 3. Join with trips and routes for display info
        // 4. Sort by departure_time
        // Key: Use pre-built index stop_code -> sorted stop_times for O(log n) lookup
    }

    /// Find trips connecting two stops (direct or with transfers)
    pub fn trips_between_stops(
        &self,
        req: &TripsBetweenRequest,
    ) -> Vec<ScheduledTrip> {
        // For "Arrive By": work backward from arrive_before
        // 1. Find services arriving at destination before target time
        // 2. For each, check if there's a departure from origin that connects
        // 3. Include transfer options if requested
        //
        // For "Depart At": work forward from depart_after
        // 1. Find services departing from origin after target time
        // 2. Compute arrival at destination
        // 3. Sort by arrival time (earliest first)
    }
}
```

**Index Optimization**: `src/services/schedule_index.rs`

```rust
/// Pre-built index for fast time-based schedule queries
pub struct ScheduleIndex {
    /// stop_code -> BTreeMap<departure_time, Vec<StopTimeEntry>>
    /// BTreeMap enables efficient range queries for time windows
    departures_by_stop: HashMap<String, BTreeMap<u32, Vec<StopTimeEntry>>>,

    /// (origin_stop, destination_stop) -> Vec<DirectConnection>
    /// Pre-computed direct connections for common stop pairs
    direct_connections: HashMap<(String, String), Vec<DirectConnection>>,

    /// service_id -> active dates
    service_calendar: HashMap<String, HashSet<NaiveDate>>,
}
```

**Data Quality Monitoring** *(Added per review)*:

```rust
/// Phase 0: Track GTFS data completeness
pub struct GtfsDataQualityReport {
    pub total_routes: usize,
    pub routes_with_stop_times: usize,
    pub total_trips: usize,
    pub trips_with_complete_schedules: usize,
    pub coverage_percentage: f64,
    pub stale_data_days: i32,  // days since last GTFS update
}

// Expose via API:
// GET /internal/gtfs/{gtfs_id}/quality-report
```

**Estimated effort**: 6 days (5 + 1 for quality monitoring)
**Dependencies**: None (foundational)
**Tests**: Unit tests for schedule queries with sample GTFS data

---

### 2.1.5 OpenTripPlanner Integration *(Added per review)*

**Rationale**: "Arrive By" reverse time-dependent routing (backward RAPTOR algorithm) is a solved problem in OpenTripPlanner. Building this from scratch in Haskell would take months. Instead, deploy OTP as a sidecar service and call it from rider-app.

**Deployment**: Docker container running OTP 2.x with Chennai GTFS feed

```
# docker-compose addition:
otp:
  image: opentripplanner/opentripplanner:2.5
  volumes:
    - ./gtfs-data:/var/opentripplanner
  ports:
    - "8080:8080"
  command: --load --serve
```

**Integration**: Call OTP's `/plan` API from rider-app:

```
GET /otp/routers/default/plan
  ?fromPlace=12.9716,77.5946
  &toPlace=13.0827,80.2707
  &date=2026-03-13
  &time=09:30
  &arriveBy=true          # Key parameter for "Arrive By"
  &mode=TRANSIT,WALK
  &numItineraries=5
```

**Haskell Client**: `Backend/app/rider-platform/rider-app/Main/src/ExternalBPP/OTP/Client.hs`

```haskell
module ExternalBPP.OTP.Client where

data OTPPlanRequest = OTPPlanRequest
  { fromPlace :: LatLng
  , toPlace :: LatLng
  , date :: Day
  , time :: TimeOfDay
  , arriveBy :: Bool
  , mode :: Text  -- "TRANSIT,WALK"
  , numItineraries :: Int
  }

callOTPPlanner :: OTPPlanRequest -> Flow [OTPItinerary]
callOTPPlanner req = do
  otpConfig <- getOTPConfig
  callAPI otpConfig.baseUrl req
```

**Estimated effort**: 4 days (deployment + Haskell client + integration)
**Dependencies**: Chennai GTFS data available
**Tests**: Integration tests with OTP container

---

### 2.2 `shared-kernel` — Shared Types

**Purpose**: Define shared types used across rider-app and other services.

#### New Types

**File**: `lib/kernel/src/Kernel/External/MultiModal/Interface/Types.hs`
(extend existing types)

```haskell
-- Add to existing MultiModal types:

data JourneyTimeConstraint
  = LeaveNow
  | DepartAt UTCTime
  | ArriveBy UTCTime
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data DepartureAdvisory = DepartureAdvisory
  { latestDeparture :: UTCTime
  , recommendedDeparture :: UTCTime
  , comfortableDeparture :: UTCTime
  , riskLevel :: RiskLevel
  , bufferMinutes :: Int
  , advisoryMessage :: Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data RiskLevel
  = Comfortable  -- >= 12 min buffer
  | Good         -- 5-12 min buffer
  | Tight        -- < 5 min buffer
  | TooLate      -- cannot make it
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data SavedTripConfig = SavedTripConfig
  { name :: Text
  , origin :: LatLng
  , destination :: LatLng
  , timeConstraint :: JourneyTimeConstraint
  , recurrence :: TripRecurrence
  , notifyBeforeMinutes :: Int
  , isActive :: Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data TripRecurrence
  = NoRecurrence
  | Daily
  | Weekdays
  | Weekends
  | CustomDays [DayOfWeek]
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
```

**Estimated effort**: 2 days
**Dependencies**: None
**Tests**: Serialization round-trip tests

---

### 2.3 `nammayatri/rider-app` — Main Backend

This is the largest piece of work, organized into sub-tasks.

#### 2.3.1 Database Schema Changes

**New YAML Spec**: `Backend/app/rider-platform/rider-app/Main/spec/Storage/SavedTrip.yaml`

```yaml
imports:
  Domain.Types.Person: Person
  Domain.Types.Merchant: Merchant
  Domain.Types.MerchantOperatingCity: MerchantOperatingCity

SavedTrip:
  tableName: saved_trip
  fields:
    id: Id SavedTrip
    riderId: Id Person
    name: Text
    originLat: Double
    originLon: Double
    originAddress: Maybe Text
    destinationLat: Double
    destinationLon: Double
    destinationAddress: Maybe Text
    timeMode: TimeMode
    targetTime: Maybe UTCTime          # For ArriveBy/DepartAt, time-of-day
    targetTimeOfDay: Maybe TimeOfDay   # Store as HH:MM for recurring
    bufferMinutes: Int
    recurrence: TripRecurrence
    notifyBeforeMinutes: Int
    isActive: Bool
    lastComputedDeparture: Maybe UTCTime
    lastNotifiedAt: Maybe UTCTime
    merchantId: Id Merchant
    merchantOperatingCityId: Id MerchantOperatingCity
    createdAt: UTCTime
    updatedAt: UTCTime

  # NOTE (per review): CustomDays needs a separate column for day list
  # TimeOfDay needs fromTType/toTType transformers
  extraFields:
    customDays: Maybe Text  # JSON-encoded [DayOfWeek] for Custom recurrence

  types:
    TimeMode:
      enum: "LeaveNow, ArriveBy, DepartAt"
    TripRecurrence:
      enum: "NoRecurrence, Daily, Weekdays, Weekends, Custom"

  beamFields:
    targetTimeOfDay:
      toTType: Kernel.Prelude.fmap Data.Time.timeToTimeOfDay
      fromTType: Kernel.Prelude.fmap Data.Time.timeOfDayToTime

  extraOperations:
    - EXTRA_QUERY_FILE  # Needed for complex findAllActiveRecurring query

  queries:
    findAllByRiderId:
      kvFunction: findAllWithKV
      where:
        riderId: Id Person
    # findAllActiveRecurring moved to Extra query file
    # because NammaDSL may not support != operator
```

**Migration**: `Backend/dev/migrations/rider-app/XXXX-add-saved-trip.sql`

```sql
CREATE TABLE atlas_app.saved_trip (
    id VARCHAR(36) PRIMARY KEY,
    rider_id VARCHAR(36) NOT NULL REFERENCES atlas_app.person(id),
    name TEXT NOT NULL,
    origin_lat DOUBLE PRECISION NOT NULL,
    origin_lon DOUBLE PRECISION NOT NULL,
    origin_address TEXT,
    destination_lat DOUBLE PRECISION NOT NULL,
    destination_lon DOUBLE PRECISION NOT NULL,
    destination_address TEXT,
    time_mode VARCHAR(20) NOT NULL DEFAULT 'LeaveNow',
    target_time TIMESTAMP WITH TIME ZONE,
    target_time_of_day TIME,
    buffer_minutes INT NOT NULL DEFAULT 10,
    recurrence VARCHAR(20) NOT NULL DEFAULT 'NoRecurrence',
    notify_before_minutes INT NOT NULL DEFAULT 5,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    last_computed_departure TIMESTAMP WITH TIME ZONE,
    last_notified_at TIMESTAMP WITH TIME ZONE,
    merchant_id VARCHAR(36) NOT NULL,
    merchant_operating_city_id VARCHAR(36) NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_saved_trip_rider ON atlas_app.saved_trip(rider_id);
CREATE INDEX idx_saved_trip_active_recurring ON atlas_app.saved_trip(is_active, recurrence)
    WHERE is_active = TRUE AND recurrence != 'NoRecurrence';
```

**Journey Table Enhancement**: Add time constraint fields

```sql
ALTER TABLE atlas_app.journey ADD COLUMN time_mode VARCHAR(20) DEFAULT 'LeaveNow';
ALTER TABLE atlas_app.journey ADD COLUMN target_arrival_time TIMESTAMP WITH TIME ZONE;
ALTER TABLE atlas_app.journey ADD COLUMN target_departure_time TIMESTAMP WITH TIME ZONE;
ALTER TABLE atlas_app.journey ADD COLUMN buffer_minutes INT DEFAULT 0;
ALTER TABLE atlas_app.journey ADD COLUMN risk_level VARCHAR(20);
ALTER TABLE atlas_app.journey ADD COLUMN recommended_departure TIMESTAMP WITH TIME ZONE;
```

#### 2.3.2 API Design *(Revised per architecture review)*

> **CRITICAL REVIEW FEEDBACK (Rated 2/5)**: Do NOT create a separate `/reachOnTime/*` API namespace.
> Instead, extend the existing multimodal search and journey APIs. The existing `SearchRequest` already
> has `startTime`/`returnTime` fields. Add `timeMode`, `targetTime`, `bufferMinutes` to the existing
> search flow and `MultiModal.yaml`.

**Extend existing YAML**: `Backend/app/rider-platform/rider-app/Main/spec/API/MultiModal.yaml`

```yaml
# ADD to existing multimodal APIs:

  # Departure advisory (extend existing journey info)
  - GET /journey/{journeyId}/advisory:
      auth: TokenAuth
      response:
        advisory: DepartureAdvisory
        alternativeAdvisories: "[DepartureAdvisory]"

  # Saved trips CRUD (new section in MultiModal.yaml)
  - POST /journey/savedTrip:
      auth: TokenAuth
      request: SavedTripCreateReq
      response:
        savedTripId: Id SavedTrip

  - GET /journey/savedTrips:
      auth: TokenAuth
      query:
        limit: Maybe Int
        offset: Maybe Int
      response:
        trips: "[SavedTripResp]"
        total: Int

  - PUT /journey/savedTrip/{savedTripId}:
      auth: TokenAuth
      request: SavedTripUpdateReq

  - DELETE /journey/savedTrip/{savedTripId}:
      auth: TokenAuth

  - POST /journey/savedTrip/{savedTripId}/compute:
      auth: TokenAuth
      response:
        advisory: DepartureAdvisory
        journeyOptions: "[JourneyInfoResp]"  # Reuse existing type
```

**Extend existing YAML**: `Backend/app/rider-platform/rider-app/Main/spec/Storage/SearchRequest.yaml`

```yaml
# ADD fields to existing SearchRequest:
  timeMode: Maybe TimeMode           # LeaveNow | ArriveBy | DepartAt
  targetArrivalTime: Maybe UTCTime   # For ArriveBy mode
  targetDepartureTime: Maybe UTCTime # For DepartAt mode
  bufferMinutes: Maybe Int           # Comfort buffer (default 10)
```

**Extend existing YAML**: `Backend/app/rider-platform/rider-app/Main/spec/Storage/MultiModal.yaml`

```yaml
# ADD fields to existing Journey section:
  timeMode: Maybe TimeMode
  targetArrivalTime: Maybe UTCTime
  targetDepartureTime: Maybe UTCTime
  bufferMinutes: Maybe Int
  riskLevel: Maybe RiskLevel
  recommendedDeparture: Maybe UTCTime
```

> **Security note (per review)**: All saved trip endpoints MUST verify `savedTrip.riderId == authenticatedPersonId`
> to prevent IDOR vulnerabilities. Add authorization check in each handler.

#### 2.3.3 Core Business Logic

**Extend existing file**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/MultimodalConfirm.hs`
*(Per review: do NOT create a separate ReachOnTime.hs — integrate into existing multimodal flow)*

```haskell
-- ADD to existing MultimodalConfirm.hs:

-- | Get departure advisory for a time-constrained journey
getJourneyAdvisory
  :: (Maybe (Id Person), Id Merchant)
  -> Id Journey
  -> Flow DepartureAdvisoryResp
getJourneyAdvisory (mbPersonId, merchantId) journeyId = do
  journey <- QJourney.findById journeyId >>= fromMaybeM (JourneyNotFound journeyId.getId)
  legs <- QJourneyLeg.findAllByJourneyId journeyId
  advisory <- computeDepartureAdvisory journey.timeMode journey.targetArrivalTime legs journey.bufferMinutes
  pure $ DepartureAdvisoryResp advisory
```

**Extend existing search flow**: `Backend/app/rider-platform/rider-app/Main/src/Domain/Action/UI/Search.hs`

```haskell
-- In the existing search handler, add time constraint propagation:
-- When searchRequest has timeMode == ArriveBy or DepartAt,
-- pass the constraint to the multimodal route planner.
-- This integrates naturally with the existing Beckn search flow
-- for taxi legs (time constraint is passed via Beckn search params).
```

**New File**: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/ReachOnTime/DepartureAdvisor.hs`

```haskell
module SharedLogic.ReachOnTime.DepartureAdvisor where

-- | Core departure advisory computation
computeDepartureAdvisory
  :: TimeMode
  -> Maybe UTCTime        -- target time
  -> MultiModalRoute      -- route option
  -> Maybe Int            -- buffer minutes
  -> Flow DepartureAdvisory
computeDepartureAdvisory timeMode targetTime route bufferMins = do
  let buffer = fromMaybe 10 bufferMins
  case timeMode of
    LeaveNow -> computeLeaveNowAdvisory route
    DepartAt departTime -> computeDepartAtAdvisory departTime route
    ArriveBy arriveTime -> computeArriveByAdvisory arriveTime route buffer

-- | For "Arrive By": work backward from target arrival
computeArriveByAdvisory
  :: UTCTime -> MultiModalRoute -> Int -> Flow DepartureAdvisory
computeArriveByAdvisory targetArrival route buffer = do
  -- 1. Get total journey duration from route
  let totalDuration = route.estimatedDuration
  -- 2. Compute latest departure = target - duration
  let latestDeparture = addUTCTime (negate $ fromIntegral totalDuration) targetArrival
  -- 3. Add buffer for recommended departure
  let recommendedDeparture = addUTCTime (negate $ fromIntegral (buffer * 60)) latestDeparture
  -- 4. Add generous buffer for comfortable departure
  let comfortableDeparture = addUTCTime (negate $ fromIntegral ((buffer + 10) * 60)) latestDeparture
  -- 5. Compute risk level based on current time
  now <- getCurrentTime
  let availableBuffer = diffUTCTime latestDeparture now
  let riskLevel = computeRisk availableBuffer buffer
  pure DepartureAdvisory
    { latestDeparture
    , recommendedDeparture
    , comfortableDeparture
    , riskLevel
    , bufferMinutes = buffer
    , advisoryMessage = generateAdvisoryMessage riskLevel availableBuffer
    }

-- | Risk level computation
computeRisk :: NominalDiffTime -> Int -> RiskLevel
computeRisk availableTime bufferMins
  | availableTime <= 0 = TooLate
  | availableTime < fromIntegral (bufferMins * 60) = Tight
  | availableTime < fromIntegral ((bufferMins + 7) * 60) = Good
  | otherwise = Comfortable

-- | Peak-hour crowding buffer (Added per review)
-- During peak hours, buses may pass full. Add extra wait time.
computeCrowdingBuffer :: UTCTime -> Text -> Flow Seconds
computeCrowdingBuffer departureTime routeId = do
  let localTime = utcToLocalTime ist departureTime
      isPeakMorning = localTime >= TimeOfDay 7 30 0 && localTime <= TimeOfDay 10 0 0
      isPeakEvening = localTime >= TimeOfDay 17 0 0 && localTime <= TimeOfDay 20 0 0
  if isPeakMorning || isPeakEvening
    then do
      -- Check historical crowding data for this route
      mbCrowdingData <- getCrowdingData routeId localTime
      case mbCrowdingData of
        Just crowding -> pure $ Seconds (crowding.averageExtraBoardingWait)
        Nothing -> pure $ Seconds 600  -- Default 10 min peak-hour buffer
    else pure $ Seconds 0

-- | Late-night safety check (Added per review)
flagLateNightWalkingLegs :: [JourneyLeg] -> UTCTime -> [SafetyWarning]
flagLateNightWalkingLegs legs time =
  let localTime = utcToLocalTime ist time
      isLateNight = localTime >= TimeOfDay 21 0 0 || localTime <= TimeOfDay 5 0 0
  in if isLateNight
     then mapMaybe checkWalkLeg legs
     else []
  where
    checkWalkLeg leg
      | leg.mode == Walk && fromMaybe 0 (distanceToMeters <$> leg.distance) > 300 =
          Just SafetyWarning
            { legId = leg.id
            , warning = "Walking " <> showDistance leg.distance <> " after dark. Consider auto instead."
            , severity = Medium
            }
      | otherwise = Nothing
```

**New File**: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/ReachOnTime/ScheduleQuery.hs`

```haskell
module SharedLogic.ReachOnTime.ScheduleQuery where

-- | Query GTFS schedule for time-constrained transit options
queryScheduledTransit
  :: TimeMode
  -> LatLng -> LatLng
  -> UTCTime
  -> MultimodalUserPreferences
  -> Flow [TransitOption]
queryScheduledTransit timeMode origin destination time prefs = do
  -- 1. Find nearby stops to origin and destination
  originStops <- findNearbyStops origin 500  -- 500m radius
  destStops <- findNearbyStops destination 500
  -- 2. Query GTFS server for trips between stop pairs
  let gtfsReq = case timeMode of
        ArriveBy arriveTime ->
          TripsBetweenRequest
            { depart_after = Nothing
            , arrive_before = Just (utcToLocalTime ist arriveTime)
            , ..
            }
        DepartAt departTime ->
          TripsBetweenRequest
            { depart_after = Just (utcToLocalTime ist departTime)
            , arrive_before = Nothing
            , ..
            }
        LeaveNow ->
          TripsBetweenRequest
            { depart_after = Just (utcToLocalTime ist time)
            , arrive_before = Nothing
            , ..
            }
  -- 3. Call GTFS in-memory server
  trips <- callGtfsServer gtfsReq
  -- 4. Filter by user preferences (allowed modes, service tiers)
  filterByPreferences prefs trips
```

**Extend Existing File**: `Backend/app/rider-platform/rider-app/Main/src/Lib/JourneyModule/Base.hs`

Add time constraint propagation to existing journey module:

```haskell
-- IMPORTANT (per review): Do NOT change the init function signature
-- (that would be a breaking change for all callers).
-- Instead, add time constraint fields to JourneyInitData:

-- In Lib/JourneyModule/Types.hs, extend JourneyInitData:
data JourneyInitData = JourneyInitData
  { -- ... existing fields ...
  , timeMode :: Maybe TimeMode                -- NEW
  , targetArrivalTime :: Maybe UTCTime        -- NEW
  , targetDepartureTime :: Maybe UTCTime      -- NEW
  , bufferMinutes :: Maybe Int                -- NEW
  }

-- Inside init:
-- When time constraint is provided, filter legs by schedule availability
-- Reuse existing filterTransitRoutes (Base.hs lines 104-138) with time param
-- Leverage existing fromDepartureTime/toArrivalTime on JourneyLeg
-- Compute departure advisory for the complete journey
-- Store time constraint and advisory in Journey record
```

> **Reuse note (per review)**: The existing `fromDepartureTime`, `toArrivalTime` fields
> on `JourneyLeg` and the `filterTransitRoutes` function in `Base.hs` already do
> primitive time-aware filtering. Extend these rather than building parallel logic.

#### 2.3.4 Notification Scheduler

**New File**: `Backend/app/rider-platform/rider-app/Main/src/SharedLogic/Scheduler/Jobs/DepartureReminder.hs`
*(Per review: scheduler jobs go in SharedLogic/Scheduler/Jobs/, not Scheduler/src/Schedulers/)*

```haskell
module Schedulers.DepartureReminder where

-- | Periodic job that checks saved trips and sends departure reminders
processDepartureReminders :: Flow ()
processDepartureReminders = do
  -- 1. Get all active recurring saved trips
  activeTrips <- QSavedTrip.findAllActiveRecurring
  now <- getCurrentTime
  forM_ activeTrips $ \trip -> do
    -- 2. Check if today matches recurrence pattern
    when (isScheduledForToday trip.recurrence now) $ do
      -- 3. Compute today's departure advisory
      advisory <- computeTodaysDeparture trip
      -- 4. Check if it's time to send notification
      let notifyTime = addUTCTime
            (negate $ fromIntegral (trip.notifyBeforeMinutes * 60))
            advisory.recommendedDeparture
      when (now >= notifyTime && not (alreadyNotifiedToday trip now)) $ do
        -- 5. Send push notification
        sendDepartureReminder trip.riderId trip advisory
        -- 6. Update last notified timestamp
        QSavedTrip.updateLastNotified trip.id now
```

**Estimated effort**: 12 days (across all rider-app changes)
**Dependencies**: shared-kernel types, gtfs-inmemory-server-rust schedule APIs
**Tests**: Unit tests for advisory computation, integration tests for search flow

---

### 2.4 `nammayatri/Frontend/ui-customer` — PureScript Frontend

#### 2.4.1 New Types

**File**: `Frontend/ui-customer/src/Screens/SearchLocationFlow/SearchLocationScreen/ScreenData.purs`
(extend existing)

```purescript
-- Add to existing SearchLocationScreenState:
type SearchLocationScreenState = {
  -- ... existing fields ...
  , timeMode :: TimeMode
  , targetTime :: Maybe String  -- ISO8601
  , bufferMinutes :: Int
  , showTimePicker :: Boolean
  }

data TimeMode = LeaveNow | ArriveBy | DepartAt

derive instance eqTimeMode :: Eq TimeMode
```

#### 2.4.2 New Components

**New File**: `Frontend/ui-customer/src/Components/TimeModeSelector/View.purs`

```purescript
module Components.TimeModeSelector.View where

-- Time mode selector with 3 toggle options
-- Collapsed: Shows current selection as pill
-- Expanded: Shows all 3 options side by side
timeModeSelector :: forall w. TimeModeConfig -> PrestoDOM (Effect Unit) w
timeModeSelector config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , cornerRadius 24.0
    , background Color.grey100
    , padding (PaddingVertical 4 4)
    ]
    [ modeButton LeaveNow "Leave Now" config
    , modeButton ArriveBy "Arrive By" config
    , modeButton DepartAt "Depart At" config
    ]

modeButton :: forall w. TimeMode -> String -> TimeModeConfig -> PrestoDOM (Effect Unit) w
modeButton mode label config =
  textView
    [ text label
    , height WRAP_CONTENT
    , width $ V 0
    , weight 1.0
    , gravity CENTER
    , padding (Padding 8 12 8 12)
    , cornerRadius 20.0
    , background if config.currentMode == mode then Color.primary else Color.transparent
    , color if config.currentMode == mode then Color.white else Color.grey700
    , onClick push (const $ config.onModeChange mode)
    ]
```

**New File**: `Frontend/ui-customer/src/Components/TimePicker/View.purs`

```purescript
module Components.TimePicker.View where

-- Bottom sheet modal with:
-- 1. Date selector (Today/Tomorrow/Calendar)
-- 2. Time scroll picker (Hour:Minute AM/PM)
-- 3. Quick options (In 30m, In 1h, In 2h)
-- 4. Buffer selector (0/5/10/15 min) - only for ArriveBy
-- 5. Set Time button

timePickerModal :: forall w. TimePickerConfig -> PrestoDOM (Effect Unit) w
timePickerModal config =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , background Color.white
    , cornerRadii (Corners 16.0 true true false false)
    ]
    [ dateSelector config
    , timeScrollPicker config
    , quickOptions config
    , if config.mode == ArriveBy then bufferSelector config else emptyView
    , setTimeButton config
    ]
```

**New File**: `Frontend/ui-customer/src/Components/RouteCard/RiskBadge.purs`

```purescript
module Components.RouteCard.RiskBadge where

-- Risk indicator badge (Comfortable/Good/Tight/TooLate)
riskBadge :: forall w. RiskLevel -> PrestoDOM (Effect Unit) w
riskBadge risk =
  let
    { bgColor, textColor, label } = case risk of
      Comfortable -> { bgColor: "#E8F5E9", textColor: "#2E7D32", label: "COMFORTABLE" }
      Good -> { bgColor: "#FFF8E1", textColor: "#F9A825", label: "GOOD" }
      Tight -> { bgColor: "#FFEBEE", textColor: "#C62828", label: "TIGHT" }
      TooLate -> { bgColor: "#F5F5F5", textColor: "#757575", label: "TOO LATE" }
  in
    textView
      [ text label
      , background bgColor
      , color textColor
      , padding (Padding 8 4 8 4)
      , cornerRadius 4.0
      , textSize FontSize.a_10
      , fontStyle FontStyle.semiBold
      ]
```

**New File**: `Frontend/ui-customer/src/Components/DepartureAdvisor/View.purs`

```purescript
module Components.DepartureAdvisor.View where

-- Departure advisor card showing:
-- 1. Recommended departure time (large, bold)
-- 2. Risk level badge
-- 3. Buffer info
-- 4. Set Reminder button
-- 5. Arrival time relative to target

departureAdvisorCard :: forall w. DepartureAdvisory -> PrestoDOM (Effect Unit) w
departureAdvisorCard advisory =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , background (riskTintColor advisory.riskLevel)
    , cornerRadius 12.0
    , padding (Padding 16 16 16 16)
    , margin (Margin 16 8 16 8)
    ]
    [ linearLayout [orientation VERTICAL, weight 1.0]
        [ riskBadge advisory.riskLevel
        , textView [text "Leave by", textSize FontSize.a_12, color Color.grey600]
        , textView
            [ text (formatTime advisory.recommendedDeparture)
            , textSize FontSize.a_24
            , fontStyle FontStyle.bold
            , color Color.black800
            ]
        , textView
            [ text ("(" <> show advisory.bufferMinutes <> " min buffer)")
            , textSize FontSize.a_12
            , color Color.grey600
            ]
        ]
    , setReminderButton advisory
    ]
```

#### 2.4.3 Modified Screens

**File**: `Frontend/ui-customer/src/Screens/SearchLocationFlow/SearchLocationScreen/View.purs`

Add time mode selector between destination input and results:

```purescript
-- In the main view function, add after destination input:
, timeModeSelector
    { currentMode: state.timeMode
    , onModeChange: \mode -> TimeModeChanged mode
    , targetTime: state.targetTime
    }
, if state.showTimePicker
    then timePickerModal
      { mode: state.timeMode
      , currentTime: state.targetTime
      , onTimeSet: \time -> TargetTimeSet time
      , onClose: TimePickerClosed
      }
    else emptyView
```

**File**: `Frontend/ui-customer/src/Screens/SearchLocationFlow/SearchLocationScreen/Controller.purs`

Add new actions:

```purescript
-- New actions:
data Action
  = -- ... existing actions ...
  | TimeModeChanged TimeMode
  | TargetTimeSet (Maybe String)
  | TimePickerClosed
  | BufferMinutesChanged Int
  | SaveTripClicked JourneyOption
  | SetReminderClicked JourneyId
```

#### 2.4.4 Service Layer Updates

**File**: `Frontend/ui-customer/src/Services/API.purs`
(extend existing)

```purescript
-- Add new API calls:
reachOnTimeSearch :: ReachOnTimeSearchReq -> FlowBT String ReachOnTimeSearchResp
savedTrips :: FlowBT String SavedTripsResp
createSavedTrip :: SavedTripCreateReq -> FlowBT String SavedTripResp
deleteSavedTrip :: String -> FlowBT String Unit
computeSavedTrip :: String -> FlowBT String DepartureAdvisoryResp
```

**Estimated effort**: 10 days
**Dependencies**: Backend APIs ready, React Native bridge for native time picker
**Tests**: Component snapshot tests, integration tests with mock APIs

---

### 2.5 `ny-react-native` — React Native Mobile App

#### 2.5.1 Native Time Picker Bridge

**New File**: `src/components/TimePicker/NativeTimePicker.tsx`

```typescript
import DateTimePicker from '@react-native-community/datetimepicker';

interface TimePickerProps {
  mode: 'arrive_by' | 'depart_at';
  initialTime: Date;
  onTimeSelected: (time: Date) => void;
  onDismiss: () => void;
  minimumDate?: Date;
}

export const NativeTimePicker: React.FC<TimePickerProps> = ({
  mode, initialTime, onTimeSelected, onDismiss, minimumDate
}) => {
  const [date, setDate] = useState(initialTime);
  const [showDatePicker, setShowDatePicker] = useState(true);
  const [showTimePicker, setShowTimePicker] = useState(false);

  return (
    <Modal visible animationType="slide" transparent>
      <View style={styles.container}>
        {showDatePicker && (
          <DateTimePicker
            value={date}
            mode="date"
            minimumDate={minimumDate || new Date()}
            onChange={(_, selectedDate) => {
              setDate(selectedDate || date);
              setShowDatePicker(false);
              setShowTimePicker(true);
            }}
          />
        )}
        {showTimePicker && (
          <DateTimePicker
            value={date}
            mode="time"
            minuteInterval={5}
            onChange={(_, selectedTime) => {
              if (selectedTime) onTimeSelected(selectedTime);
              setShowTimePicker(false);
            }}
          />
        )}
      </View>
    </Modal>
  );
};
```

#### 2.5.2 Push Notification Handler

**File**: `src/notifications/DepartureReminderHandler.ts`

```typescript
import notifee, { AndroidImportance } from '@notifee/react-native';

export async function handleDepartureReminder(data: DepartureReminderData) {
  const channelId = await notifee.createChannel({
    id: 'departure-reminders',
    name: 'Departure Reminders',
    importance: AndroidImportance.HIGH,
    sound: 'departure_alert',
  });

  await notifee.displayNotification({
    title: `Time to leave for ${data.tripName}!`,
    body: `Leave now to catch ${data.firstTransitMode} ${data.firstTransitRoute} at ${data.firstDepartureTime}`,
    android: {
      channelId,
      pressAction: { id: 'open-journey', launchActivity: 'default' },
      actions: [
        { title: 'Start Journey', pressAction: { id: 'start-journey' } },
        { title: 'Snooze 5 min', pressAction: { id: 'snooze' } },
      ],
    },
    ios: {
      categoryId: 'departure-reminder',
      sound: 'departure_alert.wav',
    },
    data: { journeyId: data.journeyId, savedTripId: data.savedTripId },
  });
}
```

**Estimated effort**: 5 days
**Dependencies**: PureScript frontend components, backend notification API
**Tests**: Component tests, notification handler tests

---

### 2.6 `location-tracking-service` — Real-Time Location

#### 2.6.1 ETA Update Endpoint

Extend existing location tracking to support ETA refinement:

**Enhancement**: When a rider is on a journey with time constraints, the location service should:
1. Track real-time position of nearby transit vehicles
2. Provide ETA updates for approaching buses/metros
3. Trigger alerts if the rider is at risk of missing their planned transit

```
New internal API:
POST /internal/eta/transit-proximity
{
  "riderLocation": { "lat": 12.9716, "lon": 77.5946 },
  "targetStopCode": "STOP123",
  "routeId": "ROUTE47B",
  "scheduledDeparture": "2026-03-13T08:20:00+05:30"
}

Response:
{
  "estimatedArrivalAtStop": "2026-03-13T08:18:00+05:30",
  "vehicleId": "TN01AB1234",
  "currentDelay": 120,  // seconds
  "isLive": true
}
```

**Estimated effort**: 3 days
**Dependencies**: GPS vehicle data availability
**Tests**: Integration tests with mock vehicle locations

---

### 2.7 `eta-compute` — ETA Computation Service

#### 2.7.1 Time-Constrained ETA

Extend ETA computation to support backward computation (from arrival to departure):

```
New endpoint:
POST /compute/reverse-eta
{
  "origin": { "lat": 12.9716, "lon": 77.5946 },
  "destination": { "lat": 13.0827, "lon": 80.2707 },
  "targetArrivalTime": "2026-03-13T09:30:00+05:30",
  "travelModes": ["BUS", "METRO", "WALK"],
  "includeTraffic": true
}

Response:
{
  "latestDeparture": "2026-03-13T08:27:00+05:30",
  "estimatedDuration": 3780,  // seconds
  "trafficFactor": 1.15,      // 15% traffic overhead
  "confidence": 0.82          // prediction confidence
}
```

**Estimated effort**: 4 days
**Dependencies**: Google Routes API, traffic data
**Tests**: Unit tests with various time/route scenarios

---

### 2.8 `nandi` — Dashboard/Analytics

#### 2.8.1 Feature Analytics Dashboard

Add dashboard panels for monitoring:
- "Arrive By" search volume and completion rates
- Average ETA prediction accuracy
- Notification delivery and engagement rates
- Saved trip usage patterns
- Risk level distribution (how many trips are Comfortable vs Tight)

**Estimated effort**: 3 days
**Dependencies**: Backend event logging in place

---

### 2.9 `gps-server` — GPS Data Feed

#### 2.9.1 Vehicle ETA Feed

Enhance GPS data processing to compute and cache per-stop ETAs for tracked vehicles:

```
Enhancement:
- For each tracked bus/metro, compute ETA to next N stops
- Store in Redis with TTL = update_interval * 3
- Key format: vehicle_eta:{vehicle_id}:{stop_code}
- Value: { eta_seconds: 420, updated_at: "...", confidence: 0.85 }
```

**Estimated effort**: 3 days
**Dependencies**: Vehicle GPS feed availability
**Tests**: Unit tests for ETA computation from GPS coordinates

---

## 3. Cross-Cutting Concerns

### 3.1 API Versioning

All new endpoints use the existing versioning scheme. Time-aware search is backward compatible — existing search continues to work as "Leave Now" (default).

### 3.2 Feature Flag

```haskell
-- In RiderConfig / MerchantConfig:
reachOnTimeEnabled :: Bool  -- Default: False
reachOnTimeNotificationsEnabled :: Bool  -- Default: False
```

Enable via dashboard per city/merchant for staged rollout.

### 3.3 Logging & Observability

```haskell
-- Standard logging for all Reach on Time operations:
logInfo $ "ReachOnTime search: timeMode=" <> show timeMode
  <> " targetTime=" <> show targetTime
  <> " routes=" <> show (length routes)

-- Metrics:
- reach_on_time_searches_total (counter, by time_mode)
- reach_on_time_advisory_risk_level (histogram, by risk_level)
- reach_on_time_notification_sent (counter)
- reach_on_time_notification_engaged (counter)
- reach_on_time_eta_accuracy (histogram, predicted_vs_actual)
```

### 3.4 Error Handling

```haskell
data ReachOnTimeError
  = TargetTimeInPast
  | TargetTimeTooSoon Seconds    -- "Not enough travel time"
  | NoTransitAtTime UTCTime      -- "No transit services at this time"
  | GtfsDataUnavailable Text     -- Fallback to Google Transit
  | SavedTripLimitExceeded Int   -- Max 20 saved trips
  deriving (Show)
```

### 3.5 Caching Strategy

| Data | Cache | TTL | Invalidation |
|------|-------|-----|-------------|
| GTFS schedules | In-memory (Rust) | Until next GTFS update | Polling-based |
| Route search results | Redis | 5 min | Time-based expiry |
| Departure advisory | Redis | 2 min | Time-based (advisory changes as time passes) |
| Saved trips | PostgreSQL + Redis | 1 hour | On update |
| User preferences | Redis | 24 hours | On update |

---

## 4. Testing Strategy

### 4.1 Unit Tests

| Component | Test Cases | Framework |
|-----------|-----------|-----------|
| DepartureAdvisor | Risk level computation, buffer calculation, edge cases | HUnit (Haskell) |
| ScheduleQuery | Time window filtering, transfer computation | HUnit |
| GTFS Schedule Service | Departure lookups, trip connections | Rust #[test] |
| TimeModeSelector | Mode switching, state management | PureScript Spec |
| TimePickerModal | Date/time selection, validation | Jest (React Native) |

### 4.2 Integration Tests

| Flow | Description | Tools |
|------|------------|-------|
| Arrive By Search | End-to-end: set arrival time → get route options with advisories | Newman/Postman |
| Depart At Search | Set departure time → get arrival estimates | Newman |
| Saved Trip CRUD | Create, read, update, delete saved trips | Newman |
| Notification Flow | Saved trip → scheduler → push notification | Manual + logs |
| GTFS Time Query | Schedule query → correct departures returned | Rust integration test |

### 4.3 Performance Tests

| Scenario | Target | Tool |
|----------|--------|------|
| Concurrent time-aware searches | 1000 req/s, p95 < 2s | k6 |
| GTFS schedule query under load | 5000 req/s, p95 < 500ms | k6 |
| Notification batch processing | 10K trips in < 60s | Custom benchmark |

### 4.4 User Acceptance Tests

| Scenario | Steps |
|----------|-------|
| Morning commute planning | Search "Arrive By 9:30 AM" for office route. Verify risk badges. |
| Airport trip | Search "Arrive By 5:30 PM" for airport. Verify buffer recommendations. |
| Late night | Search "Arrive By 11:00 PM" when last bus is at 10:30. Verify "Too Late" + taxi option. |
| Save and notify | Save commute trip, wait for notification at computed time. |
| Real-time update | Start journey, verify live bus tracking updates ETA. |

---

## 5. Deployment Order

```
Day 1:     GTFS data quality monitoring pipeline (Phase 0)
           Schedule adherence logging instrumentation

Week 1-2:  gtfs-inmemory-server-rust (schedule APIs)
           shared-kernel (new types)
           nammayatri DB migrations
           OpenTripPlanner container deployment

Week 3-4:  nammayatri/rider-app (search + advisory logic)
           OTP integration for "Arrive By"
           eta-compute (reverse ETA)

Week 5-6:  Frontend/ui-customer (PureScript components)
           ny-react-native (native time picker, notifications)

Week 7-8:  nammayatri/rider-app (saved trips, notification scheduler)
           location-tracking-service (transit proximity)
           gps-server (vehicle ETA cache)
           Crowding buffer + safety routing logic

Week 9-10: nandi (analytics dashboard)
           Integration testing across all services
           Staged rollout (Chennai first)

Week 11-12: Bug fixes and polish
            Performance optimization
            Full rollout
```

---

## 6. Frameworks & Patterns to Reuse

### 6.1 Existing Patterns to Leverage

| Pattern | Existing Usage | Reuse For |
|---------|---------------|-----------|
| JourneyLeg type class | `Lib/JourneyLeg/Interface.hs` | Time-constrained leg operations |
| JourneyModule orchestration | `Lib/JourneyModule/Base.hs` | Time-aware journey init |
| NammaDSL code generation | `spec/Storage/*.yaml`, `spec/API/*.yaml` | SavedTrip schema + APIs |
| MultiModal user preferences | `MultimodalUserPreferences` type | Extend with time preferences |
| Redis locking | `Redis.withLockRedisAndReturnValue` | Departure computation locking |
| Scheduler jobs | `rider-app/Scheduler/` | Departure reminder scheduler |
| Push notifications | Existing FCM integration | Departure reminders |
| GTFS service integration | `Storage/GraphqlQueries/RouteStopTimeTable.hs` | Schedule queries |
| Beckn search flow | `Domain/Action/UI/Search.hs` | Time-constrained search |
| Cached queries pattern | `Storage/CachedQueries/` | Advisory caching |

### 6.2 New Reusable Frameworks

| Framework | Purpose | Reusable For |
|-----------|---------|-------------|
| DepartureAdvisor | Compute departure windows with risk | Any time-sensitive feature |
| ScheduleQueryService | Time-based GTFS querying | Any schedule-dependent feature |
| RecurringTaskScheduler | Schedule recurring computations | Passes, subscriptions, etc. |
| RiskLevelEngine | Compute risk from time buffers | Driver ETA risk, delivery timing |

### 6.3 Open Source Libraries

| Library | Usage | Repo |
|---------|-------|------|
| `@react-native-community/datetimepicker` | Native time picker | ny-react-native |
| `@notifee/react-native` | Rich push notifications | ny-react-native |
| `chrono` (Rust) | Date/time handling in GTFS server | gtfs-inmemory-server-rust |
| `gtfs-structures` (Rust) | GTFS data parsing | gtfs-inmemory-server-rust |

---

## 7. Risk Mitigation

| Risk | Mitigation |
|------|------------|
| GTFS data quality | Validate GTFS feed on import; flag missing stop_times |
| Google Transit API rate limits | Cache results aggressively; batch queries |
| Notification timing accuracy | Use server-side scheduling (not client alarms) |
| Complex state management | Reuse existing JourneyModule patterns |
| Performance under load | Pre-compute popular route advisories; cache in Redis |
| Backward compatibility | Feature flag; existing search unchanged |
