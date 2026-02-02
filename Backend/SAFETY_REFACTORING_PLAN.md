# Safety APIs Refactoring Plan

**Objective**: Refactor safety APIs and tables to follow the shared-services pattern, similar to IssueManagement, where core logic resides in shared-services while platform-specific implementations remain in rider-app and provider-app.

**Reference Implementation**: `Backend/lib/shared-services/src/IssueManagement/`

---

## Table of Contents
1. [Current State Analysis](#1-current-state-analysis)
2. [Target Architecture](#2-target-architecture)
3. [Migration Phases](#3-migration-phases)
4. [Detailed Implementation Steps](#4-detailed-implementation-steps)

---

## 1. Current State Analysis

### Current File Locations

**Rider Platform:**
- `rider-app/Main/src/Domain/Action/UI/Sos.hs` (769 lines - MAIN)
- `rider-app/Main/src/Domain/Action/Dashboard/Sos.hs`
- `rider-app/Main/src/Domain/Action/Dashboard/SosMedia.hs`
- `rider-app/Main/src/SharedLogic/SosLocationTracking.hs` (71 lines)
- `rider-app/Main/src/Storage/CachedQueries/Sos.hs`
- `rider-app/Main/src/Storage/Clickhouse/Sos.hs`
- `rider-app/Main/src/API/UI/Sos.hs`
- `rider-app/Main/spec/Storage/sos.yaml`
- `rider-app/Main/spec/Storage/SafetySettings.yaml`
- `rider-app/Main/spec/API/sos.yaml`

**Provider Platform:**
- `dynamic-offer-driver-app/Main/src/Domain/Action/UI/SafetyWebhook.hs`

**Shared Services (Partial):**
- `lib/shared-services/src/Safety/Domain/Types/Common.hs`
- `lib/shared-services/src/Safety/Storage/BeamFlow.hs`
- `lib/shared-services/spec/Safety/Storage/SafetySettings.yaml`
- `lib/shared-services/spec/Safety/Storage/Sos.yaml`

### Key Observations

1. **Schema Already Partially Moved**: `Sos` and `SafetySettings` schemas exist in shared-services spec
2. **Heavy Merchant Coupling**: `RiderConfig` has 20+ safety-related fields
3. **External Integrations**: Exotel (IVR), Kapture (tickets), ERSS (police), S3 (media)
4. **Dual Entity Support**: Ride-based and Non-ride SOS scenarios
5. **Multi-Layer Storage**: Redis (hot), PostgreSQL (warm), ClickHouse (cold)

---

## 2. Target Architecture

### Directory Structure

```
Backend/lib/shared-services/src/Safety/
├── API/
│   ├── Beckn/                              # Beckn protocol integration
│   │   └── Sos.hs                          # Beckn SOS handlers
│   ├── UI/
│   │   └── Sos.hs                          # UI API endpoint definitions
│   └── Dashboard/
│       ├── Sos.hs                          # Dashboard API definitions
│       └── SosMedia.hs                     # Media management API
├── Common/
│   ├── UI/
│   │   └── Sos.hs                          # UI request/response types
│   ├── Dashboard/
│   │   └── Sos.hs                          # Dashboard request/response types
│   └── Common.hs                           # Shared types (RideShareOptions, SosType, etc.)
├── Domain/
│   ├── Action/
│   │   ├── UI/
│   │   │   └── Sos.hs                      # Core UI operations (platform-agnostic)
│   │   └── Dashboard/
│   │       ├── Sos.hs                      # Core dashboard operations
│   │       └── SosMedia.hs                 # Media operations
│   └── Types/
│       ├── Sos/
│       │   ├── Sos.hs                      # Main SOS entity
│       │   ├── SafetySettings.hs           # User safety preferences
│       │   ├── SosMockDrill.hs             # Mock drill entity
│       │   ├── SosLocationData.hs          # Location tracking entity
│       │   └── SosConfig.hs                # Configuration entity
│       └── MediaFile.hs                    # Media file entity (reuse from IssueManagement if applicable)
├── Storage/
│   ├── Beam/
│   │   ├── Sos.hs                          # Beam schema for SOS
│   │   └── SafetySettings.hs               # Beam schema for settings
│   ├── Queries/
│   │   ├── Sos.hs                          # Direct SQL queries
│   │   ├── SafetySettings.hs               # Settings queries
│   │   └── SosExtra.hs                     # Extra/complex queries
│   ├── CachedQueries/
│   │   ├── Sos.hs                          # Redis-backed queries
│   │   └── SosMockDrill.hs                 # Mock drill caching
│   ├── Clickhouse/
│   │   └── Sos.hs                          # ClickHouse analytics
│   └── BeamFlow.hs                         # Transaction wrapper (already exists)
├── SharedLogic/
│   ├── LocationTracking.hs                 # Real-time location tracking logic
│   └── NotificationHelper.hs               # Emergency notification helpers
└── Tools/
    └── Error.hs                            # Safety-specific errors

Backend/lib/shared-services/spec/Safety/
├── RiderPlatform/
│   ├── API/
│   │   ├── Sos.yaml                        # Rider/Customer-facing APIs (13 endpoints)
│   │   └── SosMedia.yaml                   # Media upload APIs
│   └── Dashboard/
│       └── Sos.yaml                        # Rider dashboard APIs
├── ProviderPlatform/
│   ├── API/
│   │   └── SafetyWebhook.yaml              # Provider webhook APIs
│   └── Dashboard/
│       └── Safety.yaml                     # Provider dashboard APIs
└── Storage/
    ├── Sos.yaml                            # Already exists
    └── SafetySettings.yaml                 # Already exists
```

### ServiceHandle Pattern

Following IssueManagement's pattern, create dependency injection handles:

**In Shared-Services** (`Safety/Domain/Action/UI/Sos.hs`):
```haskell
data ServiceHandle m = ServiceHandle
  { -- Entity Lookups
    findPersonById :: Id Person -> m (Maybe Person)
  , findRideById :: Id Ride -> Id Merchant -> m (Maybe Ride)
  , findBookingById :: Id Booking -> m (Maybe Booking)
  , findMerchantById :: Id Merchant -> m (Maybe Merchant)
  , findMOCityById :: Id MerchantOperatingCity -> m (Maybe MerchantOperatingCity)

    -- Configuration
  , getSafetyConfig :: Id Merchant -> Id MerchantOperatingCity -> m SosConfig
  , getPersonDefaultEmergencyNumbers :: Id Person -> m [PersonDefaultEmergencyNumber]

    -- External Integrations (abstracted)
  , createTicket :: Id Merchant -> Id MerchantOperatingCity -> CreateTicketReq -> m CreateTicketResp
  , updateTicket :: Id Merchant -> Id MerchantOperatingCity -> UpdateTicketReq -> m UpdateTicketResp
  , sendNotification :: Id Person -> NotificationReq -> m APISuccess
  , sendSMS :: Text -> Text -> m APISuccess
  , uploadMediaToStorage :: MediaUploadReq -> m MediaUploadResp
  , makeIVRCall :: IVRCallReq -> m IVRCallResp
  , reportPoliceIncident :: PoliceIncidentReq -> m PoliceIncidentResp

    -- Tracking & Analytics
  , storeLocationUpdate :: Id Sos -> LocationData -> m ()
  , getLocationHistory :: Id Sos -> m [LocationData]
  , pushToClickhouse :: ClickhouseSosData -> m ()

    -- Ride Context
  , getRideDetails :: Id Ride -> Id Merchant -> m RideDetails
  , getDriverDetails :: Id Person -> Id Merchant -> m DriverDetails

    -- Notifications
  , notifyEmergencyContacts :: Id Person -> SosNotificationData -> m APISuccess
  , notifySafetyTeam :: Id Merchant -> Id MerchantOperatingCity -> SosNotificationData -> m APISuccess
  }
```

**In Rider-App** (`rider-app/Main/src/Domain/Action/UI/Sos.hs`):
```haskell
-- Implement the handle with rider-specific logic
riderSosHandle :: ServiceHandle Flow
riderSosHandle = ServiceHandle
  { findPersonById = QPerson.findById
  , findRideById = \rideId merchantId -> QRide.findById rideId
  , findBookingById = QBooking.findById
  , getSafetyConfig = getRiderSafetyConfig  -- Maps RiderConfig to SosConfig
  , createTicket = Tools.Ticket.createTicket
  , sendNotification = Tools.Notifications.sendNotification
  , makeIVRCall = Exotel.makeCall
  , reportPoliceIncident = ERSS.reportIncident
  , -- ... other implementations
  }

-- Adapter function to convert RiderConfig to SosConfig
getRiderSafetyConfig :: Id Merchant -> Id MerchantOperatingCity -> Flow SosConfig
getRiderSafetyConfig merchantId mocId = do
  riderConfig <- QRC.findByMerchantOperatingCityId mocId
  pure $ SosConfig
    { enableSupportForSafety = riderConfig.enableSupportForSafety
    , videoFileSizeUpperLimit = riderConfig.videoFileSizeUpperLimit
    , safetyCheckStartTime = riderConfig.safetyCheckStartTime
    , safetyCheckEndTime = riderConfig.safetyCheckEndTime
    , trackingUrlPattern = riderConfig.trackingShortUrlPattern
    , ivrTriggerDelay = riderConfig.ivrTriggerDelay
    , policeTriggerDelay = riderConfig.policeTriggerDelay
    , incidentReportEnabled = riderConfig.incidentReportSupport
    , -- ... map all safety-related fields
    }
```

**In Provider-App** (`dynamic-offer-driver-app/Main/src/Domain/Action/UI/Safety.hs`):
```haskell
-- Implement the handle with provider-specific logic
providerSafetyHandle :: ServiceHandle Flow
providerSafetyHandle = ServiceHandle
  { findPersonById = QDriver.findById  -- Different Person query
  , getSafetyConfig = getProviderSafetyConfig  -- Maps DriverPoolConfig to SosConfig
  , createTicket = Tools.Ticket.createProviderTicket
  , -- ... provider-specific implementations
  }
```

---

## 3. Migration Phases

### Phase 1: Planning Schema & Storage Migration (Day 1-3)
- Move schemas to shared-services
- Create Beam definitions
- Implement Storage layer (Queries, CachedQueries, Clickhouse)
- Verify schema compatibility with existing tables

### Phase 2: Domain Types Extraction (Day 3-4)
- Move domain types to `Safety/Domain/Types/`
- Create common request/response types
- Implement ServiceHandle interface
- Create SosConfig abstraction

### Phase 3: Core Logic Migration (Day 4-6)
- Extract pure business logic to `Safety/Domain/Action/`
- Remove merchant-specific coupling
- Implement ServiceHandle pattern throughout
- Create platform-agnostic action handlers

### Phase 4: Platform-Specific Adapters (Day 6-7)
- Implement rider-app ServiceHandle
- Implement provider-app ServiceHandle
- Create config adapters (RiderConfig → SosConfig)
- Wire up API handlers

### Phase 5: API Specification (Day 7-8)
- Create YAML specs in `shared-services/spec/Safety/`
- Generate API types using DSL
- Update API handlers to use generated types
- Validate endpoint compatibility

### Phase 6: Integration Testing (Day 8-9)
- Test rider-app endpoints
- Test provider-app endpoints
- Test dashboard APIs
- Load testing for performance regression

---

## 4. Detailed Implementation Steps

### Step 1: Set Up Shared-Services Directory Structure

**Task 1.1**: Create directory structure
```bash
mkdir -p Backend/lib/shared-services/src/Safety/{API/{UI,Dashboard,Beckn},Common/{UI,Dashboard},Domain/{Action/{UI,Dashboard},Types/Sos},Storage/{Beam,Queries,CachedQueries,Clickhouse},SharedLogic,Tools}
mkdir -p Backend/lib/shared-services/spec/Safety/{RiderPlatform/{API,Dashboard},ProviderPlatform/{API,Dashboard},Storage}
```

**Task 1.2**: Create placeholder files
```bash
touch Backend/lib/shared-services/src/Safety/API/UI/Sos.hs
touch Backend/lib/shared-services/src/Safety/Domain/Action/UI/Sos.hs
touch Backend/lib/shared-services/src/Safety/Domain/Types/Sos/Sos.hs
```

**Task 1.3**: Update `shared-services.cabal`
- Add new modules to `exposed-modules`
- Add dependencies: `servant`, `beam-postgres`, `redis`, `clickhouse-haskell`

---

### Step 2: Migrate Storage Layer

**Task 2.1**: Move Beam schemas from rider-app to shared-services

**Source Files**:
- `rider-app/Main/src-read-only/Storage/Beam/Sos.hs`
- `rider-app/Main/src-read-only/Storage/Beam/SafetySettings.hs`

**Target**:
- `shared-services/src/Safety/Storage/Beam/Sos.hs`
- `shared-services/src/Safety/Storage/Beam/SafetySettings.hs`

**Changes Required**:
- Update import paths to shared-services
- Remove rider-app specific dependencies
- Ensure compatibility with existing database schema

**Task 2.2**: Move Storage Queries

**Source**:
- `rider-app/Main/src/Storage/Queries/Sos.hs` (auto-generated)
- `rider-app/Main/src/Storage/Queries/SafetySettings.hs` (auto-generated)

**Target**:
- `shared-services/src/Safety/Storage/Queries/Sos.hs`
- `shared-services/src/Safety/Storage/Queries/SafetySettings.hs`

**Task 2.3**: Move CachedQueries

**Source**:
- `rider-app/Main/src/Storage/CachedQueries/Sos.hs`

**Target**:
- `shared-services/src/Safety/Storage/CachedQueries/Sos.hs`

**Changes**:
- Abstract Redis key prefixes (use `Identifier` pattern from IssueManagement)
- Add `riderPlatform:` or `providerPlatform:` prefixes dynamically

**Task 2.4**: Move Clickhouse Storage

**Source**:
- `rider-app/Main/src/Storage/Clickhouse/Sos.hs`

**Target**:
- `shared-services/src/Safety/Storage/Clickhouse/Sos.hs`

**Task 2.5**: Verify BeamFlow abstraction

**Check**: `shared-services/src/Safety/Storage/BeamFlow.hs` already exists
- Ensure it covers both Sos and SafetySettings tables
- Add any missing constraint aliases

---

### Step 3: Migrate Domain Types

**Task 3.1**: Move core domain types

**Source**: `rider-app/Main/src-read-only/Domain/Types/Sos.hs`

**Target**: `shared-services/src/Safety/Domain/Types/Sos/Sos.hs`

**Type**: `Sos` entity
```haskell
data Sos = Sos
  { id :: Id Sos
  , personId :: Id Person
  , rideId :: Maybe (Id Ride)
  , flow :: SosType
  , status :: SosStatus
  , ticketId :: Maybe Text
  , mediaFiles :: [Id MediaFile]
  , trackingExpiresAt :: Maybe UTCTime
  , sosState :: Maybe SosState
  , entityType :: Maybe SosEntityType
  , merchantId :: Maybe (Id Merchant)
  , merchantOperatingCityId :: Maybe (Id MerchantOperatingCity)
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
```

**Task 3.2**: Move SafetySettings type

**Source**: `rider-app/Main/src-read-only/Domain/Types/SafetySettings.hs`

**Target**: `shared-services/src/Safety/Domain/Types/Sos/SafetySettings.hs`

**Type**: `SafetySettings` entity

**Task 3.3**: Extract enums to Common module

**Target**: `shared-services/src/Safety/Common/Common.hs`

**Enums to move**:
- `SosType` (Police | CustomerCare | EmergencyContact | SafetyFlow | CSAlertSosTicket | AudioRecording | KaptureDashboard)
- `SosStatus` (Resolved | NotResolved | Pending | MockPending | MockResolved)
- `SosState` (LiveTracking | SosActive)
- `SosEntityType` (Ride | NonRide)
- `RideShareOptions` (already in Common.hs - verify)

**Task 3.4**: Create SosConfig abstraction

**Target**: `shared-services/src/Safety/Domain/Types/Sos/SosConfig.hs`

```haskell
-- Abstract configuration to decouple from RiderConfig
data SosConfig = SosConfig
  { enableSupportForSafety :: Bool
  , videoFileSizeUpperLimit :: Int
  , timeDiffFromUtc :: Seconds
  , enableEmergencyContactAddedMessage :: Bool
  , safetyCheckStartTime :: Seconds
  , safetyCheckEndTime :: Seconds
  , trackingUrlPattern :: Text
  , autoUnblockSafetyCenterAfterDays :: Int
  , kaptureQueue :: Text
  , kaptureSosQueue :: Maybe Text
  , kaptureDisposition :: Text
  , ivrTriggerDelay :: NominalDiffTime
  , policeTriggerDelay :: NominalDiffTime
  , incidentReportEnabled :: Bool
  , cxAgentDetails :: [CxAgentDetail]
  , exotelAppIdMapping :: Maybe ExotelMapping
  , hardLimitForSafetyJobs :: Int
  , postRideSafetyNotificationDelay :: NominalDiffTime
  , useUserSettingsForSafetyIVR :: Bool
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data CxAgentDetail = CxAgentDetail
  { agentMobileNumber :: Text
  , agentName :: Text
  , agentEmail :: Text
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)
```

**Task 3.5**: Create common request/response types

**Target**: `shared-services/src/Safety/Common/UI/Sos.hs`

**Types to define**:
- `SosReq` (create SOS request)
- `SosRes` (SOS response)
- `SosDetailsRes` (detailed SOS info)
- `UpdateSosStatusReq`
- `MarkRideAsSafeReq`
- `CreateMockSosReq`
- `UpdateLocationReq`
- `GetTrackingRes`
- `SosMediaUploadReq`
- `SosMediaUploadRes`

**Task 3.6**: Create dashboard types

**Target**: `shared-services/src/Safety/Common/Dashboard/Sos.hs`

Dashboard-specific request/response types (if any)

---

### Step 4: Implement ServiceHandle Interface

**Task 4.1**: Define ServiceHandle type

**Target**: `shared-services/src/Safety/Domain/Action/UI/Sos.hs`

```haskell
{-# LANGUAGE RankNTypes #-}

module Safety.Domain.Action.UI.Sos where

import qualified Safety.Domain.Types.Sos.Sos as DSos
import qualified Safety.Domain.Types.Sos.SafetySettings as DSettings
import qualified Safety.Domain.Types.Sos.SosConfig as DConfig

data ServiceHandle m = ServiceHandle
  { -- Entity Lookups
    findPersonById :: Id Person -> m (Maybe Person)
  , findRideById :: Id Ride -> Id Merchant -> m (Maybe Ride)
  , findBookingById :: Id Booking -> m (Maybe Booking)
  , findMerchantById :: Id Merchant -> m (Maybe Merchant)
  , findMOCityById :: Id MerchantOperatingCity -> m (Maybe MerchantOperatingCity)

    -- Configuration
  , getSafetyConfig :: Id Merchant -> Id MerchantOperatingCity -> m DConfig.SosConfig
  , getPersonDefaultEmergencyNumbers :: Id Person -> m [PersonDefaultEmergencyNumber]

    -- External Integrations (abstracted)
  , createTicket :: Id Merchant -> Id MerchantOperatingCity -> CreateTicketReq -> m CreateTicketResp
  , updateTicket :: Id Merchant -> Id MerchantOperatingCity -> UpdateTicketReq -> m UpdateTicketResp
  , sendNotification :: Id Person -> NotificationReq -> m APISuccess
  , sendSMS :: Text -> Text -> m APISuccess
  , uploadMediaToStorage :: MediaUploadReq -> m MediaUploadResp
  , makeIVRCall :: IVRCallReq -> m IVRCallResp
  , reportPoliceIncident :: PoliceIncidentReq -> m PoliceIncidentResp

    -- Tracking & Analytics
  , storeLocationUpdate :: Id DSos.Sos -> LocationData -> m ()
  , getLocationHistory :: Id DSos.Sos -> m [LocationData]
  , pushToClickhouse :: ClickhouseSosData -> m ()

    -- Ride Context
  , getRideDetails :: Id Ride -> Id Merchant -> m RideDetails
  , getDriverDetails :: Id Person -> Id Merchant -> m DriverDetails

    -- Notifications
  , notifyEmergencyContacts :: Id Person -> SosNotificationData -> m APISuccess
  , notifySafetyTeam :: Id Merchant -> Id MerchantOperatingCity -> SosNotificationData -> m APISuccess

    -- Scheduler
  , schedulePoliceAlert :: Id DSos.Sos -> NominalDiffTime -> m ()
  , scheduleIVRCallback :: Id DSos.Sos -> NominalDiffTime -> m ()
  , scheduleSafetyCheckNotification :: Id Booking -> NominalDiffTime -> m ()
  }
```

---

### Step 5: Extract Core Business Logic

**Task 5.1**: Migrate SOS creation logic

**Source**: `rider-app/Main/src/Domain/Action/UI/Sos.hs::createSos`

**Target**: `shared-services/src/Safety/Domain/Action/UI/Sos.hs::createSos`

**Function Signature**:
```haskell
createSos ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r)
  => ServiceHandle m
  -> Id Person
  -> SosReq
  -> m SosRes
```

**Changes**:
- Replace `QRiderConfig.findByMerchantOperatingCityId` with `getSafetyConfig handle`
- Replace `Tools.Ticket.createTicket` with `createTicket handle`
- Replace `Tools.Notifications.sendNotification` with `sendNotification handle`
- Remove direct imports of rider-app modules

**Task 5.2**: Migrate update status logic

**Source**: `rider-app/Main/src/Domain/Action/UI/Sos.hs::updateSosStatus`

**Target**: `shared-services/src/Safety/Domain/Action/UI/Sos.hs::updateSosStatus`

**Task 5.3**: Migrate mark ride safe logic

**Source**: `rider-app/Main/src/Domain/Action/UI/Sos.hs::markRideAsSafe`

**Target**: `shared-services/src/Safety/Domain/Action/UI/Sos.hs::markRideAsSafe`

**Task 5.4**: Migrate mock SOS logic

**Source**: `rider-app/Main/src/Domain/Action/UI/Sos.hs::createMockSos`

**Target**: `shared-services/src/Safety/Domain/Action/UI/Sos.hs::createMockSos`

**Task 5.5**: Migrate police call logic

**Source**: `rider-app/Main/src/Domain/Action/UI/Sos.hs::callPolice`

**Target**: `shared-services/src/Safety/Domain/Action/UI/Sos.hs::callPolice`

**Task 5.6**: Migrate location tracking logic

**Source**: `rider-app/Main/src/SharedLogic/SosLocationTracking.hs`

**Target**: `shared-services/src/Safety/SharedLogic/LocationTracking.hs`

**Functions to migrate**:
- `updateLocationForSos`
- `getTrackingDetails`
- `startTracking`
- `updateSosState`

**Task 5.7**: Migrate media upload logic

**Source**: `rider-app/Main/src/Domain/Action/Dashboard/SosMedia.hs`

**Target**: `shared-services/src/Safety/Domain/Action/Dashboard/SosMedia.hs`

**Task 5.8**: Migrate IVR outcome handler

**Source**: `rider-app/Main/src/Domain/Action/UI/Sos.hs::handleIvrOutcome`

**Target**: `shared-services/src/Safety/Domain/Action/UI/Sos.hs::handleIvrOutcome`

**Task 5.9**: Create helper functions

**Target**: `shared-services/src/Safety/SharedLogic/NotificationHelper.hs`

```haskell
module Safety.SharedLogic.NotificationHelper where

-- Helper to notify emergency contacts
notifyEmergencyContactsHelper ::
  (MonadFlow m)
  => ServiceHandle m
  -> Id Person
  -> SosNotificationData
  -> m APISuccess

-- Helper to notify safety team
notifySafetyTeamHelper ::
  (MonadFlow m)
  => ServiceHandle m
  -> Id Merchant
  -> Id MerchantOperatingCity
  -> SosNotificationData
  -> m APISuccess

-- Helper to build tracking URL
buildTrackingUrl :: Text -> Id Sos -> ShortId Sos -> Text
```

---

### Step 6: Create Platform-Specific Adapters

**Task 6.1**: Implement rider-app ServiceHandle

**Target**: `rider-app/Main/src/Domain/Action/UI/Sos.hs`

```haskell
module Domain.Action.UI.Sos
  ( module Reexport
  , API
  , handler
  , riderSosHandle
  ) where

import qualified Safety.Domain.Action.UI.Sos as SharedSos
import qualified Safety.Domain.Types.Sos.SosConfig as SosConfig
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.RiderConfig as QRC
import qualified Tools.Ticket as Ticket
import qualified Tools.Notifications as Notif
import qualified SharedLogic.Exotel as Exotel
import qualified SharedLogic.ERSS as ERSS

-- Re-export shared types
import Safety.Domain.Action.UI.Sos as Reexport (SosReq, SosRes, UpdateSosStatusReq, MarkRideAsSafeReq)

-- Implement ServiceHandle for rider platform
riderSosHandle :: SharedSos.ServiceHandle Flow
riderSosHandle = SharedSos.ServiceHandle
  { findPersonById = QPerson.findById
  , findRideById = \rideId merchantId -> QRide.findById rideId
  , findBookingById = QBooking.findById
  , findMerchantById = QMerchant.findById
  , findMOCityById = QMOC.findById
  , getSafetyConfig = getRiderSafetyConfig
  , getPersonDefaultEmergencyNumbers = QPDEN.findAllByPersonId
  , createTicket = Ticket.createTicket
  , updateTicket = Ticket.updateTicket
  , sendNotification = Notif.sendNotification
  , sendSMS = Notif.sendSMS
  , uploadMediaToStorage = S3.uploadFile
  , makeIVRCall = Exotel.makeCall
  , reportPoliceIncident = ERSS.reportIncident
  , storeLocationUpdate = Redis.setLocationData
  , getLocationHistory = Redis.getLocationHistory
  , pushToClickhouse = Clickhouse.pushSosEvent
  , getRideDetails = getRideDetailsHelper
  , getDriverDetails = getDriverDetailsHelper
  , notifyEmergencyContacts = notifyEmergencyContactsRider
  , notifySafetyTeam = notifySafetyTeamRider
  , schedulePoliceAlert = Scheduler.schedulePoliceAlert
  , scheduleIVRCallback = Scheduler.scheduleIVRCallback
  , scheduleSafetyCheckNotification = Scheduler.scheduleSafetyCheckNotification
  }

-- Adapter: RiderConfig → SosConfig
getRiderSafetyConfig :: Id Merchant -> Id MerchantOperatingCity -> Flow SosConfig.SosConfig
getRiderSafetyConfig merchantId mocId = do
  riderConfig <- QRC.findByMerchantOperatingCityId mocId
    >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
  pure $ SosConfig.SosConfig
    { enableSupportForSafety = riderConfig.enableSupportForSafety
    , videoFileSizeUpperLimit = riderConfig.videoFileSizeUpperLimit
    , timeDiffFromUtc = riderConfig.timeDiffFromUtc
    , enableEmergencyContactAddedMessage = riderConfig.enableEmergencyContactAddedMessage
    , safetyCheckStartTime = riderConfig.safetyCheckStartTime
    , safetyCheckEndTime = riderConfig.safetyCheckEndTime
    , trackingUrlPattern = riderConfig.trackingShortUrlPattern
    , autoUnblockSafetyCenterAfterDays = riderConfig.autoUnblockSafetyCenterAfterDays
    , kaptureQueue = riderConfig.kaptureConfig.queue
    , kaptureSosQueue = riderConfig.kaptureConfig.sosQueue
    , kaptureDisposition = riderConfig.kaptureConfig.disposition
    , ivrTriggerDelay = riderConfig.ivrTriggerDelay
    , policeTriggerDelay = riderConfig.policeTriggerDelay
    , incidentReportEnabled = riderConfig.incidentReportSupport
    , cxAgentDetails = maybe [] (map convertCxAgent) riderConfig.cxAgentDetails
    , exotelAppIdMapping = riderConfig.exotelAppIdMapping
    , hardLimitForSafetyJobs = riderConfig.hardLimitForSafetyJobs
    , postRideSafetyNotificationDelay = riderConfig.postRideSafetyNotificationDelay
    , useUserSettingsForSafetyIVR = riderConfig.useUserSettingsForSafetyIVR
    }
  where
    convertCxAgent agent = SosConfig.CxAgentDetail
      { agentMobileNumber = agent.agentMobileNumber
      , agentName = agent.agentName
      , agentEmail = agent.agentEmail
      }

-- API handler using shared logic
handler :: FlowServer API
handler = do
  createSos merchantId personId req -> SharedSos.createSos riderSosHandle personId req
  updateSosStatus sosId req -> SharedSos.updateSosStatus riderSosHandle sosId req
  markRideAsSafe sosId req -> SharedSos.markRideAsSafe riderSosHandle sosId req
  -- ... delegate all handlers to shared-services
```

**Task 6.2**: Implement provider-app ServiceHandle

**Target**: `dynamic-offer-driver-app/Main/src/Domain/Action/UI/Safety.hs`

```haskell
module Domain.Action.UI.Safety where

import qualified Safety.Domain.Action.UI.Sos as SharedSos
import qualified Safety.Domain.Types.Sos.SosConfig as SosConfig
import qualified Storage.Queries.Driver as QDriver
import qualified Storage.Queries.DriverPoolConfig as QDPC

-- Provider-specific ServiceHandle
providerSafetyHandle :: SharedSos.ServiceHandle Flow
providerSafetyHandle = SharedSos.ServiceHandle
  { findPersonById = QDriver.findById  -- Drivers are "Person" in provider context
  , findRideById = QRide.findById
  , getSafetyConfig = getProviderSafetyConfig  -- Map DriverPoolConfig to SosConfig
  , createTicket = Ticket.createProviderTicket
  , sendNotification = Notif.sendDriverNotification
  , makeIVRCall = Exotel.makeDriverCall
  , reportPoliceIncident = ERSS.reportDriverIncident
  , notifyEmergencyContacts = notifyDriverEmergencyContacts
  , notifySafetyTeam = notifyProviderSafetyTeam
  -- ... other provider-specific implementations
  }

-- Adapter: DriverPoolConfig → SosConfig
getProviderSafetyConfig :: Id Merchant -> Id MerchantOperatingCity -> Flow SosConfig.SosConfig
getProviderSafetyConfig merchantId mocId = do
  driverPoolConfig <- QDPC.findByMerchantOperatingCityId mocId
    >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
  pure $ SosConfig.SosConfig
    { enableSupportForSafety = driverPoolConfig.enableSafetyForDrivers
    , videoFileSizeUpperLimit = driverPoolConfig.videoFileSizeLimit
    -- ... map provider-specific config fields
    , trackingUrlPattern = driverPoolConfig.driverTrackingUrlPattern
    , kaptureQueue = driverPoolConfig.providerKaptureQueue
    -- ... other mappings
    }
```

**Task 6.3**: Update SafetyWebhook handler

**Target**: `dynamic-offer-driver-app/Main/src/Domain/Action/UI/SafetyWebhook.hs`

- Keep as-is (provider-specific logic)
- May optionally use shared types if applicable
- Add `getSosDetails` call to shared-services if needed

---

### Step 7: Create API Specifications

**Task 7.1**: Create rider platform API spec

**Target**: `shared-services/spec/Safety/RiderPlatform/API/Sos.yaml`

```yaml
# Based on rider-app/Main/spec/API/sos.yaml
endpoints:
  - name: GetSosDetails
    method: GET
    path: /sos/getDetails/{rideId}
    auth: TokenAuth
    request:
      pathParams:
        - name: rideId
          type: Id Ride
    response: SosDetailsRes

  - name: CreateSos
    method: POST
    path: /sos/create
    auth: TokenAuth
    request:
      body: SosReq
    response: SosRes

  - name: UpdateSosStatus
    method: POST
    path: /sos/{sosId}/status
    auth: TokenAuth
    request:
      pathParams:
        - name: sosId
          type: Id Sos
      body: UpdateSosStatusReq
    response: APISuccess

  - name: MarkRideAsSafe
    method: POST
    path: /sos/markRideAsSafe/{sosId}
    auth: TokenAuth
    request:
      pathParams:
        - name: sosId
          type: Id Sos
      body: MarkRideAsSafeReq
    response: APISuccess

  - name: CreateMockSos
    method: POST
    path: /sos/createMockSos
    auth: TokenAuth
    request:
      body: CreateMockSosReq
    response: SosRes

  - name: CallPolice
    method: POST
    path: /sos/callPolice
    auth: TokenAuth
    request:
      body: CallPoliceReq
    response: APISuccess

  - name: UpdateLocation
    method: POST
    path: /sos/{sosId}/updateLocation
    auth: TokenAuth
    request:
      pathParams:
        - name: sosId
          type: Id Sos
      body: UpdateLocationReq
    response: APISuccess

  - name: GetTracking
    method: GET
    path: /sos/{sosId}/tracking
    auth: NoAuth  # Public tracking link
    request:
      pathParams:
        - name: sosId
          type: Id Sos
    response: GetTrackingRes

  - name: StartTracking
    method: POST
    path: /sos/startTracking
    auth: TokenAuth
    request:
      body: StartTrackingReq
    response: SosRes

  - name: UpdateState
    method: POST
    path: /sos/updateState/{sosId}
    auth: TokenAuth
    request:
      pathParams:
        - name: sosId
          type: Id Sos
      body: UpdateStateReq
    response: APISuccess

  - name: GetTrackingDetails
    method: GET
    path: /sos/trackingDetails/{sosId}
    auth: TokenAuth
    request:
      pathParams:
        - name: sosId
          type: Id Sos
    response: TrackingDetailsRes

  - name: UploadMedia
    method: POST
    path: /sos/{sosId}/upload
    auth: TokenAuth
    request:
      pathParams:
        - name: sosId
          type: Id Sos
      body: SosMediaUploadReq
    response: SosMediaUploadRes

  - name: HandleIvrOutcome
    method: GET
    path: /sos/IvrOutcome
    auth: NoAuth  # Called by Exotel
    request:
      queryParams:
        - name: callSid
          type: Text
        - name: status
          type: Text
        - name: digits
          type: Maybe Text
    response: APISuccess
```

**Task 7.2**: Generate API types

```bash
# Run DSL code generation
cd Backend/lib/shared-services
stack exec api-spec-generator -- spec/Safety/RiderPlatform/API/Sos.yaml
```

This generates:
- `src-read-only/API/Types/RiderPlatform/Safety/Endpoints/Sos.hs`
- `src-read-only/API/Types/RiderPlatform/Safety/Sos.hs`

**Task 7.3**: Create provider platform API spec

**Target**: `shared-services/spec/Safety/ProviderPlatform/API/SafetyWebhook.yaml`

```yaml
endpoints:
  - name: HandleSuspectedDriversWebhook
    method: POST
    path: /safety/webhook/suspectedDrivers
    auth: ApiAuth
    request:
      body: SuspectedDriversWebhookReq
    response: APISuccess
```

---

### Step 8: Wire Up API Handlers

**Task 8.1**: Update rider-app API handler

**Target**: `rider-app/Main/src/API/UI/Sos.hs`

```haskell
module API.UI.Sos where

import qualified Domain.Action.UI.Sos as DSos
import qualified Safety.API.Types.RiderPlatform.Sos as SosAPI

type API = SosAPI.API

handler :: FlowServer API
handler =
  getSosDetails
    :<|> createSos
    :<|> updateSosStatus
    :<|> markRideAsSafe
    :<|> createMockSos
    :<|> callPolice
    :<|> updateLocation
    :<|> getTracking
    :<|> startTracking
    :<|> updateState
    :<|> getTrackingDetails
    :<|> uploadMedia
    :<|> handleIvrOutcome

getSosDetails :: Id Ride -> FlowHandler SosDetailsRes
getSosDetails rideId = withFlowHandlerAPI $ DSos.getSosDetails rideId

createSos :: SosReq -> FlowHandler SosRes
createSos req = withFlowHandlerAPI $ DSos.createSos req

-- ... other handlers
```

**Task 8.2**: Update provider-app API handler

**Target**: `dynamic-offer-driver-app/Main/src/API/UI/SafetyWebhook.hs`

```haskell
module API.UI.SafetyWebhook where

import qualified Domain.Action.UI.SafetyWebhook as DSafety
import qualified Safety.API.Types.ProviderPlatform.SafetyWebhook as SafetyAPI

type API = SafetyAPI.API

handler :: FlowServer API
handler = handleSuspectedDriversWebhook

handleSuspectedDriversWebhook :: SuspectedDriversWebhookReq -> FlowHandler APISuccess
handleSuspectedDriversWebhook req = withFlowHandlerAPI $ DSafety.handleWebhook req
```

---

### Step 9: Update Database Migrations

**Task 9.1**: Verify table compatibility

Check that shared-services Beam schemas match existing database tables:
- `sos` table
- `safety_settings` table

**Task 9.2**: Create migration if needed

If any changes to tables are required:
```sql
-- Example: Add index for performance
CREATE INDEX IF NOT EXISTS idx_sos_person_status ON atlas_app.sos(person_id, status);
CREATE INDEX IF NOT EXISTS idx_sos_merchant_city ON atlas_app.sos(merchant_operating_city_id);
```

**Task 9.3**: Update Beam table definitions

Ensure `shared-services/src/Safety/Storage/Beam/Sos.hs` has all necessary indexes and constraints.

---

### Step 10: Testing

**Task 10.1**: Unit tests for shared-services

**Target**: `shared-services/test/Safety/Domain/Action/UI/SosSpec.hs`

```haskell
module Safety.Domain.Action.UI.SosSpec where

import Test.Hspec
import Safety.Domain.Action.UI.Sos

-- Mock ServiceHandle for testing
mockServiceHandle :: ServiceHandle IO
mockServiceHandle = ServiceHandle
  { findPersonById = \_ -> pure $ Just mockPerson
  , findRideById = \_ _ -> pure $ Just mockRide
  , getSafetyConfig = \_ _ -> pure mockSosConfig
  , createTicket = \_ _ _ -> pure mockTicketResp
  -- ... mock all functions
  }

spec :: Spec
spec = do
  describe "createSos" $ do
    it "creates SOS with valid ride" $ do
      result <- createSos mockServiceHandle mockPersonId validSosReq
      result.sosId `shouldNotBe` Nothing

    it "fails with invalid ride" $ do
      createSos mockServiceHandle mockPersonId invalidRideReq
        `shouldThrow` (== RideNotFound)
```

**Task 10.2**: Integration tests for rider-app

**Target**: `rider-app/test/src/API/UI/SosSpec.hs`

Test actual API endpoints with test database:
```haskell
spec :: Spec
spec = do
  describe "POST /sos/create" $ do
    it "creates SOS for active ride" $ do
      response <- postJSON "/sos/create" validSosReq
      statusCode response `shouldBe` 200
```

**Task 10.3**: Integration tests for provider-app

**Target**: `dynamic-offer-driver-app/test/src/API/UI/SafetyWebhookSpec.hs`

Test webhook endpoint:
```haskell
spec :: Spec
spec = do
  describe "POST /safety/webhook/suspectedDrivers" $ do
    it "processes suspected driver webhook" $ do
      response <- postJSON "/safety/webhook/suspectedDrivers" webhookPayload
      statusCode response `shouldBe` 200
```

---

**Document Version**: 1.0
**Last Updated**: 2026-02-02
**Author**: Engineering Team
**Status**: Draft - Awaiting Review
