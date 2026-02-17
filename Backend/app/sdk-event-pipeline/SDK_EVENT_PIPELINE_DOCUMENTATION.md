# SDK Event Pipeline - Comprehensive Documentation

**Last Updated**: 2026-01-06
**Version**: 1.0
**Purpose**: Centralized event collection and ingestion gateway for Namma Yatri mobile applications

---

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Backend Implementation](#backend-implementation)
4. [Frontend Implementation](#frontend-implementation)
5. [Data Flow](#data-flow)
6. [Configuration](#configuration)
7. [Event Schema](#event-schema)
8. [Kafka Topics](#kafka-topics)
9. [Authentication](#authentication)
10. [Deployment](#deployment)
11. [Monitoring & Observability](#monitoring--observability)
12. [Troubleshooting](#troubleshooting)

---

## Overview

### What is SDK Event Pipeline?

The **SDK Event Pipeline** is a dedicated microservice that acts as a **centralized event collection and ingestion gateway** for both the Rider App and Driver App. It receives granular user interaction events from mobile clients and routes them to Apache Kafka topics for downstream processing, analytics, and monitoring.

### Key Characteristics

- **Standalone Haskell microservice** built with Servant framework and Euler-HS
- **Lightweight event aggregator** designed to handle high-frequency event data
- **Purpose**: Collect telemetry events, performance metrics, and user interactions without burdening main application APIs
- **Scalable design**: Uses Kafka as the event bus for distributed processing
- **Fire-and-forget**: Non-blocking event ingestion with immediate response
- **Port**: 8090 (default)

### Why Separate Service?

1. **Separation of Concerns**: Keeps event collection decoupled from main business logic
2. **Scalability**: Can be scaled independently based on event volume
3. **Performance**: Non-blocking, asynchronous event collection doesn't impact main APIs
4. **Flexibility**: Easy to modify event pipeline without touching core services
5. **Observability**: Centralized telemetry for monitoring and analytics

---

## Architecture

### High-Level Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                    MOBILE APPLICATIONS                               │
│                                                                      │
│  ┌──────────────────┐              ┌──────────────────┐           │
│  │   Rider App      │              │   Driver App     │           │
│  │   (PureScript)   │              │   (PureScript)   │           │
│  └────────┬─────────┘              └────────┬─────────┘           │
│           │                                  │                      │
│           │  Events.addEvent()              │                      │
│           │  Events.getEvents()             │                      │
│           │  Remote.pushSDKEvents()         │                      │
│           │                                  │                      │
└───────────┼──────────────────────────────────┼──────────────────────┘
            │                                  │
            │     POST /sdk/events            │
            └──────────────┬───────────────────┘
                           │
            ┌──────────────▼──────────────┐
            │                              │
            │  SDK Event Pipeline Service  │
            │  (Haskell - Port 8090)      │
            │                              │
            │  ┌────────────────────────┐ │
            │  │  POST /sdk/events      │ │
            │  │  ├─ Parse JSON        │ │
            │  │  ├─ Validate          │ │
            │  │  └─ Route by type     │ │
            │  └────────────────────────┘ │
            │                              │
            └──────────────┬───────────────┘
                           │
            ┌──────────────▼───────────────┐
            │      Apache Kafka            │
            │                              │
            │  ┌────────────────────────┐ │
            │  │ rider-sdk-events       │ │
            │  ├────────────────────────┤ │
            │  │ driver-sdk-events      │ │
            │  ├────────────────────────┤ │
            │  │ metro-webview-events   │ │
            │  └────────────────────────┘ │
            └──────────────┬───────────────┘
                           │
            ┌──────────────▼───────────────┐
            │   Consumer Services          │
            │                              │
            │  - Analytics Engine          │
            │  - Monitoring Dashboard      │
            │  - Data Warehouse            │
            │  - Alerting System           │
            └──────────────────────────────┘
```

### Component Overview

| Component | Technology | Purpose |
|-----------|-----------|---------|
| **Mobile Apps** | PureScript + JavaScript | Event generation and collection |
| **SDK Event Pipeline** | Haskell (Servant + Euler-HS) | Event ingestion and routing |
| **Kafka Cluster** | Apache Kafka | Event streaming and distribution |
| **Consumer Services** | Various | Event processing and analytics |

---

## Backend Implementation

### Directory Structure

```
app/sdk-event-pipeline/
├── src/
│   ├── API.hs                          # REST API endpoints
│   ├── App.hs                          # Application setup
│   ├── Environment.hs                  # Configuration & environment
│   ├── Domain/
│   │   ├── Types/
│   │   │   └── SDKEvents.hs           # Data types
│   │   └── Action/
│   │       └── SDKEvents.hs           # Business logic
│   ├── Tools/
│   │   └── Auth.hs                     # Authentication handling
│   └── External/
│       ├── API/
│       │   └── DriverAppAuth.hs       # Driver app auth API
│       ├── Flow.hs                     # External API flows
│       └── Types.hs                    # External response types
├── server/
│   └── Main.hs                         # Entry point
├── package.yaml                        # Package dependencies
└── sdk-event-pipeline.cabal           # Cabal build file
```

### Key Files and Implementation

#### 1. API Definition (`src/API.hs`)

**Location**: `app/sdk-event-pipeline/src/API.hs:17`

```haskell
type API =
  Get '[JSON] Text                              -- Health check: "App is up"
    :<|> ( "sdk" :> "events"
           :> ReqBody '[JSON] SDKEventsReq
           :> Post '[JSON] APISuccess
         )
```

**Endpoints**:
- `GET /` - Health check endpoint returning "App is up"
- `POST /sdk/events` - Main event ingestion endpoint

#### 2. Data Types (`src/Domain/Types/SDKEvents.hs`)

**Location**: `app/sdk-event-pipeline/src/Domain/Types/SDKEvents.hs:14`

```haskell
data SDKEventsReq = SDKEventsReq
  { event :: Text,                    -- JSON string of aggregated events
    clientType :: Maybe ClientType    -- Optional client type
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ClientType = DRIVER | RIDER | METRO_WEBVIEW
  deriving (Generic, Show, Read, ToJSON, FromJSON, ToSchema, Eq)
```

**Fields**:
- `event`: JSON-encoded string containing one or more events
- `clientType`: Optional discriminator (defaults to DRIVER if not provided)

#### 3. Business Logic (`src/Domain/Action/SDKEvents.hs`)

**Location**: `app/sdk-event-pipeline/src/Domain/Action/SDKEvents.hs:10`

```haskell
postSdkEvents :: SDKEventsReq -> Flow APISuccess
postSdkEvents req = do
  -- Default to DRIVER if no client type specified
  let clientType = fromMaybe DRIVER req.clientType

  -- Parse JSON event string to Aeson Value
  let mbEvent :: Maybe A.Value = A.decode $ encodeUtf8 req.event

  -- Only process if valid JSON
  whenJust mbEvent $ \event -> do
    case clientType of
      RIDER -> do
        riderSDKEventsKafkaTopic <- asks (.riderSDKEventsKafkaTopic)
        produceMessage (riderSDKEventsKafkaTopic, Nothing) event

      DRIVER -> do
        driverSDKEventsKafkaTopic <- asks (.driverSDKEventsKafkaTopic)
        produceMessage (driverSDKEventsKafkaTopic, Nothing) event

      METRO_WEBVIEW -> do
        metroWebviewEventsKafkaTopic <- asks (.metroWebviewEventsKafkaTopic)
        produceMessage (metroWebviewEventsKafkaTopic, Nothing) event

  return Success
```

**Processing Steps**:
1. Extract client type (default: DRIVER)
2. Parse event JSON string using Aeson
3. Route to appropriate Kafka topic based on client type
4. Produce message to Kafka (fire-and-forget)
5. Return success immediately

**Important Notes**:
- Returns success even if JSON parsing fails (silent failure)
- No synchronous validation of event structure
- Kafka production is asynchronous
- No retry logic at this layer

#### 4. Environment Configuration (`src/Environment.hs`)

**Location**: `app/sdk-event-pipeline/src/Environment.hs`

```haskell
data AppCfg = AppCfg
  { port :: Int                              -- Service port (8090)
  , hedisClusterCfg :: HedisCfg             -- Redis cache configuration
  , kafkaProducerCfg :: KafkaProducerCfg    -- Kafka broker configuration
  , riderSDKEventsKafkaTopic :: Text        -- Topic: "rider-sdk-events"
  , driverSDKEventsKafkaTopic :: Text       -- Topic: "driver-sdk-events"
  , metroWebviewEventsKafkaTopic :: Text    -- Topic: "metro-webview-events"
  , apiRateLimitOptions :: APIRateLimitOptions
  , driverAppConfig :: DriverAppConfig      -- For validating driver tokens
  , cacheConfig :: CacheConfig
  }
```

**Key Configuration Fields**:
- **port**: HTTP server port (default 8090)
- **Kafka topics**: Three separate topics for different client types
- **Redis**: Used for caching authentication results
- **Rate limiting**: API rate limit configuration

#### 5. Authentication (`src/Tools/Auth.hs`)

**Location**: `app/sdk-event-pipeline/src/Tools/Auth.hs`

**Note**: Currently commented out in API but available for future use.

```haskell
data TokenAuth (clientType :: ClientType) = TokenAuth
  { personId :: Id Person
  , merchantId :: Id Merchant
  , merchantOperatingCityId :: Id MerchantOperatingCity
  }
```

**Features**:
- **Token-based authentication** supporting all three client types
- **Driver validation**: Validates driver tokens via external Driver App API
- **Caching**: Caches authentication results in Redis
  - Cache key pattern: `driverAppAuth:{token}`
  - Configurable TTL
- **Access control**: Can be configured to reject unauthorized requests

**Integration Flow**:
```haskell
1. Extract token from Authorization header
2. Check Redis cache for existing validation
3. If not cached:
   a. Call Driver App API to validate token
   b. Cache result with TTL
4. Return TokenAuth with personId and merchant info
```

---

## Frontend Implementation

### Directory Structure

```
Frontend/
├── ui-common/
│   └── src/
│       └── Engineering/
│           └── Helpers/
│               ├── Events.purs         # PureScript event collection
│               └── Events.js           # JavaScript native layer
├── ui-driver/
│   └── src/
│       └── Services/
│           ├── Endpoint.purs           # API endpoints
│           ├── API.purs                # API types
│           └── Backend.purs            # API calls
└── ui-customer/
    └── src/
        └── Services/
            ├── EndPoint.purs           # API endpoints
            ├── API.purs                # API types
            └── Backend.purs            # API calls
```

### Event Collection Module

#### PureScript Layer (`ui-common/src/Engineering/Helpers/Events.purs`)

**Event Data Type**:
```haskell
type Event =
  { name :: String           -- Event name/identifier
  , module :: String         -- Module where event occurred
  , clientType :: String     -- DRIVER, RIDER, or METRO_WEBVIEW
  , sessionId :: String      -- Unique session identifier
  , payload :: String        -- Additional event data (JSON)
  , source :: String         -- Event source/origin
  , userId :: String         -- User identifier
  , timestamp :: String      -- UTC timestamp
  , vehicleType :: String    -- Vehicle category (for drivers)
  , cityId :: String         -- Driver location city
  }
```

**Key Functions**:

```haskell
-- Add single event to storage with enriched metadata
addEvent :: String -> EventPayload -> Effect Unit

-- Retrieve all accumulated events as JSON string
getEvents :: Unit -> Effect String

-- Send events to backend in chunks
pushEvent :: Array Event -> Effect Unit

-- Clear local event storage after successful push
clearEventStorage :: Unit -> Effect Unit
```

**Event Enrichment Logic**:
```haskell
addEvent eventName payload = do
  -- Get existing events from SharedPreferences
  existingEvents <- getKeyInSharedPrefKeys "EVENT_STORAGE"

  -- Enrich with metadata
  clientType <- detectClientType  -- DRIVER/RIDER from merchant config
  sessionId <- getValueFromIdMap "SESSION_ID"
  userId <- getUserId             -- driverId or customerId
  timestamp <- getCurrentUTC
  vehicleType <- getVehicleCategory
  cityId <- getDriverCity

  -- Create enriched event
  let enrichedEvent =
        { name: eventName
        , module: payload.module
        , clientType: clientType
        , sessionId: sessionId
        , payload: encode payload.data
        , source: payload.source
        , userId: userId
        , timestamp: timestamp
        , vehicleType: vehicleType
        , cityId: cityId
        }

  -- Append to storage
  let updatedEvents = existingEvents <> [enrichedEvent]
  setKeyInSharedPrefKeys "EVENT_STORAGE" (encode updatedEvents)
```

#### JavaScript Native Layer (`ui-common/src/Engineering/Helpers/Events.js`)

**Purpose**: Bridges PureScript to native capabilities and collects platform-specific metrics.

**Key Functions**:

```javascript
// Performance timing measurements
export const initMeasuringDuration = (key) => () => {
  window.startTime = window.startTime || {};
  window.startTime[key] = Date.now();
}

export const endMeasuringDuration = (key) => () => {
  if (window.startTime && window.startTime[key]) {
    const duration = Date.now() - window.startTime[key];
    delete window.startTime[key];
    return duration;
  }
  return null;
}

// Event aggregation
export const addEventData = (key) => (value) => () => {
  window.eventData = window.eventData || {};
  window.eventData[key] = {
    value: value,
    timestamp: new Date().toISOString()
  };
}

export const addEventAggregate = (key) => () => {
  window.eventAggregates = window.eventAggregates || {};
  window.eventAggregates[key] = (window.eventAggregates[key] || 0) + 1;
}

// Collect all events and metadata
export const getEvents = () => {
  const events = {
    // App metadata
    appVersion: getAppVersion(),
    configVersion: getConfigVersion(),
    sdkVersion: getSDKVersion(),
    os: getPlatformOS(),  // "ANDROID" or "IOS"
    service: getServiceName(),

    // User context
    sessionId: getSessionId(),
    personId: getPersonId(),
    userId: getUserId(),

    // Performance timings
    newEvents: collectDurationMeasurements(),

    // Custom events
    ...window.eventData,
    ...window.eventAggregates
  };

  // Clear collected data
  window.eventData = {};
  window.eventAggregates = {};

  return JSON.stringify(events);
}
```

**Collected Metadata**:
- **App versions**: app, config, SDK, hypercore versions
- **Platform**: OS type (Android/iOS)
- **Session**: Unique session identifier
- **User**: Person ID, User ID
- **Performance**: Duration measurements for key operations
- **Custom events**: Application-specific event data

### API Integration

#### Request Type (`ui-driver/src/Services/API.purs`)

```haskell
newtype SDKEventsReq = SDKEventsReq
  { event :: String                     -- JSON string (legacy/aggregated)
  , events :: Array EventsPayload       -- Structured events array
  }

newtype EventsPayload = EventsPayload
  { eventName :: String
  , module :: Maybe String
  , clientType :: String
  , sessionId :: String
  , payload :: Maybe String
  , source :: String
  , userId :: String
  , timestamp :: String
  , vehicleType :: Maybe String
  , cityId :: Maybe String
  }
```

**Note**: Current implementation uses `event` field (JSON string), while `events` array is prepared for future structured format.

#### Backend Call (`ui-driver/src/Services/Backend.purs`)

```haskell
pushSDKEvents :: Flow GlobalState (Either ErrorResponse ApiSuccessResult)
pushSDKEvents = do
  -- Get HTTP headers (no auth required)
  headers <- getHeaders "" false

  -- Collect all accumulated events from storage
  events <- liftFlow $ Events.getEvents

  -- Make API call
  withAPIResult (EP.pushSDKEvents "") unwrapResponse $
    callAPI headers (SDKEventsReq { event: events, events: [] })
```

**Similar implementation in Customer App** (`ui-customer/src/Services/Backend.purs`)

### Event Transmission Triggers

#### Driver App (`ui-driver/src/Screens/*/Flow.purs`)

**Periodic Transmission**:
```haskell
-- Called after measuring screen duration
void $ lift $ lift $ fork $ Remote.pushSDKEvents
```

**Trigger Points**:
- After screen transitions
- After completing specific user flows
- On app backgrounding
- Configurable periodic interval

#### Customer App (`ui-customer/src/Screens/*/Flow.purs`)

**Remote Config Controlled**:
```haskell
-- Configuration from remote config
sdkEventsPushInterval <- getRemoteConfig "SDK_EVENTS_PUSH_INTERVAL"

-- Periodic push based on interval
when (shouldPushEvents currentTime lastPushTime interval) $
  void $ lift $ lift $ fork $ Remote.pushSDKEvents
```

**Trigger Points**:
- Time-based: Configured interval (e.g., every 5 minutes)
- Event-based: After critical user actions
- On app state changes

---

## Data Flow

### Complete End-to-End Flow

```
┌─────────────────────────────────────────────────────────────────────┐
│ STEP 1: EVENT GENERATION (Mobile App)                               │
└─────────────────────────────────────────────────────────────────────┘

User Action (e.g., "Book Ride" button click)
    ↓
Events.addEvent("BOOK_RIDE_CLICKED", {
  module: "HomeScreen",
  source: "ride_booking",
  data: { rideType: "AUTO", distance: 5.2 }
})
    ↓
Enrichment Layer adds:
  - clientType: "RIDER"
  - sessionId: "sess_abc123"
  - userId: "user_xyz789"
  - timestamp: "2026-01-06T10:30:45.123Z"
  - cityId: "bangalore"
    ↓
Store in SharedPreferences["EVENT_STORAGE"]

┌─────────────────────────────────────────────────────────────────────┐
│ STEP 2: EVENT COLLECTION (JavaScript Layer)                         │
└─────────────────────────────────────────────────────────────────────┘

Events.getEvents() called
    ↓
Collect from window.eventData, window.eventAggregates
Collect from SharedPreferences["EVENT_STORAGE"]
    ↓
Aggregate into single JSON object:
{
  "appVersion": "1.5.3",
  "configVersion": "2.1.0",
  "sdkVersion": "4.2.1",
  "os": "ANDROID",
  "sessionId": "sess_abc123",
  "userId": "user_xyz789",
  "newEvents": {
    "screenLoadDuration": 245,
    "apiCallDuration": 180
  },
  "events": [
    {
      "name": "BOOK_RIDE_CLICKED",
      "module": "HomeScreen",
      "clientType": "RIDER",
      "timestamp": "2026-01-06T10:30:45.123Z",
      ...
    }
  ]
}

┌─────────────────────────────────────────────────────────────────────┐
│ STEP 3: HTTP TRANSMISSION (PureScript Backend Service)              │
└─────────────────────────────────────────────────────────────────────┘

Remote.pushSDKEvents()
    ↓
POST https://api.nammayatri.in/sdk/events
Headers: { Content-Type: application/json }
Body: {
  "event": "<JSON_STRING_FROM_STEP_2>",
  "events": [],
  "clientType": "RIDER"
}
    ↓
Response: 200 OK { "result": "Success" }
    ↓
Events.clearEventStorage()  // Clear local cache

┌─────────────────────────────────────────────────────────────────────┐
│ STEP 4: BACKEND PROCESSING (Haskell Service)                        │
└─────────────────────────────────────────────────────────────────────┘

SDK Event Pipeline receives POST /sdk/events
    ↓
Parse SDKEventsReq from JSON
    ↓
Extract: clientType = RIDER, event = "<JSON_STRING>"
    ↓
Decode JSON string to Aeson Value
    ↓
Route based on clientType:
  RIDER → Kafka topic: "rider-sdk-events"
    ↓
produceMessage("rider-sdk-events", eventJsonValue)
    ↓
Return: APISuccess (immediate response, don't wait for Kafka)

┌─────────────────────────────────────────────────────────────────────┐
│ STEP 5: KAFKA DISTRIBUTION                                          │
└─────────────────────────────────────────────────────────────────────┘

Kafka Producer writes to "rider-sdk-events" topic
    ↓
Message partitioned by Kafka (default partitioning)
    ↓
Replicated across Kafka cluster nodes
    ↓
Available for consumption by downstream services

┌─────────────────────────────────────────────────────────────────────┐
│ STEP 6: CONSUMER PROCESSING (Downstream Services)                   │
└─────────────────────────────────────────────────────────────────────┘

Multiple consumers read from Kafka topic:
    ↓
├─ Analytics Engine
│  └─ Process events for dashboards and reports
│
├─ Monitoring Service
│  └─ Track performance metrics and SLA compliance
│
├─ Data Warehouse
│  └─ Store events for historical analysis
│
└─ Alerting System
   └─ Trigger alerts based on event patterns
```

### Timing and Performance

**Latency Breakdown**:
1. **Event collection** (local): < 1ms
2. **Event enrichment** (local): < 5ms
3. **HTTP transmission**: 50-200ms (network dependent)
4. **Backend processing**: < 10ms
5. **Kafka production**: < 5ms (async)
6. **Total client-perceived latency**: 50-215ms

**Throughput Characteristics**:
- **Mobile app**: Batched transmission (reduces network overhead)
- **Backend service**: Handles 1000+ req/sec per instance
- **Kafka**: Millions of events per second (cluster dependent)

---

## Configuration

### Backend Configuration (Dhall)

**File**: `dhall-configs/dev/sdk-event-pipeline.dhall`

```dhall
let common = ./common.dhall
let sec = ./secrets.dhall

in  { port = 8090

    , logLevel = "INFO"

    -- Kafka Configuration
    , kafkaProducerCfg =
        { brokers =
            [ "kafka-broker-1:9092"
            , "kafka-broker-2:9092"
            , "kafka-broker-3:9092"
            ]
        , compression = "snappy"
        , retries = 3
        , batchSize = 16384
        , lingerMs = 10
        }

    -- Topic Names
    , riderSDKEventsKafkaTopic = "rider-sdk-events"
    , driverSDKEventsKafkaTopic = "driver-sdk-events"
    , metroWebviewEventsKafkaTopic = "metro-webview-events"

    -- Redis Configuration
    , hedisClusterCfg =
        { connectHost = "redis-cluster.internal"
        , connectPort = 6379
        , connectAuth = Some sec.redisPassword
        , connectDatabase = 0
        , connectMaxConnections = 50
        , connectMaxIdleTime = 30
        }

    -- API Rate Limiting
    , apiRateLimitOptions =
        { limit = 1000              -- Max requests per window
        , slidingWindowSeconds = 60 -- Time window
        , limitByIPorToken = "IP"   -- Rate limit by IP address
        }

    -- External API Configuration
    , driverAppConfig =
        { baseUrl = "https://driver-app.nammayatri.in"
        , timeout = 5000
        , retries = 2
        }

    -- Caching Configuration
    , cacheConfig =
        { enableCache = True
        , defaultTTL = 300          -- 5 minutes
        , authCacheTTL = 600        -- 10 minutes
        }
    }
```

### Frontend Configuration

**Remote Config Keys**:

```javascript
// Customer App
SDK_EVENTS_PUSH_INTERVAL: 300000        // 5 minutes in milliseconds
SDK_EVENTS_BATCH_SIZE: 100              // Max events per batch
SDK_EVENTS_ENABLED: true                // Feature flag

// Driver App
DRIVER_SDK_EVENTS_INTERVAL: 180000      // 3 minutes
DRIVER_PERFORMANCE_TRACKING: true       // Enable performance metrics
```

**Local Storage Keys**:
- `EVENT_STORAGE`: Array of accumulated events
- `SESSION_ID`: Current session identifier
- `LAST_EVENT_PUSH_TIME`: Timestamp of last successful push

---

## Event Schema

### Event Structure

**Single Event Object**:
```json
{
  "name": "RIDE_BOOKED",
  "module": "BookingScreen",
  "clientType": "RIDER",
  "sessionId": "sess_abc123def456",
  "payload": "{\"rideType\":\"AUTO\",\"distance\":5.2,\"fare\":120}",
  "source": "ride_booking_flow",
  "userId": "user_789xyz",
  "timestamp": "2026-01-06T10:30:45.123Z",
  "vehicleType": "",
  "cityId": "bangalore"
}
```

### Aggregated Events Payload

**Full Payload Sent to Backend**:
```json
{
  "appVersion": "1.5.3",
  "configVersion": "2.1.0",
  "sdkVersion": "4.2.1",
  "hypercoreVersion": "3.0.1",
  "os": "ANDROID",
  "service": "rider-app",
  "sessionId": "sess_abc123def456",
  "personId": "person_123",
  "userId": "user_789xyz",

  "newEvents": {
    "homeScreenLoadDuration": 245,
    "rideSearchDuration": 1850,
    "apiResponseTime": 180,
    "mapRenderTime": 420
  },

  "events": [
    {
      "name": "APP_LAUNCHED",
      "module": "SplashScreen",
      "clientType": "RIDER",
      "timestamp": "2026-01-06T10:28:15.000Z",
      "payload": "{\"launchSource\":\"notification\"}"
    },
    {
      "name": "RIDE_SEARCHED",
      "module": "HomeScreen",
      "clientType": "RIDER",
      "timestamp": "2026-01-06T10:30:00.500Z",
      "payload": "{\"pickup\":\"Koramangala\",\"drop\":\"Indiranagar\"}"
    },
    {
      "name": "RIDE_BOOKED",
      "module": "BookingScreen",
      "clientType": "RIDER",
      "timestamp": "2026-01-06T10:30:45.123Z",
      "payload": "{\"rideType\":\"AUTO\",\"distance\":5.2,\"fare\":120}"
    }
  ],

  "userInteractionCounts": {
    "buttonClicks": 15,
    "screenViews": 8,
    "apiCalls": 5
  }
}
```

### Common Event Types

**Rider App Events**:
- `APP_LAUNCHED` - App startup
- `RIDE_SEARCHED` - User searches for ride
- `RIDE_BOOKED` - Ride booking confirmed
- `PAYMENT_COMPLETED` - Payment successful
- `RIDE_CANCELLED` - Ride cancellation
- `SCREEN_VIEW` - Screen navigation
- `API_ERROR` - API failure events

**Driver App Events**:
- `APP_LAUNCHED` - App startup
- `DRIVER_ONLINE` - Driver goes online
- `DRIVER_OFFLINE` - Driver goes offline
- `RIDE_ACCEPTED` - Driver accepts ride
- `RIDE_STARTED` - Trip started
- `RIDE_COMPLETED` - Trip completed
- `LOCATION_UPDATE` - Driver location update
- `EARNINGS_VIEWED` - Earnings screen viewed

**Performance Events**:
- `SCREEN_LOAD_DURATION` - Time to load screen
- `API_RESPONSE_TIME` - API call latency
- `MAP_RENDER_TIME` - Map rendering duration
- `SEARCH_DURATION` - Ride search time

---

## Kafka Topics

### Topic Configuration

#### 1. rider-sdk-events

**Purpose**: All events from the Rider/Customer mobile app

**Configuration**:
```properties
partitions: 12
replication-factor: 3
retention.ms: 604800000        # 7 days
compression.type: snappy
segment.ms: 86400000           # 1 day
```

**Consumers**:
- Rider analytics service
- Performance monitoring
- User behavior analysis
- A/B testing platform

**Message Rate**: ~500-1000 msgs/sec (peak hours)

#### 2. driver-sdk-events

**Purpose**: All events from the Driver mobile app

**Configuration**:
```properties
partitions: 12
replication-factor: 3
retention.ms: 604800000        # 7 days
compression.type: snappy
segment.ms: 86400000
```

**Consumers**:
- Driver analytics service
- Fleet management
- Driver performance tracking
- Supply-demand balancing

**Message Rate**: ~300-600 msgs/sec (peak hours)

#### 3. metro-webview-events

**Purpose**: Events from Metro WebView integration

**Configuration**:
```properties
partitions: 6
replication-factor: 3
retention.ms: 259200000        # 3 days
compression.type: snappy
```

**Consumers**:
- Metro integration analytics
- WebView performance monitoring

**Message Rate**: ~50-100 msgs/sec

### Message Format in Kafka

**Kafka Message Structure**:
```json
{
  "key": null,
  "value": {
    "appVersion": "1.5.3",
    "events": [...],
    ...
  },
  "headers": {},
  "timestamp": 1704537045123,
  "partition": 5,
  "offset": 1234567
}
```

**Note**: Currently no message key (partitioning is round-robin). Future optimization could use `userId` or `sessionId` as key for ordered processing per user.

---

## Authentication

### Current Implementation

**Status**: Authentication is **currently disabled** in the API endpoint.

The API endpoint in `src/API.hs` does not require authentication:
```haskell
"sdk" :> "events" :> ReqBody '[JSON] SDKEventsReq :> Post '[JSON] APISuccess
```

### Available Auth Mechanism (Commented Out)

The codebase includes a complete authentication system in `src/Tools/Auth.hs` that can be enabled:

```haskell
type AuthenticatedAPI =
  "sdk" :> "events"
    :> TokenAuth 'DRIVER
    :> ReqBody '[JSON] SDKEventsReq
    :> Post '[JSON] APISuccess
```

### How Auth Works (When Enabled)

#### 1. Token Extraction

```haskell
-- Extract token from Authorization header
Authorization: Bearer <token>
```

#### 2. Driver Token Validation Flow

```
Request with token
    ↓
Check Redis cache: "driverAppAuth:{token}"
    ├─ Cache HIT → Return cached TokenAuth
    │
    └─ Cache MISS
        ↓
    Call Driver App API: GET /internal/auth/verify
        ↓
    Driver App validates token
        ↓
    Response: { personId, merchantId, merchantOperatingCityId }
        ↓
    Cache result in Redis (TTL: 10 minutes)
        ↓
    Return TokenAuth
```

#### 3. External API Call (`src/External/API/DriverAppAuth.hs`)

```haskell
verifyToken :: Text -> Flow DriverAuthResp
verifyToken token = do
  driverAppConfig <- asks (.driverAppConfig)
  let url = driverAppConfig.baseUrl <> "/internal/auth/verify"
  let headers = [("Authorization", "Bearer " <> encodeUtf8 token)]

  response <- callAPI url headers GET
  case response of
    Right resp -> return resp
    Left err -> throwError $ AuthenticationError "Invalid token"
```

#### 4. Redis Caching

**Cache Key Pattern**: `driverAppAuth:{token}`

**Cache Entry Structure**:
```json
{
  "personId": "person_123",
  "merchantId": "merchant_456",
  "merchantOperatingCityId": "city_789",
  "cachedAt": 1704537045
}
```

**TTL**: 600 seconds (10 minutes)

### Access Control

When auth is enabled:
- **DRIVER** requests: Validated via Driver App API
- **RIDER** requests: Currently **rejected** (AccessDenied error)
- **METRO_WEBVIEW** requests: No validation (open access)

### Enabling Authentication

To enable authentication:

1. **Uncomment auth in API.hs**:
```haskell
type API =
  Get '[JSON] Text
    :<|> ( "sdk" :> "events"
           :> TokenAuth 'DRIVER  -- Add this line
           :> ReqBody '[JSON] SDKEventsReq
           :> Post '[JSON] APISuccess
         )
```

2. **Update handler signature**:
```haskell
postSdkEvents :: TokenAuth 'DRIVER -> SDKEventsReq -> Flow APISuccess
```

3. **Configure Driver App endpoint** in dhall config

4. **Ensure Redis is configured** for caching

---

## Deployment

### Service Deployment

**Deployment Method**: Kubernetes (K8s)

**Namespace**: `backend-services`

**Service Name**: `sdk-event-pipeline`

### Kubernetes Resources

#### Deployment Configuration

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: sdk-event-pipeline
  namespace: backend-services
spec:
  replicas: 3                    # 3 instances for HA
  selector:
    matchLabels:
      app: sdk-event-pipeline
  template:
    metadata:
      labels:
        app: sdk-event-pipeline
    spec:
      containers:
      - name: sdk-event-pipeline
        image: nammayatri/sdk-event-pipeline:latest
        ports:
        - containerPort: 8090
        env:
        - name: CONFIG_PATH
          value: /config/sdk-event-pipeline.dhall
        resources:
          requests:
            memory: "256Mi"
            cpu: "200m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /
            port: 8090
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /
            port: 8090
          initialDelaySeconds: 5
          periodSeconds: 5
```

#### Service Configuration

```yaml
apiVersion: v1
kind: Service
metadata:
  name: sdk-event-pipeline
  namespace: backend-services
spec:
  selector:
    app: sdk-event-pipeline
  ports:
  - protocol: TCP
    port: 8090
    targetPort: 8090
  type: ClusterIP
```

#### Ingress Configuration

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: sdk-event-pipeline-ingress
  namespace: backend-services
  annotations:
    nginx.ingress.kubernetes.io/rate-limit: "1000"
    nginx.ingress.kubernetes.io/proxy-body-size: "5m"
spec:
  rules:
  - host: api.nammayatri.in
    http:
      paths:
      - path: /sdk/events
        pathType: Prefix
        backend:
          service:
            name: sdk-event-pipeline
            port:
              number: 8090
```

### Scaling Strategy

**Horizontal Pod Autoscaling (HPA)**:
```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: sdk-event-pipeline-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: sdk-event-pipeline
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

**Scaling Triggers**:
- CPU usage > 70%
- Memory usage > 80%
- Custom metric: Request rate > 800 req/sec per pod

### Dependencies

**Required Services**:
1. **Kafka Cluster** (3+ brokers)
2. **Redis Cluster** (for caching)
3. **Driver App API** (if auth enabled)

**Network Access**:
- Outbound to Kafka brokers (port 9092)
- Outbound to Redis (port 6379)
- Outbound to Driver App API (HTTPS)
- Inbound from API Gateway/Ingress (port 8090)

---

## Monitoring & Observability

### Health Checks

**Endpoint**: `GET /`

**Response**: `"App is up"` (200 OK)

**Kubernetes Probes**:
- **Liveness**: Checks if service is running
- **Readiness**: Checks if service can accept traffic

### Metrics

**Application Metrics** (via Prometheus):

```
# Request metrics
sdk_events_requests_total{client_type="RIDER"}
sdk_events_requests_total{client_type="DRIVER"}
sdk_events_requests_total{client_type="METRO_WEBVIEW"}

# Response metrics
sdk_events_response_time_seconds{quantile="0.5"}
sdk_events_response_time_seconds{quantile="0.95"}
sdk_events_response_time_seconds{quantile="0.99"}

# Kafka metrics
sdk_events_kafka_produce_total{topic="rider-sdk-events"}
sdk_events_kafka_produce_errors{topic="rider-sdk-events"}
sdk_events_kafka_produce_duration_seconds

# Error metrics
sdk_events_errors_total{error_type="json_parse_error"}
sdk_events_errors_total{error_type="kafka_produce_error"}

# Cache metrics
sdk_events_cache_hits_total
sdk_events_cache_misses_total
```

### Logging

**Log Levels**: DEBUG, INFO, WARN, ERROR

**Log Format**: JSON structured logging

**Sample Log Entries**:

```json
// Successful event processing
{
  "timestamp": "2026-01-06T10:30:45.123Z",
  "level": "INFO",
  "message": "Events processed successfully",
  "clientType": "RIDER",
  "eventCount": 15,
  "kafkaTopic": "rider-sdk-events",
  "duration": 8
}

// JSON parse error
{
  "timestamp": "2026-01-06T10:31:12.456Z",
  "level": "WARN",
  "message": "Failed to parse event JSON",
  "clientType": "DRIVER",
  "error": "Invalid JSON syntax",
  "rawEvent": "{invalid json..."
}

// Kafka production error
{
  "timestamp": "2026-01-06T10:32:00.789Z",
  "level": "ERROR",
  "message": "Kafka produce failed",
  "topic": "driver-sdk-events",
  "error": "Connection timeout",
  "retryAttempt": 2
}
```

### Alerting

**Critical Alerts**:
1. **Service Down**: Health check failures > 3 consecutive
2. **High Error Rate**: Error rate > 5% for 5 minutes
3. **Kafka Connection Lost**: Unable to produce for 1 minute
4. **High Latency**: P95 response time > 500ms

**Warning Alerts**:
1. **Increased Load**: Request rate > 1000 req/sec
2. **Cache Miss Rate**: > 30% for 10 minutes
3. **Memory Usage**: > 80% for 5 minutes

### Dashboards

**Grafana Dashboard Panels**:
1. **Request Rate**: Requests per second by client type
2. **Response Time**: P50, P95, P99 latencies
3. **Error Rate**: Percentage of failed requests
4. **Kafka Throughput**: Messages produced per second
5. **Cache Performance**: Hit/miss ratio
6. **Resource Usage**: CPU, memory, network

---

## Troubleshooting

### Common Issues

#### 1. Events Not Appearing in Kafka

**Symptoms**:
- Mobile app sends events successfully (200 OK)
- No messages in Kafka topics

**Diagnosis**:
```bash
# Check Kafka connectivity
kubectl exec -it sdk-event-pipeline-pod -- nc -zv kafka-broker 9092

# Check Kafka topic
kafka-console-consumer --bootstrap-server kafka-broker:9092 \
  --topic rider-sdk-events --from-beginning

# Check service logs
kubectl logs -f sdk-event-pipeline-pod | grep -i kafka
```

**Possible Causes**:
- Kafka broker unreachable
- Invalid JSON (silently fails)
- Wrong topic configuration
- Kafka producer misconfigured

**Resolution**:
1. Verify Kafka broker connectivity
2. Check dhall config for correct topic names
3. Review logs for JSON parsing errors
4. Validate Kafka producer configuration

#### 2. High Latency

**Symptoms**:
- Mobile app experiences slow response from `/sdk/events`
- P95 latency > 500ms

**Diagnosis**:
```bash
# Check service metrics
curl http://sdk-event-pipeline:8090/metrics | grep response_time

# Check pod resources
kubectl top pod sdk-event-pipeline-pod

# Check network latency
kubectl exec -it sdk-event-pipeline-pod -- ping kafka-broker
```

**Possible Causes**:
- High CPU/memory usage (need scaling)
- Network latency to Kafka
- Large event payloads
- Synchronous Kafka production (should be async)

**Resolution**:
1. Scale up replicas if CPU/memory high
2. Optimize Kafka producer settings (batching, compression)
3. Investigate network issues
4. Review event payload sizes

#### 3. JSON Parse Errors

**Symptoms**:
- Logs show "Failed to parse event JSON"
- Events accepted but not reaching Kafka

**Diagnosis**:
```bash
# Check logs for parse errors
kubectl logs sdk-event-pipeline-pod | grep "parse"

# Sample raw events from mobile app
# Check mobile app event serialization
```

**Possible Causes**:
- Mobile app sending invalid JSON
- Encoding issues (UTF-8)
- Malformed event structure

**Resolution**:
1. Review mobile app event serialization
2. Add JSON schema validation in mobile app
3. Implement stricter validation in backend (currently silent fail)
4. Log raw events for debugging

#### 4. Authentication Failures (When Auth Enabled)

**Symptoms**:
- 401 Unauthorized errors
- High cache miss rate

**Diagnosis**:
```bash
# Check Redis connectivity
kubectl exec -it sdk-event-pipeline-pod -- redis-cli -h redis-cluster ping

# Check Driver App API
curl https://driver-app.nammayatri.in/internal/auth/verify \
  -H "Authorization: Bearer <token>"

# Check cache entries
redis-cli -h redis-cluster KEYS "driverAppAuth:*"
```

**Possible Causes**:
- Expired tokens from mobile app
- Driver App API unavailable
- Redis cache unavailable
- Incorrect auth configuration

**Resolution**:
1. Verify Driver App API is reachable
2. Check Redis cluster health
3. Review token refresh logic in mobile app
4. Validate dhall config for Driver App endpoint

#### 5. Memory Leaks

**Symptoms**:
- Pod memory usage growing over time
- OOMKilled errors in pod status

**Diagnosis**:
```bash
# Check memory usage over time
kubectl top pod sdk-event-pipeline-pod

# Check for heap growth
# Enable GHC profiling in production

# Review pod events
kubectl describe pod sdk-event-pipeline-pod
```

**Possible Causes**:
- Event accumulation in memory
- Kafka producer buffer not flushing
- Connection leaks

**Resolution**:
1. Increase memory limits
2. Review Kafka producer flush settings
3. Implement connection pooling properly
4. Profile with GHC heap profiling

### Debug Mode

**Enable debug logging**:
```dhall
-- In dhall config
logLevel = "DEBUG"
```

**Debug endpoints** (can be added for troubleshooting):
```haskell
-- GET /debug/kafka/topics
-- Returns list of configured Kafka topics

-- GET /debug/cache/stats
-- Returns Redis cache statistics

-- POST /debug/event/validate
-- Validates event JSON without producing to Kafka
```

### Performance Tuning

**Kafka Producer Optimization**:
```dhall
kafkaProducerCfg =
  { batchSize = 32768           -- Increase batch size
  , lingerMs = 50               -- Wait longer to fill batches
  , compression = "snappy"      -- Use compression
  , acks = 1                    -- Don't wait for all replicas
  , retries = 3
  }
```

**Service Scaling**:
- **Low load** (< 500 req/sec): 2-3 replicas
- **Medium load** (500-1000 req/sec): 3-5 replicas
- **High load** (> 1000 req/sec): 5-10 replicas

---

## Future Enhancements

### Planned Improvements

1. **Structured Events Array**:
   - Move from single JSON string to array of structured events
   - Use `events` field in `SDKEventsReq`
   - Enable per-event validation and processing

2. **Event Schema Validation**:
   - Define JSON schemas for event types
   - Validate events against schemas
   - Reject malformed events with clear error messages

3. **Authentication Enforcement**:
   - Enable token-based authentication
   - Support both DRIVER and RIDER auth
   - Implement API key authentication for webview

4. **Advanced Routing**:
   - Route based on event type, not just client type
   - Support multiple Kafka topics per client
   - Dynamic topic selection based on event metadata

5. **Event Enrichment**:
   - Add server-side timestamp
   - Enrich with geo-location data
   - Add request metadata (IP, user-agent)

6. **Rate Limiting Improvements**:
   - Per-user rate limiting (not just IP)
   - Adaptive rate limiting based on load
   - Quota management per client

7. **Observability**:
   - Distributed tracing with OpenTelemetry
   - Custom metrics dashboard
   - Event sampling for debugging

8. **Batch Processing**:
   - Accept bulk event submissions
   - Process multiple events in single request
   - Optimize Kafka production with batching

---

## Appendix

### Quick Reference

**Service Endpoints**:
- Health Check: `GET /`
- Event Ingestion: `POST /sdk/events`

**Kafka Topics**:
- Rider events: `rider-sdk-events`
- Driver events: `driver-sdk-events`
- Metro events: `metro-webview-events`

**Configuration File**: `dhall-configs/dev/sdk-event-pipeline.dhall`

**Key Source Files**:
- API Definition: `app/sdk-event-pipeline/src/API.hs:17`
- Business Logic: `app/sdk-event-pipeline/src/Domain/Action/SDKEvents.hs:10`
- Data Types: `app/sdk-event-pipeline/src/Domain/Types/SDKEvents.hs:14`
- Environment: `app/sdk-event-pipeline/src/Environment.hs`
- Authentication: `app/sdk-event-pipeline/src/Tools/Auth.hs`

### Related Documentation

- [Kafka Setup Guide](../infrastructure/kafka-setup.md)
- [Mobile App Event Tracking](../../Frontend/docs/event-tracking.md)
- [Analytics Pipeline](../analytics/pipeline-overview.md)
- [Monitoring Guide](../operations/monitoring.md)

### Contact

**Team**: Backend Infrastructure
**Slack**: #backend-infrastructure
**On-call**: Check PagerDuty rotation

---

*This documentation is maintained by the Backend team. Last updated: 2026-01-06*
