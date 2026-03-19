# NammaYatri Backend — Haskell Developer Onboarding Guide

This guide walks you through every major pattern in the NammaYatri Haskell backend with real code from the repository. Read it top to bottom before writing your first feature.

---

## 1. Service Pattern — Anatomy of a Microservice

Every microservice follows a 5-layer startup pipeline: **Main → App → Environment → Server → API handlers**.

### 1.1 Entry Point (Main.hs)

The thinnest possible layer — it just calls `runRiderApp`:

```haskell
-- app/rider-platform/rider-app/Main/server/Main.hs:15-22
module Main where

import App (runRiderApp)

main :: IO ()
main = runRiderApp id
```

The `id` argument is a config modifier `(AppCfg -> AppCfg)` — tests pass a different modifier to override ports/DB config.

### 1.2 Application Bootstrap (App.hs)

`App.hs` orchestrates the full startup sequence:

```haskell
-- app/rider-platform/rider-app/Main/src/App.hs:87-91
runRiderApp :: (AppCfg -> AppCfg) -> IO ()
runRiderApp configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "rider-app"  -- Step 1: Load Dhall config
  Metrics.serve (appCfg.metricsPort)                                -- Step 2: Start Prometheus metrics server
  runRiderApp' appCfg                                               -- Step 3: Boot the actual service
```

The core bootstrap in `runRiderApp'`:

```haskell
-- app/rider-platform/rider-app/Main/src/App.hs:93-154
runRiderApp' :: AppCfg -> IO ()
runRiderApp' appCfg = do
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig

  -- 1. Build runtime environment (connects to PG, Redis, Kafka, ClickHouse)
  appEnv <-
    try (buildAppEnv appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "

  -- 2. Configure Warp with graceful shutdown
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appCfg.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
          & setPort (appCfg.port)

  -- 3. Create FlowRuntime, prepare connections, run DB migration
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    runFlow flowRt (prepareConnectionRider (...) >> L.setOption KafkaConn appEnv.kafkaProducerTools)
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        kvConfigs <- findById "kv_configs" >>= pure . decodeFromText' @Tables
          >>= fromMaybeM (InternalError "Couldn't find kv_configs table")
        L.setOption KBT.Tables kvConfigs
        initCityMaps
        allBaps <- try QMerchant.loadAllBaps
          >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        -- ... auth manager setup ...
        pure flowRt'

    -- 4. Attach middleware and start Warp
    let timeoutMiddleware = UE.timeoutEvent flowRt appEnv (responseLBS status408 [] "") appCfg.incomingAPIResponseTimeout
    runSettings settings $ timeoutMiddleware (App.run (App.EnvR flowRt' appEnv))
```

### 1.3 Environment Types (Environment.hs)

Two key structs: `AppCfg` (static config from Dhall) and `AppEnv` (live runtime with connections):

```haskell
-- app/rider-platform/rider-app/Main/src/Environment.hs:100-201
data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,             -- Primary Postgres
    esqDBReplicaCfg :: EsqDBConfig,       -- Read replica
    hedisCfg :: HedisCfg,                 -- Redis single instance
    hedisClusterCfg :: HedisCfg,          -- Redis cluster
    hedisNonCriticalCfg :: HedisCfg,      -- Non-critical Redis (failures don't crash)
    kafkaProducerCfg :: KafkaProducerCfg, -- Kafka producer
    port :: Int,                          -- HTTP port (8013 for rider-app)
    graceTerminationPeriod :: Seconds,    -- Shutdown grace period
    authTokenCacheExpiry :: Seconds,      -- Auth token TTL in Redis
    incomingAPIResponseTimeout :: Int,    -- Request timeout (ms)
    -- ... ~80 more fields ...
  }
  deriving (Generic, FromDhall)

-- app/rider-platform/rider-app/Main/src/Environment.hs:204-317
data AppEnv = AppEnv
  { esqDBEnv :: EsqDBEnv,                -- Live DB connection pool
    hedisEnv :: HedisEnv,                 -- Live Redis connection
    hedisClusterEnv :: HedisEnv,          -- Live Redis cluster connection
    kafkaProducerTools :: KafkaProducerTools,
    isShuttingDown :: TMVar (),           -- Signal for graceful shutdown
    bapMetrics :: BAPMetricsContainer,    -- Prometheus counters
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    -- ... ~80 more fields ...
  }
  deriving (Generic)
```

`buildAppEnv` connects everything:

```haskell
-- app/rider-platform/rider-app/Main/src/Environment.hs:330-379
buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv cfg@AppCfg {..} = do
  hostname <- getPodName
  psqlConn <- PG.connect (toConnectInfo esqDBCfg)     -- Connect to Postgres
  isShuttingDown <- newEmptyTMVarIO                     -- Empty TMVar = running
  bapMetrics <- registerBAPMetricsContainer metricsSearchDurationTimeout
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg secondaryKafkaProducerCfg
  hedisEnv <- connectHedis hedisCfg riderAppPrefix      -- "rider-app:" prefix
  hedisClusterEnv <- connectHedisCluster hedisClusterCfg riderAppPrefix
  serviceClickhouseEnv <- createConn riderClickhouseCfg -- ClickHouse for analytics
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv
  disconnectHedis hedisClusterEnv
```

### 1.4 Server & Middleware Stack (App/Server.hs)

The middleware pipeline is composed right-to-left via `&`:

```haskell
-- app/rider-platform/rider-app/Main/src/App/Server.hs:29-43
run :: Env -> Application
run = withModifiedEnv' $ \modifiedEnv ->
  BU.run appAPI API.handler context modifiedEnv   -- Core Servant handler
    & logRequestAndResponse' modifiedEnv           -- Log every req/res
    & addServantInfo modifiedEnv.appEnv.version appAPI  -- X-Servant-Info header
    & hashBodyForSignature                         -- BECKN signature support
    & supportProxyAuthorization                    -- Proxy auth passthrough
  where
    appAPI = Proxy @API.API
    context =
      verifyPersonAction @(FlowR AppEnv)           -- TokenAuth handler
        :. verifyDashboardAction @(FlowR AppEnv)   -- DashboardTokenAuth handler
        :. verifyPartnerOrganizationAction @(FlowR AppEnv)
        :. EmptyContext
```

### 1.5 Type Aliases

Every service defines these four type aliases for convenience:

```haskell
-- app/rider-platform/rider-app/Main/src/Environment.hs:389-395
type Env = EnvR AppEnv                    -- Full runtime env (FlowRuntime + AppEnv)
type FlowHandler = FlowHandlerR AppEnv    -- Servant handler monad
type FlowServer api = FlowServerR AppEnv api  -- Servant server type
type Flow = FlowR AppEnv                  -- Business logic monad
```

---

## 2. API Definition Pattern — From YAML to Servant

APIs are defined in YAML, code-generated into Servant types, then manually implemented.

### 2.1 YAML Spec (Source of Truth)

```yaml
# app/rider-platform/rider-app/Main/spec/API/FollowRide.yaml
imports:
  Ride: Domain.Types.Ride

module: FollowRide

types:
  Followers:
    bookingId: Id Booking
    name: Text
    priority: Int
  ShareRideReq:
    emergencyContactNumbers: "[Text]"
  EmergencyContactsStatusRes:
    contacts: "[EmergencyContactStatus]"

apis:
  - GET:
      endpoint: /follow/ride
      auth: TokenAuth
      response:
        type: "[Followers]"

  - POST:
      endpoint: /share/ride
      auth: TokenAuth
      request:
        type: ShareRideReq
      response:
        type: API.Types.UI.APISuccess

  - GET:
      endpoint: /followRide/ECStatus/{rideId}
      auth: TokenAuth
      params:
        rideId: Id Ride
      response:
        type: EmergencyContactsStatusRes
```

### 2.2 Generated Servant API Types

The generator produces type-level routes with `:<|>` composition:

```haskell
-- src-read-only/API/Action/UI/FollowRide.hs (GENERATED — do not edit)
type API =
  ( TokenAuth :> "follow" :> "ride" :> Get '[JSON] [API.Types.UI.FollowRide.Followers]
    :<|> TokenAuth :> "share" :> "ride"
      :> ReqBody '[JSON] API.Types.UI.FollowRide.ShareRideReq
      :> Post '[JSON] Kernel.Types.APISuccess.APISuccess
    :<|> TokenAuth :> "followRide" :> "ECStatus"
      :> Capture "rideId" (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
      :> Get '[JSON] API.Types.UI.FollowRide.EmergencyContactsStatusRes
  )

handler :: Environment.FlowServer API
handler = getFollowRide :<|> postShareRide :<|> getFollowRideECStatus
```

### 2.3 Five Auth Patterns

| YAML Auth | Servant Type | Result Type |
|---|---|---|
| `TokenAuth` (rider) | `HeaderAuth "token" VerifyToken` | `(Id Person, Id Merchant)` |
| `TokenAuth` (driver) | `HeaderAuth "token" VerifyToken` | `(Id Person, Id Merchant, Id MerchantOperatingCity)` |
| `DashboardAuth` | `HeaderAuth "token" DashboardVerifyToken` | `Dashboard` |
| `PartnerOrganizationAPIKey` | `HeaderAuth "x-api-key" PartnerOrganizationVerifyAPIKey` | `PartnerOrganization` |
| `NoAuth` | (no wrapper) | No auth tuple in handler |

Auth is verified in `Tools/Auth.hs`:

```haskell
-- app/rider-platform/rider-app/Main/src/Tools/Auth.hs:47-83
type TokenAuth = HeaderAuth "token" VerifyToken

instance VerificationMethod VerifyToken where
  type VerificationResult VerifyToken = (Id Person.Person, Id Merchant.Merchant)

verifyPerson :: (...) => RegToken -> m (Id Person.Person, Id Merchant.Merchant)
verifyPerson token = do
  let key = authTokenCacheKey token  -- "rider-platform:authTokenCacheKey:{token}"
  authTokenCacheExpiry <- getSeconds <$> asks (.authTokenCacheExpiry)
  result <- Redis.safeGet key
  case result of
    Just (personId, merchantId) -> return (personId, merchantId)  -- Cache hit
    Nothing -> do
      sr <- verifyToken token                        -- DB lookup
      Redis.setExp key (personId, merchantId) expiryTime  -- Cache for next time
      return (personId, merchantId)
```

### 2.4 Servant Combinator Cheat Sheet

```haskell
-- Path segments:                 "ride" :> "status"           → /ride/status
-- Path parameter:                Capture "rideId" (Id Ride)   → /ride/{rideId}
-- Mandatory query param:         MandatoryQueryParam "from" UTCTime  → ?from=...
-- Optional query param:          QueryParam "limit" Int        → ?limit=... (Maybe Int in handler)
-- Request body:                  ReqBody '[JSON] SearchReq     → JSON POST body
-- Response:                      Get '[JSON] SearchRes         → JSON GET response
-- Multiple endpoints:            API1 :<|> API2 :<|> API3
-- Prefix group:                  "v2" :> (API1 :<|> API2)     → /v2/...
```

### 2.5 Top-Level API Composition

```haskell
-- app/rider-platform/rider-app/Main/src/API/UI.hs
type API =
  "v2"
    :> ( Get '[JSON] Text           -- Health check
           :<|> Registration.API
           :<|> Profile.API
           :<|> Search.API
           :<|> Booking.API
           :<|> Ride.API
           :<|> Cancel.API
           :<|> FollowRide.API
           -- ... 40+ more modules ...
       )

handler :: FlowServer API
handler =
  pure "App is UP"
    :<|> Registration.handler
    :<|> Profile.handler
    :<|> Search.handler
    -- ... matched 1:1 with :<|> ...
```

### 2.6 Manual Handler (Business Logic)

Generated stubs delegate to `Domain.Action.UI.*` where you write logic:

```haskell
-- app/rider-platform/rider-app/Main/src/Domain/Action/UI/FollowRide.hs
getFollowRide ::
  (Maybe (Id Person), Id Merchant) ->
  Flow [Followers]
getFollowRide (mbPersonId, _merchantId) = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "")
  -- Your business logic here
  emergencyContacts <- QEmergencyContact.findAllByPersonId personId
  pure $ map toFollower emergencyContacts
```

---

## 3. Domain Type Pattern — Phantom Types, Encryption, and Safety

### 3.1 Simple Domain Type (Location)

```haskell
-- src-read-only/Domain/Types/Location.hs
data Location = Location
  { id :: Id Location,
    lat :: Double,
    lon :: Double,
    address :: LocationAddress,
    merchantId :: Maybe (Id Merchant),
    merchantOperatingCityId :: Maybe (Id MerchantOperatingCity),
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, HasCoordinates, ToJSON, FromJSON, ToSchema)
```

### 3.2 Phantom Type Safety (Merchant)

`UsageSafety` prevents using half-constructed types:

```haskell
-- src-read-only/Domain/Types/Merchant.hs
data MerchantD (s :: UsageSafety) = Merchant
  { id :: Id Merchant,
    shortId :: ShortId Merchant,
    bapId :: Text,
    subscriberId :: Text,
    gatewayUrl :: BaseUrl,
    signingPublicKey :: Base64,
    -- ...
  }

type Merchant = MerchantD 'Safe        -- Fully constructed, safe to use
type UnsafeMerchant = MerchantD 'Unsafe -- Intermediate construction
```

### 3.3 Encrypted Fields (Person)

Sensitive fields use `EncryptedHashedField`:

```haskell
-- src-read-only/Domain/Types/Person.hs
data PersonE (e :: EncryptionStatus) = Person
  { id :: Id Person,
    firstName :: Maybe Text,
    mobileNumber :: Maybe (EncryptedHashedField e Text),  -- Encrypted!
    email :: Maybe (EncryptedHashedField e Text),          -- Encrypted!
    blocked :: Bool,
    enabled :: Bool,
    aadhaarVerified :: Bool,
    totalRatings :: Int,
    totalRatingScore :: Int,
    -- ...
  }

type Person = PersonE 'AsEncrypted           -- Stored encrypted
type DecryptedPerson = PersonE 'AsUnencrypted -- Decrypted for use
```

### 3.4 Sum Type / Enum (BookingDetails)

YAML spec for algebraic data types:

```yaml
# spec/Storage/Booking.yaml
types:
  BookingDetails:
    enum: OneWayDetails OneWayBookingDetails,
          RentalDetails RentalBookingDetails,
          DriverOfferDetails OneWayBookingDetails,
          InterCityDetails InterCityBookingDetails,
          AmbulanceDetails AmbulanceBookingDetails,
          DeliveryDetails DeliveryBookingDetails
```

Generates:

```haskell
data BookingDetails
  = OneWayDetails OneWayBookingDetails
  | RentalDetails RentalBookingDetails
  | DriverOfferDetails OneWayBookingDetails
  | InterCityDetails InterCityBookingDetails
  | AmbulanceDetails AmbulanceBookingDetails
  | DeliveryDetails DeliveryBookingDetails
  deriving (Show, Generic, ToJSON, FromJSON)
```

### 3.5 Complex Nested Type (Ride)

```haskell
-- src-read-only/Domain/Types/Ride.hs
data Ride = Ride
  { id :: Id Ride,
    shortId :: ShortId Ride,
    bppRideId :: Id BPPRide,
    bookingId :: Id Booking,
    status :: RideStatus,
    fromLocation :: Location,
    toLocation :: Maybe Location,
    driverName :: Text,
    driverMobileNumber :: Text,
    fare :: Maybe Price,
    totalFare :: Maybe Price,
    chargeableDistance :: Maybe Distance,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    paymentStatus :: Maybe PaymentStatus,
    -- ... 50+ fields
  }
```

---

## 4. Storage Pattern — Beam, Queries, and Caching

### 4.1 Beam Table Definition

Beam tables mirror domain types with `f` phantom parameter:

```haskell
-- src-read-only/Storage/Beam/Location.hs
data LocationT f = LocationT
  { id :: B.C f Text,              -- IDs stored as Text
    lat :: B.C f Double,
    lon :: B.C f Double,
    area :: B.C f (Maybe Text),    -- Flattened from LocationAddress
    city :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime,
    merchantId :: B.C f (Maybe Text),
  }
  deriving (Generic, B.Beamable)

instance B.Table LocationT where
  data PrimaryKey LocationT f = LocationId (B.C f Text)
  primaryKey = LocationId . id

$(enableKVPG ''LocationT ['id] [])                -- KV + PG dual storage
$(mkTableInstances ''LocationT "location")         -- Table name mapping
```

### 4.2 Domain ↔ Beam Conversion (FromTType/ToTType)

```haskell
-- src-read-only/Storage/Queries/RentalDetails.hs
instance FromTType' Beam.RentalDetails Domain.Types.RentalDetails.RentalDetails where
  fromTType' (Beam.RentalDetailsT {..}) = do
    pure $ Just RentalDetails
      { baseFare = mkPriceWithDefault baseFareAmount currency baseFare,
        id = Id id,
        includedDistancePerHr = mkDistanceWithDefaultMeters distanceUnit
          includedDistancePerHrValue (kilometersToMeters includedKmPerHr),
        nightShiftInfo = mkNightShiftInfo nightShiftCharge nightShiftChargeAmount
          nightShiftEnd nightShiftStart currency,
        perHourCharge = mkPriceWithDefault perHourChargeAmount currency perHourCharge
      }

instance ToTType' Beam.RentalDetails Domain.Types.RentalDetails.RentalDetails where
  toTType' (RentalDetails {..}) = do
    Beam.RentalDetailsT
      { Beam.baseFare = (.amountInt) baseFare,
        Beam.baseFareAmount = Just $ (.amount) baseFare,
        Beam.currency = Just $ (.currency) baseFare,
        Beam.id = getId id,
        -- Price splits into amount + amountInt + currency columns
      }
```

**Key conversion helpers:**
- `mkPriceWithDefault` — Reconstructs `Price` from separate amount/currency columns
- `mkDistanceWithDefaultMeters` — Reconstructs `Distance` with unit conversion
- `getId` / `Id` — Unwrap/wrap `Id` phantom type to/from `Text`

### 4.3 Query Patterns (10+ Examples)

All queries use three standard constraints:

```haskell
type QueryConstraints m r = (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
```

**CREATE:**

```haskell
-- src-read-only/Storage/Queries/Person.hs
create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Person -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Person] -> m ()
createMany = traverse_ create
```

**FIND BY ID (single result):**

```haskell
findById :: (...) => Id Person -> m (Maybe Person)
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (getId id)]
```

**FIND BY FIELD (multiple results):**

```haskell
findAllByDeviceId :: (...) => Maybe Text -> m [Person]
findAllByDeviceId deviceId = do findAllWithKV [Se.Is Beam.deviceId $ Se.Eq deviceId]
```

**FIND WITH AND (compound conditions):**

```haskell
findBySRIdAndStatus :: (...) => EstimateStatus -> Id SearchRequest -> m (Maybe Estimate)
findBySRIdAndStatus status requestId = do
  findOneWithKV [Se.And [ Se.Is Beam.status $ Se.Eq status
                        , Se.Is Beam.requestId $ Se.Eq (getId requestId)]]
```

**UPDATE (always sets updatedAt):**

```haskell
updateAverageRating :: (...) => Int -> Int -> Bool -> Id Person -> m ()
updateAverageRating totalRatings totalRatingScore isValidRating id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.totalRatings totalRatings,
      Se.Set Beam.totalRatingScore totalRatingScore,
      Se.Set Beam.isValidRating isValidRating,
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (getId id)]
```

**UPDATE with Maybe fields:**

```haskell
updateEstimatedEndTimeRange :: (...) => Maybe EstimatedEndTimeRange -> Id Ride -> m ()
updateEstimatedEndTimeRange estimatedEndTimeRange id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.estimatedEndTimeRangeEnd (fmap (.end) estimatedEndTimeRange),
      Se.Set Beam.estimatedEndTimeRangeStart (fmap (.start) estimatedEndTimeRange),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (getId id)]
```

**DELETE:**

```haskell
deleteById :: (...) => Id Person -> m ()
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (getId id)]
```

**UPDATE BY PRIMARY KEY (full entity):**

```haskell
updateByPrimaryKey :: (...) => Estimate -> m ()
updateByPrimaryKey (Estimate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppEstimateId (getId bppEstimateId),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now,
      -- ... all fields ...
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (getId id)]]
```

**FIND WITH CONDITIONAL DB (KV + PG dual read):**

```haskell
findAllBySRId :: (...) => Id SearchRequest -> m [Estimate]
findAllBySRId requestId = do
  findAllWithKVAndConditionalDB [Se.Is Beam.requestId $ Se.Eq (getId requestId)] Nothing
```

### 4.4 Sequelize Filter DSL

```haskell
Se.Is Beam.field $ Se.Eq value        -- field = value
Se.Is Beam.field $ Se.In [v1, v2]     -- field IN (v1, v2)
Se.Is Beam.field $ Se.GreaterThan val  -- field > value
Se.And [cond1, cond2]                  -- cond1 AND cond2
Se.Or [cond1, cond2]                   -- cond1 OR cond2
Se.Set Beam.field newValue             -- SET field = newValue
```

### 4.5 Cached Queries (Redis Layer)

```haskell
-- src-read-only/Storage/CachedQueries/FeedbackForm.hs
findAllFeedback :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => m [FeedbackForm]
findAllFeedback = do
  (Hedis.safeGet $ "CachedQueries:FeedbackForm")        -- Try Redis first
    >>= ( \case
            Just a -> pure a                              -- Cache hit
            Nothing ->
              (\dataToBeCached -> do
                expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                Hedis.setExp "CachedQueries:FeedbackForm" dataToBeCached expTime
              ) /=<< Queries.findAllFeedback              -- Cache miss → DB → store in Redis
        )
```

**Key naming pattern:** `"CachedQueries:{Type}:{Field1}-{value1}:{Field2}-{value2}"`

Example with parameters:

```haskell
findAllFeedbackByMerchantOpCityIdAndRating :: (...) => Id MerchantOperatingCity -> Int -> m [FeedbackForm]
findAllFeedbackByMerchantOpCityIdAndRating merchantOperatingCityId rating = do
  (Hedis.safeGet $ "CachedQueries:FeedbackForm:" <>
    ":MerchantOperatingCityId-" <> getId merchantOperatingCityId <>
    ":Rating-" <> show rating)
    >>= ( \case
            Just a -> pure a
            Nothing -> (\dataToBeCached -> do
                expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                Hedis.setExp (...) dataToBeCached expTime
              ) /=<< Queries.findAllFeedbackByMerchantOpCityIdAndRating ...)
```

### 4.6 Transaction Handling

Single creates do **not** need transaction wrappers:

```haskell
-- ✅ Correct: single create
QRide.create ride

-- ✅ Correct: multiple operations in transaction
runTransaction $ do
  QGI.deleteAll specialLocationId
  QSLG.create newSpecialLocation
```

---

## 5. External API Pattern — HTTP Clients and BECKN Protocol

### 5.1 Servant Client Generation

External APIs are defined as Servant types, and clients are generated with `ET.client`:

```haskell
-- ExternalBPP/ExternalAPI/Bus/EBIX/Auth.hs:44-66
type AuthAPI =
  "token"
    :> ReqBody '[FormUrlEncoded] AuthReq
    :> Get '[JSON] AuthRes

authAPI :: Proxy AuthAPI
authAPI = Proxy

getAuthToken :: (...) => EBIXConfig -> m Text
getAuthToken config = do
  authToken <- Hedis.get authTokenKey           -- Check cache first
  case authToken of
    Nothing -> do
      password <- decrypt config.password       -- Decrypt from Passetto
      auth <- callAPI config.networkHostUrl
        (ET.client authAPI $ AuthReq config.username password) "authEBIX" authAPI
        >>= fromEitherM (ExternalAPICallError (Just "EBIX_AUTH_API") config.networkHostUrl)
      Hedis.setExp authTokenKey auth.accessToken auth.expiresIn.getSeconds
      return auth.accessToken
    Just token -> return token
```

### 5.2 BECKN Protocol Calls

```haskell
-- SharedLogic/CallBPP.hs:54-75
searchV2 :: (...) => BaseUrl -> API.SearchReqV2 -> Id Merchant -> m API.SearchRes
searchV2 gatewayUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.searchReqContext.contextBapId
    & fromMaybeM (InvalidRequest "BapId is missing")
  -- Calls external BPP with BECKN signature authentication
  res <- callBecknAPIWithSignature' merchantId bapId "search"
    API.searchAPIV2 gatewayUrl internalEndPointHashMap req
  -- Log to Kafka asynchronously
  fork ("Logging Internal API Call") $ do
    ApiCallLogger.pushInternalApiCallDataToKafka "searchV2" "BAP" transactionId (Just req) res
  pure res
```

### 5.3 Retry with Distributed Locking

```haskell
-- ExternalBPP/ExternalAPI/Subway/CRIS/Auth.hs:94-150
callCRISAPI :: (...) => CRISConfig -> Proxy api -> (Text -> ET.EulerClient res) -> Text -> m res
callCRISAPI config proxy clientFn description = do
  token <- getAuthToken config
  eitherResp <- withTryCatch "CRIS:auth" $
    callApiUnwrappingApiError (identity @CRISErrorUnhandled)
      (Just $ ET.ManagerSelector $ T.pack crisHttpManagerKey)
      Nothing Nothing config.baseUrl (clientFn token) description proxy
  case eitherResp of
    Left err ->
      if is401Error err
        then do
          freshToken <- resetAuthToken config    -- Token refresh with Redis lock
          -- Retry once with fresh token
          eitherRetryResp <- withTryCatch "callCRISAPI:retry" $
            callApiUnwrappingApiError (...) config.baseUrl (clientFn freshToken) description proxy
          case eitherRetryResp of
            Left retryErr -> throwError $ CRISErrorUnhandled "Auth failed after retry"
            Right res -> return res
        else throwError $ CRISErrorUnhandled $ "Error: " <> show err
    Right res -> return res
```

Token refresh uses Redis distributed lock to prevent thundering herd:

```haskell
resetAuthToken :: (...) => CRISConfig -> m Text
resetAuthToken config = do
  lockAcquired <- Hedis.tryLockRedis getCRISTokenRefreshLockKey 30  -- 30s lock
  if lockAcquired
    then do
      tokenRes <- (callAPI config.baseUrl (ET.client authAPI basicAuthData ...) "authCRIS" authAPI
        >>= fromEitherM (ExternalAPICallError (Just "CRIS_AUTH_API") config.baseUrl))
        `finally` Hedis.unlockRedis getCRISTokenRefreshLockKey
      Hedis.setExp getCRISTokenKey (tokenRes.access_token) (tokenRes.expires_in * 90 `div` 100)
      return tokenRes.access_token
    else do
      threadDelay 3000000  -- Another pod is refreshing, wait 3s
      Hedis.get getCRISTokenKey >>= fromMaybeM (CRISErrorUnhandled "No token after waiting")
```

### 5.4 BECKN ACL (Anti-Corruption Layer)

ACL modules translate between domain types and BECKN protocol types:

```haskell
-- Beckn/ACL/Search.hs:32-44
buildSearchReqV2 :: (...) => SLS.SearchRes -> m Spec.SearchReq
buildSearchReqV2 res@SLS.SearchRes {..} = do
  bapUri <- Utils.mkBapUri merchant.id
  bapConfig <- listToMaybe bapConfigs
    & fromMaybeM (InternalError "Beckn Config not found")
  messageId <- generateGUIDText
  let eBecknSearchReq = Search.buildBecknSearchReqV2 res bapConfig bapUri messageId
  case eBecknSearchReq of
    Left err -> throwError $ InternalError err
    Right becknSearchReq -> pure becknSearchReq
```

---

## 6. Kafka Pattern — Producers and Consumers

### 6.1 Producer Setup

Producers are configured in Dhall and built during `buildAppEnv`:

```haskell
-- Environment.hs:344
kafkaProducerTools <- buildKafkaProducerTools kafkaProducerCfg secondaryKafkaProducerCfg
```

Message production:

```haskell
-- lib/sessionizer-metrics/src/Lib/SessionizerMetrics/Kafka/Internal.hs:25-42
streamUpdates :: (..., ToJSON p) => Event p -> KafkaConfig -> m ()
streamUpdates event kcfg = do
  let topicName = kcfg.topicName
  let key = kcfg.kafkaKey
  produceMessage (topicName, Just (encodeUtf8 key)) event
```

### 6.2 Event Stream Routing

Events are routed to Kafka/Prometheus based on config:

```haskell
-- lib/sessionizer-metrics/src/Lib/SessionizerMetrics/EventStream.hs:23-69
triggerEvent :: (..., ToJSON p) => Event p -> m ()
triggerEvent event = do
  allEventStream <- asks (.eventStreamMap)
  let streamNames = filter (elem (eventType event) . eventTypes) allEventStream
  forM_ streamNames $ \stream ->
    case streamName stream of
      KAFKA_STREAM -> case stream.streamConfig of
        KafkaStream matchedConfig ->
          fork "updating in kafka" $ streamUpdates event matchedConfig
      PROMETHEUS_STREAM ->
        fork "updating in prometheus" $ incrementCounter merchantId eventType deploymentVersion
      _ -> logDebug "Default stream"
```

### 6.3 Consumer Setup (Streamly-based)

```haskell
-- app/kafka-consumers/src/Consumer/Flow.hs:41-80
runConsumer :: L.FlowRuntime -> AppEnv -> ConsumerType -> Consumer.KafkaConsumer -> IO ()
runConsumer flowRt appEnv consumerType kafkaConsumer =
  case consumerType of
    AVAILABILITY_TIME -> availabilityConsumer flowRt appEnv kafkaConsumer
    BROADCAST_MESSAGE -> broadcastMessageConsumer flowRt appEnv kafkaConsumer
    PERSON_STATS      -> updateCustomerStatsConsumer flowRt appEnv kafkaConsumer
    LOCATION_UPDATE   -> locationUpdateConsumer flowRt appEnv kafkaConsumer

-- Streamly pipeline: read → process → batch → commit
availabilityConsumer :: L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
availabilityConsumer flowRt appEnv kafkaConsumer =
  readMessages kafkaConsumer
    & S.mapM (\(msg, key, cr) -> processRealtimeLocationUpdates msg key $> (msg, key, cr))
    & S.intervalsOf (fromIntegral appEnv.dumpEvery)
        (SF.lmap (\(msg, key, cr) -> ((key, msg.m_id), (msg, cr))) (SF.classify buildTimeSeries))
    & S.mapM (Map.traverseWithKey calculateAvailableTime)
    & S.mapM (Map.traverseWithKey commitAllPartitions)
    & S.drain
```

### 6.4 Typed Consumer (PublicTransportQuoteList)

```haskell
-- search-result-aggregator/src/Service/Runner.hs:30-43
run :: (MonadConsumer PublicTransportQuoteList m, HedisFlow m r, MonadFlow m) => m ()
run = do
  withLogTag "Service" $ do
    listenForMessages @PublicTransportQuoteList isRunning $ \PublicTransportQuoteList {..} ->
      withTransactionIdLogTag' transactionId $
        PublicTransport.cachePublicTransportOffers (Id transactionId) quoteList
```

---

## 7. Redis Pattern — Caching, Locks, Rate Limiting, Scheduler

### 7.1 Basic Cache (Set/Get with TTL)

```haskell
-- lib/utils/src/Tools/SharedRedisKeys.hs
setBatchConfig :: CacheFlow m r => Text -> BatchConfig -> m ()
setBatchConfig id batchConfig = do
  Hedis.withCrossAppRedis $ Hedis.setExp (batchConfigKey id) batchConfig 600  -- 10 min TTL

getBatchConfig :: CacheFlow m r => Text -> m (Maybe BatchConfig)
getBatchConfig srId =
  Hedis.runInMultiCloudRedisMaybeResult $ Hedis.withCrossAppRedis (Hedis.safeGet (batchConfigKey srId))
```

### 7.2 Distributed Lock (withLockRedis)

```haskell
-- Storage/Queries/SafetySettingsExtra.hs
upsert :: (...) => Id Person -> UpdateEmergencyInfo -> m ()
upsert (Id personId) info =
  Hedis.withLockRedis (mkSafetySettingsByPersonIdKey personId) 1 $ do  -- 1s lock
    now <- getCurrentTime
    res <- findOneWithKV [Se.Is BeamP.personId $ Se.Eq personId]
    if isJust res
      then updateWithKV [...]   -- Update existing
      else createWithKV settings -- Create new

-- Lock with return value
findSafetySettingsWithFallback :: (...) => Id Person -> Maybe Person -> m SafetySettings
findSafetySettingsWithFallback personId mbPerson =
  Hedis.withLockRedisAndReturnValue (mkSafetySettingsByPersonIdKey personId.getId) 1 $ do
    res <- findOneWithKV [...]
    case res of
      Just settings -> return settings
      Nothing -> createWithKV defaults >> return defaults
```

### 7.3 Sorted Sets for Job Scheduling

```haskell
-- lib/scheduler/src/Lib/Scheduler/JobStorageType/Redis/Queries.hs
createJobFunc :: (...) => AnyJob t -> m ()
createJobFunc (AnyJob job) = do
  key <- getShardKey  -- e.g., "schedulerSet{3}"
  Hedis.withNonCriticalCrossAppRedis $
    Hedis.zAdd key [(utcToMilliseconds job.scheduledAt, AnyJob job)]

getShardKey :: (...) => m Text
getShardKey = do
  setName <- asks (.schedulerSetName)
  maxShards <- asks (.maxShards)
  myShardId <- (`mod` maxShards) . fromIntegral <$> Hedis.incr getShardIdKey
  return $ setName <> "{" <> show myShardId <> "}"  -- Hash slot syntax for cluster
```

### 7.4 Sliding Window Rate Limiting

```haskell
-- lib/yudhishthira/src/Lib/Yudhishthira/Event/KaalChakra/Internal.hs
executeSlidingWindowCount :: (...) => RedisQueryConfig -> UserIdKeyMapping -> m [Maybe BS.ByteString]
executeSlidingWindowCount config userIdMapping = do
  period <- maybe (throwError $ InvalidRequest "requires windowPeriod") pure config.windowPeriod
  periodType <- either (throwError . InvalidRequest) pure $ parseWindowPeriodType periodTypeText
  let opts = SWCTypes.SlidingWindowOptions period periodType
  results <- forM userIdMapping $ \(userId, baseKey) -> do
    count <- Hedis.withCrossAppRedis $ SWC.getCurrentWindowCount baseKey opts
    pure $ Just $ BS.toStrict $ A.encode (A.Number $ fromIntegral count)
  pure results
```

### 7.5 Redis Streams for Consumer Groups

```haskell
-- lib/scheduler/src/Lib/Scheduler/JobStorageType/Redis/Queries.hs
getReadyTasks :: (...) => Maybe Int -> m [(AnyJob t, BS.ByteString)]
getReadyTasks _ = do
  key <- asks (.streamName)
  groupName <- asks (.groupName)
  consumerName <- asks (.version) <&> (.getDeploymentVersion)
  result' <- Hedis.withNonCriticalCrossAppRedis $
    Hedis.xReadGroup groupName consumerName [(key, ">")]
  let result = maybe [] (concatMap (extractKeyValuePairs . records)) result'
  -- ...
```

---

## 8. Error Handling Pattern — The Full Chain

### 8.1 Error Type Hierarchy

Every domain error follows a 4-step pattern:

```haskell
-- app/rider-platform/rider-app/Main/src/Tools/Error.hs:24-41
-- Step 1: Define the error type
data CustomerError = PersonMobileAlreadyExists Text | DeviceTokenNotFound
  deriving (Eq, Show, IsBecknAPIError)

-- Step 2: Wire into exception hierarchy
instanceExceptionWithParent 'HTTPException ''CustomerError

-- Step 3: Human-readable messages
instance IsBaseError CustomerError where
  toMessage (PersonMobileAlreadyExists phoneNo) =
    Just $ "Mobile number " <> phoneNo <> " already exists."
  toMessage DeviceTokenNotFound = Just "Device Token does not exist."

-- Step 4: HTTP status codes and machine-readable error codes
instance IsHTTPError CustomerError where
  toErrorCode = \case
    PersonMobileAlreadyExists _ -> "PERSON_MOBILE_ALREADY_EXISTS"
    DeviceTokenNotFound -> "DEVICE_TOKEN_NOT_FOUND"
  toHttpCode = \case
    PersonMobileAlreadyExists _ -> E400
    DeviceTokenNotFound -> E400

instance IsAPIError CustomerError
```

### 8.2 Throwing Errors

```haskell
-- Pattern: throwError with constructor
throwError $ PersonMobileAlreadyExists phoneNo
throwError $ EstimateDoesNotExist estimateId
throwError $ InvalidRequest "BapId is missing"

-- Pattern: fromMaybeM (throw if Nothing)
person <- QPerson.findById personId
  >>= fromMaybeM (PersonNotFound personId.getId)

-- Pattern: fromEitherM (throw if Left)
auth <- callAPI config.url (ET.client authAPI req) "authEBIX" authAPI
  >>= fromEitherM (ExternalAPICallError (Just "EBIX_AUTH_API") config.url)
```

### 8.3 Error Response (FromResponse for external APIs)

```haskell
-- Tools/Error.hs:90-94
instance FromResponse TrackUrlError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just InvalidRideRequest
    503 -> Just BPPServerUnavailable
    _ -> Just TrackingUrlFailed
```

### 8.4 API Error Response Format

All errors become JSON like:

```json
{
  "errorCode": "PERSON_MOBILE_ALREADY_EXISTS",
  "errorMessage": "Mobile number +91XXXXXXXX already exists with another user."
}
```

With the HTTP status from `toHttpCode` (E400 = 400, E403 = 403, E500 = 500).

### 8.5 More Error Examples

```haskell
-- Estimates
data EstimateError = EstimateDoesNotExist Text | EstimateCancelled Text | EstimateNotFound
instance IsHTTPError EstimateError where
  toHttpCode = \case
    EstimateDoesNotExist _ -> E400
    EstimateCancelled _    -> E403   -- 403 for cancelled
    EstimateNotFound       -> E400

-- Issue Management (lib/shared-services)
data IssueReportError = IssueReportDoesNotExist Text | IssueReportAlreadyExists Text
data IssueOptionError = IssueOptionNotFound Text | IssueOptionDoesNotExist Text | IssueOptionInvalid Text Text
-- Each with full IsBaseError, IsHTTPError, IsAPIError instances

-- Scheduler
data JobDecodeError = InvalidJobType Text | InvalidJobData Text
instance IsHTTPError JobDecodeError where
  toHttpCode _ = E500  -- Internal errors get 500
```

---

## 9. Configuration Pattern — Dhall, Secrets, and Feature Flags

### 9.1 Dhall Config Structure

```
dhall-configs/
├── generic/
│   └── common.dhall          -- Shared type definitions (LogLevel, S3Config, etc.)
├── dev/
│   ├── rider-app.dhall        -- Dev config for rider-app
│   ├── dynamic-offer-driver-app.dhall
│   └── secrets/
│       └── rider-app.dhall    -- Secrets (DB passwords, API keys)
```

### 9.2 Main Service Config

```dhall
-- dhall-configs/dev/rider-app.dhall
let common = ./common.dhall
let sec = ./secrets/rider-app.dhall

let esqDBCfg = {
  connectHost = "localhost",
  connectPort = 5434,
  connectUser = sec.dbUserId,
  connectPassword = sec.dbPassword,
  connectDatabase = "atlas_dev",
  connectSchemaName = "atlas_app",     -- Rider uses atlas_app schema
  connectionPoolCount = +10
}

let hcfg = {
  connectHost = "localhost",
  connectPort = 6379,
  connectAuth = None Text,
  connectDatabase = +0,
  connectMaxConnections = +50,
  connectMaxIdleTime = +30,
  connectTimeout = None Integer,
  connectReadOnly = True
}

let apiRateLimitOptions = { limit = +8000, limitResetTimeInSec = +1 }
let searchRateLimitOptions = { limit = +8000, limitResetTimeInSec = +1 }

let eventStreamMappings =
  [ { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
    , streamConfig = globalCommon.streamConfig.KafkaStream sampleKafkaConfig
    , eventTypes = [
        globalCommon.eventType.RideCreated,
        globalCommon.eventType.RideStarted,
        globalCommon.eventType.RideEnded
      ]
    }
  ]
```

### 9.3 Secrets File

```dhall
-- dhall-configs/dev/secrets/rider-app.dhall
in {
  dbUserId = "<DB_USER>",
  dbPassword = "<DB_PASSWORD>",
  signingKey = "<BASE64_SIGNING_KEY>",
  dashboardToken = "<DASHBOARD_TOKEN>",
  internalAPIKey = "<INTERNAL_API_KEY>",
  ondcTokenMap = [ { mapKey = { merchantId = "<MERCHANT_ID>", domain = "MOBILITY" }
                   , mapValue = { token = "<ONDC_TOKEN>", ondcUrl = ondclogsUrl } } ]
}
```

### 9.4 Loading Config in Haskell

```haskell
-- App.hs:88-89
appCfg <- configModifier <$> readDhallConfigDefault "rider-app"
-- Reads: dhall-configs/dev/rider-app.dhall
-- Deserializes into: AppCfg (via FromDhall typeclass)
```

### 9.5 Merchant Feature Flags (YAML-Driven)

```yaml
# spec/Storage/MerchantConfig.yaml
MerchantConfig:
  tableName: merchant_config
  fields:
    enabled: Bool
    fraudBookingCancellationCountThreshold: Int
    fraudBookingCancellationCountWindow: SlidingWindowOptions
    fraudSearchCountThreshold: Int
    fraudAuthCountThreshold: Maybe Int

  default:
    fraudBookingCancellationCountWindow: '{"period":24, "periodType":"Hours"}'
    fraudAuthCountThreshold: "8"
    fraudAuthCountWindow: '{"period":20, "periodType":"Minutes"}'
    enabled: "True"
```

Cached queries support A/B testing via Yudhishthira:

```haskell
-- Storage/CachedQueries/MerchantConfig.hs
findAllByMerchantOperatingCityId :: (...) =>
  Id MerchantOperatingCity -> Maybe [LYT.ConfigVersionMap] -> m [MerchantConfig]
findAllByMerchantOperatingCityId id mbConfigVersions =
  DynamicLogic.findAllConfigs
    (cast id)
    (LYT.RIDER_CONFIG LYT.MerchantConfig)
    mbConfigVersions
    Nothing
    (Queries.findAllByMerchantOperatingCityId id True)  -- DB fallback
```

---

## 10. Testing Pattern — HSpec, QuickCheck, and Integration Tests

### 10.1 Test Framework Stack

- **HSpec** — BDD-style test specs
- **QuickCheck** — Property-based testing
- **Tasty** — Test tree organization
- **Custom `TestM` monad** — Wraps `FlowR AppEnv` for real Redis/DB access

### 10.2 Test Environment Setup

```haskell
-- lib/location-updates/test/src/Utils.hs
data AppEnv = AppEnv
  { loggerConfig :: LoggerConfig,
    loggerEnv :: LoggerEnv,
    hedisEnv :: HedisEnv,             -- Real Redis connection
    hedisClusterEnv :: HedisEnv,
    coreMetrics :: CoreMetricsContainer,
    -- ...
  }

type TestM = FlowR AppEnv             -- Same monad as production code

runFlow :: Text -> AppEnv -> FlowR AppEnv a -> IO a
runFlow tag appEnv flow = do
  R.withFlowRuntime Nothing $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $
      addAuthManagersToFlowRt flowRt [(...)]
    runFlowR flowRt' appEnv $ withLogTag tag flow

wrapTests :: (Environment.AppCfg -> AppEnv -> IO a) -> IO a
wrapTests func = do
  withHedisEnv defaultHedisCfg ("locationUpdatesTest:" <>) $ \hedisEnv -> do
    let loggerConfig = defaultLoggerConfig {logToFile = True, prettyPrinting = True}
    withLoggerEnv loggerConfig Nothing $ \loggerEnv -> do
      coreMetrics <- Metrics.registerCoreMetricsContainer
      appCfg <- Environment.readConfig "../"
      let appEnv = AppEnv {..}
      func appCfg appEnv
```

### 10.3 Property-Based Tests (QuickCheck)

```haskell
-- lib/location-updates/test/src/RedisAlgorithm.hs
locationUpdatesTree :: AppEnv -> TestTree
locationUpdatesTree appEnv =
  testProperty "Location updates property tests" $ withMaxSuccess 100 $ qcTest appEnv

newtype BatchSize = BatchSize Integer deriving newtype (Show)
newtype PointsNum = PointsNum Int deriving newtype (Show)

instance Arbitrary BatchSize where
  arbitrary = BatchSize <$> choose (5, 20)

instance Arbitrary PointsNum where
  arbitrary = PointsNum <$> choose (8, 100)

qcTest :: AppEnv -> BatchSize -> PointsNum -> GroupingList -> Property
qcTest appEnv (BatchSize batchSize) (PointsNum numPoints) (GroupingList groupingList) =
  classify (numPoints < fromIntegral batchSize) "Lesser points than batch size"
    . classify (div_ > 0 && mod_ == 0) "Total is integer number of batches"
    $ monadic runner $ qcTest' batchSize numPoints groupingList
  where
    runner = ioProperty . runFlow "" appEnv

qcTest' :: Integer -> Int -> [Int] -> PropertyM TestM ()
qcTest' batchSize numPoints groupingList = do
  let pointsToAdd = generatePointsOnTheEquator numPoints
  driverId <- run $ Id <$> generateGUIDText
  forM_ (groupPoints groupingList pointsToAdd) $ processPointsGroup ih driverId
  run $ recalcDistanceBatches ih True driverId
  totalDistance <- run $ checkTraveledDistance driverId
  assert $ equalsEps 100 (fromIntegral (numPoints - 1) * segmentLength) totalDistance
  run $ deleteDistanceKey driverId  -- Clean up
```

### 10.4 Integration Tests with Mock Servers

```haskell
-- lib/location-updates/test/app/Main.hs
main :: IO ()
main = wrapTests $ \appCfg -> specs appCfg >=> defaultMain

specs :: Environment.AppCfg -> AppEnv -> IO TestTree
specs appCfg appEnv = do
  googleCfgEncrypted <- Environment.buildGoogleConfig appCfg.encTools appCfg.googleCfg
  -- Start mock Google server in a thread
  _ <- forkIO $ MockGoogle.runService $ \cfg ->
    cfg & hideLogging
      & #mockDataPath .~ "../../app/mocks/google/mock-data/"
  -- Build test trees
  apiTreeSnap <- testSpec "Testing API using Snap-to-road" $ apiSpec appEnv googleCfgEncrypted
  apiTreeOsrm <- testSpec "Testing API using Osrm" $ apiSpec appEnv osrmConfig
  return $ testGroup "Unit tests" [locationUpdatesTree appEnv, apiTreeSnap, apiTreeOsrm]
```

### 10.5 Client-Based API Testing

```haskell
-- test/src/Common.hs
getAppBaseUrl :: BaseUrl
getAppBaseUrl = BaseUrl Http "localhost" 8013 "/v2"

callRiderApp :: (Show a) => ClientM a -> IO a
callRiderApp = runClient' appBackendClientEnv

-- Verify error responses
verifyError :: Int -> B.ByteString -> Either ClientError a -> IO ()
verifyError expectedCode expectedMessage serverResponse =
  case serverResponse of
    Left (FailureResponse _ response) -> do
      statusCode (responseStatusCode response) `shouldBe` expectedCode
      BL.toStrict (responseBody response) `shouldSatisfy` (expectedMessage `B.isInfixOf`)
    _ -> expectationFailure ("Expected " <> B.toString expectedMessage <> " error.")
```

### 10.6 Test Organization Pattern

```haskell
-- test/src/PublicTransport/Spec.hs
mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  searchSpec <- testSpec "Search" Search.spec
  pure $ testGroup "PublicTransport"
    [ hcSpec,
      after AllSucceed "HealthCheck" $       -- Dependencies between test groups
        testGroup "APIs" [searchSpec]
    ]
```

---

## Quick Reference — Creating a New Feature

1. **Define the YAML spec** in `spec/API/YourFeature.yaml` and `spec/Storage/YourEntity.yaml`
2. **Run the generator:** `, run-generator` from Backend/
3. **Compile:** `cabal build all` to verify generated code
4. **Implement business logic** in the stub at `src/Domain/Action/UI/YourFeature.hs`
5. **Add custom queries** in `src/Storage/Queries/YourEntityExtra.hs` if needed
6. **Define errors** in `src/Tools/Error.hs`
7. **Wire into API** — The generator handles this, but if manual: add to `API/UI.hs`
8. **Write migration SQL** in `dev/migrations/`
9. **Test:** `cabal build all` (with `-Werror`, unused imports are fatal)
