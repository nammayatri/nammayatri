# NammaYatri Haskell Backend — Developer Onboarding Guide

This guide documents every major code pattern in the NammaYatri Haskell backend with real code examples and file references. Read this before writing your first line of code.

---

## 1. Service Pattern — Anatomy of a Microservice

Every backend service follows a four-layer structure: `Main.hs` → `App.hs` → `Environment.hs` → `App/Server.hs`.

### Entry Point (`Main.hs`)

The entry point is deliberately minimal — a thin wrapper that delegates immediately:

```haskell
-- app/rider-platform/rider-app/Main/server/Main.hs:20-22
module Main where
import App
main :: IO ()
main = do
  runRiderApp id
```

The `id` function is a config modifier hook — services can transform the config before startup (useful for test harnesses).

### Application Bootstrap (`App.hs`)

`App.hs` is where the real initialization happens. The `rider-app` version (`app/rider-platform/rider-app/Main/src/App.hs:87-154`) shows the full lifecycle:

```haskell
-- App.hs:87-91 — Config loading and metrics server
runRiderApp :: (AppCfg -> AppCfg) -> IO ()
runRiderApp configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "rider-app"
  Metrics.serve (appCfg.metricsPort)
  runRiderApp' appCfg
```

The main initialization sequence in `runRiderApp'` follows this exact order:

```haskell
-- App.hs:93-154
runRiderApp' :: AppCfg -> IO ()
runRiderApp' appCfg = do
  -- 1. Read pod name for distributed logging
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig

  -- 2. Build environment (Postgres, Redis, Kafka connections)
  appEnv <-
    try (buildAppEnv appCfg)
      >>= handleLeftIO @SomeException exitBuildingAppEnvFailure "Couldn't build AppEnv: "

  -- 3. Configure Warp with graceful shutdown
  let settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appCfg.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
          & setPort (appCfg.port)

  -- 4. Initialize EulerHS FlowRuntime (DB pools, Kafka producer)
  R.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    runFlow flowRt
      ( prepareConnectionRider (ConnectionConfigRider { ... })
          appCfg.kvConfigUpdateFrequency
          >> L.setOption KafkaConn appEnv.kafkaProducerTools
      )

    -- 5. Run startup operations inside the Flow monad
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        -- Run DB migrations
        migrateIfNeeded appCfg.migrationPath appCfg.autoMigrate appCfg.esqDBCfg
          >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
        -- Load KV configs from system_configs table
        kvConfigs <- findById "kv_configs" >>= pure . decodeFromText' @Tables
          >>= fromMaybeM (InternalError "Couldn't find kv_configs table")
        L.setOption KBT.Tables kvConfigs
        -- Initialize city coordinate maps
        initCityMaps
        -- Load all registered BAPs (merchants)
        allBaps <- try QMerchant.loadAllBaps
          >>= handleLeft @SomeException exitLoadAllProvidersFailure "Exception thrown: "
        -- Prepare HTTP auth managers for signing BECKN calls
        flowRt' <- addAuthManagersToFlowRt flowRt
          $ catMaybes [ Just (Nothing, prepareAuthManagers flowRt appEnv allSubscriberIds) ]
        pure flowRt'

    -- 6. Start Warp server with timeout middleware
    let timeoutMiddleware = UE.timeoutEvent flowRt appEnv
          (responseLBS status408 [] "") appCfg.incomingAPIResponseTimeout
    runSettings settings $ timeoutMiddleware (App.run (App.EnvR flowRt' appEnv))
```

### Environment (`Environment.hs`)

The environment module defines two core types — `AppCfg` (configuration loaded from Dhall) and `AppEnv` (runtime state with live connections):

```haskell
-- app/rider-platform/rider-app/Main/src/Environment.hs:100-120
data AppCfg = AppCfg
  { esqDBCfg :: EsqDBConfig,             -- Primary PostgreSQL
    esqDBReplicaCfg :: EsqDBConfig,       -- Read replica
    hedisCfg :: HedisCfg,                 -- Redis standalone
    hedisClusterCfg :: HedisCfg,          -- Redis cluster
    hedisSecondaryClusterCfg :: HedisCfg, -- Secondary cluster (failover)
    riderClickhouseCfg :: ClickhouseCfg,  -- Analytics DB
    kafkaProducerCfg :: KafkaProducerCfg, -- Kafka producer
    port :: Int,
    graceTerminationPeriod :: Seconds,
    ...
  }
  deriving (Generic, FromDhall)
```

`buildAppEnv` initializes all connections; `releaseAppEnv` tears them down:

```haskell
-- Environment.hs (releaseAppEnv)
releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseKafkaProducerTools kafkaProducerTools
  releaseLoggerEnv loggerEnv
  disconnectHedis hedisEnv
  disconnectHedis hedisClusterEnv
  maybe (pure ()) disconnectHedis secondaryHedisClusterEnv
```

The `AppEnv` carries an `isShuttingDown :: TMVar ()` field — when a SIGTERM arrives, `handleShutdown` fills this TMVar, signaling all in-flight requests to drain before `releaseAppEnv` runs.

### Middleware Stack (`App/Server.hs`)

The WAI middleware stack is composed with `&` (forward pipe):

```haskell
-- app/rider-platform/rider-app/Main/src/App/Server.hs:29-43
run :: Env -> Application
run = withModifiedEnv' $ \modifiedEnv ->
  BU.run appAPI API.handler context modifiedEnv
    & logRequestAndResponse' modifiedEnv      -- Log req/res
    & addServantInfo modifiedEnv.appEnv.version appAPI  -- Version headers
    & hashBodyForSignature                    -- Body hash for BECKN signing
    & supportProxyAuthorization               -- Proxy auth header handling
  where
    appAPI = Proxy @API.API
    context =
      verifyPersonAction @(FlowR AppEnv)      -- TokenAuth handler
        :. verifyDashboardAction @(FlowR AppEnv)
        :. verifyPartnerOrganizationAction @(FlowR AppEnv)
        :. EmptyContext
```

Each middleware wraps the previous one. The Servant `context` provides auth verification functions that get injected into `TokenAuth`/`DashboardAuth` combinators at the type level.

---

## 2. API Definition Pattern

APIs are defined as Servant type-level specifications using the `:>` and `:<|>` combinators.

### Example 1: Registration API (multiple auth types)

```haskell
-- app/rider-platform/rider-app/Main/src/API/UI/Registration.hs:72-100
type API =
  "auth"
    :> ( ReqBody '[JSON] DRegistration.AuthReq
           :> Header "x-bundle-version" Version
           :> Header "x-client-version" Version
           :> Header "x-config-version" Version
           :> Header "x-rn-version" Text
           :> Header "x-device" Text
           :> Header "x-forwarded-for" Text
           :> Header "x-sender-hash" Text
           :> Post '[JSON] DRegistration.AuthRes           -- No auth (public endpoint)
         :<|> "signature"
           :> SignatureAuth DRegistration.AuthReq "x-sdk-authorization"
           :> Header "x-bundle-version" Version
           :> Post '[JSON] DRegistration.AuthRes           -- Signature-based auth
         :<|> "password"
           :> ReqBody '[JSON] DRegistration.PasswordAuthReq
           :> Post '[JSON] DRegistration.AuthRes           -- Password auth
         :<|> "business-email"
           :> ( "send-verification"
                  :> TokenAuth                             -- Token-based auth
                  :> Post '[JSON] APISuccess
              )
       )
```

**Key patterns:**
- `TokenAuth` extracts `(Id Person, Id Merchant)` from Bearer token
- `SignatureAuth` verifies request body signatures
- `Header "x-*"` captures versioning/device info as `Maybe` values
- `Capture "id" (Id Type)` extracts phantom-typed IDs from URL paths

### Example 2: Booking API (query params, path captures)

```haskell
-- app/rider-platform/rider-app/Main/src/API/UI/Booking.hs:45-65
type API =
  "rideBooking"
    :> ( Capture "rideBookingId" (Id SRB.Booking)
           :> TokenAuth
           :> Get '[JSON] SRB.BookingAPIEntity
         :<|> Capture "rideBookingId" (Id SRB.Booking)
           :> "cancel"
           :> TokenAuth
           :> ReqBody '[JSON] DCancel.CancelReq
           :> Post '[JSON] APISuccess
         :<|> "list"
           :> TokenAuth
           :> QueryParam "limit" Integer
           :> QueryParam "offset" Integer
           :> QueryParam "onlyActive" Bool
           :> QueryParams "rideStatus" SRB.BookingStatus   -- Multiple values
           :> Get '[JSON] DBooking.BookingListRes
       )
```

### Example 3: Handler bridging (API → Domain Action)

Handlers unwrap auth tuples and delegate to domain logic:

```haskell
-- API/UI/Booking.hs:126-139
bookingStatus :: (Id Person.Person, Id Merchant.Merchant) -> Id SRB.Booking
  -> FlowHandler SRB.BookingAPIEntity
bookingStatus (personId, merchantId) bookingId =
  withFlowHandlerAPI . withPersonIdLogTag personId $
    DBooking.bookingStatus (personId, merchantId) bookingId

bookingList :: (Id Person.Person, Id Merchant.Merchant) -> Maybe Integer
  -> Maybe Integer -> Maybe Bool -> [SRB.BookingStatus]
  -> FlowHandler DBooking.BookingListRes
bookingList (personId, merchantId) mbLimit mbOffset mbOnlyActive statuses =
  withFlowHandlerAPI . withPersonIdLogTag personId $
    DBooking.bookingList (Just personId, merchantId) Nothing False mbLimit mbOffset ...
```

**The bridge pattern is always**: `withFlowHandlerAPI . withPersonIdLogTag personId $ DomainModule.action args`

### Example 4: Driver Ride API (multipart uploads)

```haskell
-- app/provider-platform/dynamic-offer-driver-app/Main/src/API/UI/Ride.hs:112-115
:<|> Capture "rideId" (Id DRide.Ride)
  :> "uploadOdometer"
  :> TokenAuth
  :> MultipartForm Tmp DRide.UploadOdometerReq    -- File upload
  :> Post '[JSON] DRide.UploadOdometerResp
```

### Example 5: API YAML Code Generation

Instead of writing Servant types by hand, most APIs are defined in YAML and generated:

```yaml
# app/rider-platform/rider-app/Main/spec/API/followRide.yaml
module: API.Action.UI.FollowRide
imports:
  Ride: Domain.Types.Ride
apis:
  - endpoint: /follow/ride/{rideId}
    method: GET
    auth: TokenAuth
    params:
      rideId: Id Ride
    response:
      type: API.Types.UI.FollowRide.FollowRideRes
```

Run `, run-generator` to produce the Servant types and handler stubs into `src-read-only/`.

---

## 3. Domain Type Pattern

Domain types live in `Domain/Types/` and represent the business model. Generated types go in `src-read-only/Domain/Types/`; manual extensions go in `src/Domain/Types/Extra/`.

### Generated Domain Type (from Storage YAML)

```haskell
-- app/rider-platform/rider-app/Main/src-read-only/Domain/Types/Booking.hs:34-109
data Booking = Booking
  { bppBookingId :: Maybe (Id BPPBooking),
    createdAt :: UTCTime,
    currency :: Maybe Currency,
    distance :: Maybe Distance,
    estimateId :: Maybe (Id Estimate),
    estimatedDistance :: Maybe Distance,
    estimatedFare :: HighPrecMoney,
    fare :: Maybe HighPrecMoney,
    id :: Id Booking,                         -- Phantom-typed ID
    merchantId :: Id Merchant,
    merchantOperatingCityId :: Id MerchantOperatingCity,
    paymentMethodId :: Maybe (Id PaymentMethod),
    primaryExophone :: Text,
    providerId :: Text,
    riderId :: Id Person,
    riderName :: Maybe Text,
    startTime :: UTCTime,
    status :: BookingStatus,
    tripTermsId :: Maybe (Id TripTerms),
    updatedAt :: UTCTime,
    vehicleVariant :: VehicleCategory,
    ...
  }
  deriving (Generic, Show, FromJSON, ToJSON)
```

**Critical pattern — Phantom-typed IDs**: `Id Booking` is `newtype Id a = Id Text` — you cannot accidentally pass an `Id Person` where `Id Booking` is expected.

### Storage YAML Spec (source of truth)

Storage YAML files define the database schema, queries, and caching rules:

```yaml
# app/rider-platform/rider-app/Main/spec/Storage/SavedReqLocation.yaml
SavedReqLocation:
  tableName: saved_location
  derives: Generic,Show,FromJSON,ToJSON
  fields:
    id: Id SavedReqLocation
    lat: Double
    lon: Double
    street: Maybe Text
    city: Maybe Text
    riderId: Id Person
    tag: Text
    createdAt: UTCTime
    updatedAt: UTCTime

  sqlType:
    ward: character varying(255)
    city: character varying(500)

  excludedFields:           # Omit standard multi-tenancy fields
    - merchantId
    - merchantOperatingCityId

  constraints:
    id: PrimaryKey
    riderId: SecondaryKey

  queries:
    deleteAllByRiderId:
      kvFunction: deleteWithKV
      where: riderId

  cachedQueries:
    findByLatLonAndRiderId:
      keyParams: [riderId, "latLong: Kernel.External.Maps.LatLong"]
      keyMaker: makeIdKey
      dbQuery: findByLatLonAndRiderId
      dbQueryParams: [riderId, "latLong: Kernel.External.Maps.LatLong"]

  extraOperations:
    - EXTRA_QUERY_FILE        # Generates editable stub in src/
    - EXTRA_CACHED_QUERY_FILE
```

Running `, run-generator` from this YAML produces:
- `src-read-only/Domain/Types/SavedReqLocation.hs` — Domain type
- `src-read-only/Storage/Beam/SavedReqLocation.hs` — Beam table
- `src-read-only/Storage/Queries/SavedReqLocation.hs` — Generated queries
- `src-read-only/Storage/CachedQueries/SavedReqLocation.hs` — Cached queries
- `src/Storage/Queries/SavedReqLocationExtra.hs` — Editable query stub (from EXTRA_QUERY_FILE)

### Beam Table (generated)

```haskell
-- app/rider-platform/rider-app/Main/src-read-only/Storage/Beam/Estimate.hs:20-50
data EstimateT f = EstimateT
  { backendAppVersion :: (B.C f (Maybe Text)),
    bppEstimateId :: (B.C f Text),                          -- IDs stored as Text
    discount :: (B.C f (Maybe HighPrecMoney)),               -- Money types
    driversLocation :: (B.C f [LatLong]),                    -- JSON arrays
    estimatedFare :: (B.C f HighPrecMoney),
    id :: (B.C f Text),
    status :: (B.C f EstimateStatus),                        -- Enums
    vehicleCategory :: (B.C f (Maybe VehicleCategory)),
    ...
  }
  deriving (Generic, B.Beamable)

instance B.Table EstimateT where
  data PrimaryKey EstimateT f = EstimateId (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = EstimateId . id

-- KV primary key + secondary indices for Redis/KV lookups
$(enableKVPG ''EstimateT ['id] [['bppEstimateId], ['requestId]])
-- Map to Postgres table with column name overrides
$(mkTableInstancesWithTModifier ''EstimateT "estimate"
  [("oldNightShiftCharge", "night_shift_multiplier")])
```

### Domain ↔ Beam Conversion

```haskell
-- Storage/Queries/Estimate.hs (generated FromTType'/ToTType')
instance FromTType' Beam.JourneyFeedback JourneyFeedback where
  fromTType' (Beam.JourneyFeedbackT {..}) = do
    pure $ Just JourneyFeedback
      { journeyId = Kernel.Types.Id.Id journeyId,   -- Text → Id
        riderId = Kernel.Types.Id.Id riderId,
        merchantId = Kernel.Types.Id.Id <$> merchantId,  -- Maybe Text → Maybe (Id x)
        ...
      }

instance ToTType' Beam.JourneyFeedback JourneyFeedback where
  toTType' (JourneyFeedback {..}) = do
    Beam.JourneyFeedbackT
      { Beam.journeyId = Kernel.Types.Id.getId journeyId,  -- Id → Text
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        ...
      }
```

### Domain Action Handler Pattern

```haskell
-- app/rider-platform/rider-app/Main/src/Domain/Action/UI/AppInstalls.hs:48-74
createAppInstallsDetails ::
  ( HasFlowEnv m r '["apiRateLimitOptions" ::: APIRateLimitOptions],
    EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r
  ) =>
  AppInstallsReq ->
  m APISuccess.APISuccess
createAppInstallsDetails req = do
  checkSlidingWindowLimit (appInstallsHitsCountKey req.deviceToken)  -- Rate limit
  merchant <- QMerchant.findByShortId req.merchantId
    >>= fromMaybeM (MerchantDoesNotExist req.merchantId.getShortId)  -- Error if not found
  id <- generateGUID                                                  -- New UUID
  now <- getCurrentTime                                               -- Current time
  let appInstallsDetails = AppInstalls
        { id = id,
          deviceToken = req.deviceToken,
          source = req.source,
          merchantId = merchant.id,
          createdAt = now,
          updatedAt = now,
          ...
        }
  QAppInstalls.upsert appInstallsDetails                             -- DB write
  return APISuccess.Success
```

---

## 4. Storage Pattern

### Core Query Functions

All queries use the `Sequelize` DSL (`Se.*`) with `findOneWithKV`, `findAllWithKV`, `createWithKV`, `updateWithKV`, `deleteWithKV`.

#### findById (single record by primary key)

```haskell
-- src-read-only/Storage/Queries/Estimate.hs:31-32
findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
  => Id Estimate -> m (Maybe Estimate)
findById id = do
  findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]
```

#### findBy with compound WHERE

```haskell
-- src-read-only/Storage/Queries/Estimate.hs:34-37
findBySRIdAndStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
  => EstimateStatus -> Id SearchRequest -> m (Maybe Estimate)
findBySRIdAndStatus status requestId = do
  findOneWithKV [Se.And [ Se.Is Beam.status $ Se.Eq status,
                          Se.Is Beam.requestId $ Se.Eq (getId requestId) ]]
```

#### findAll (multiple records)

```haskell
-- src-read-only/Storage/Queries/Estimate.hs:25-26
findAllBySRId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
  => Id SearchRequest -> m [Estimate]
findAllBySRId requestId = do
  findAllWithKVAndConditionalDB
    [Se.Is Beam.requestId $ Se.Eq (getId requestId)] Nothing
```

#### create (insert)

```haskell
-- src-read-only/Storage/Queries/SavedReqLocation.hs:19-22
create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
  => SavedReqLocation -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
  => [SavedReqLocation] -> m ()
createMany = traverse_ create
```

**Important**: Don't wrap single `create` calls in `runInTransaction`. The framework handles transactions.

#### updateOneWithKV (single field update)

```haskell
-- src-read-only/Storage/Queries/Estimate.hs:39-40
updateStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
  => EstimateStatus -> Id Estimate -> m ()
updateStatus status id = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.updatedAt _now, Se.Set Beam.status status]
    [Se.Is Beam.id $ Se.Eq (getId id)]
```

#### updateWithKV (multi-field update)

```haskell
-- src-read-only/Storage/Queries/Estimate.hs:42-47
updateStatusAndProviderUrl :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
  => EstimateStatus -> BaseUrl -> Id Estimate -> m ()
updateStatusAndProviderUrl status providerUrl id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.updatedAt _now,
      Se.Set Beam.status status,
      Se.Set Beam.providerUrl (showBaseUrl providerUrl) ]
    [Se.Is Beam.id $ Se.Eq (getId id)]
```

#### updateByPrimaryKey (full record update)

```haskell
-- src-read-only/Storage/Queries/Estimate.hs:52-70
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
  => Estimate -> m ()
updateByPrimaryKey (Estimate {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.backendAppVersion backendAppVersion,
      Se.Set Beam.bppEstimateId (getId bppEstimateId),
      Se.Set Beam.discount (discount <&> (.amount)),
      Se.Set Beam.estimatedFare estimatedFare.amount,
      Se.Set Beam.clientManufacturer (clientDevice >>= (.deviceManufacturer)),
      Se.Set Beam.updatedAt _now,
      ... -- all fields
    ]
    [Se.Is Beam.id $ Se.Eq (getId id)]
```

#### deleteWithKV

```haskell
-- src-read-only/Storage/Queries/SavedReqLocation.hs:25-26
deleteAllByRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
  => Id Person -> m ()
deleteAllByRiderId riderId = do
  deleteWithKV [Se.Is Beam.riderId $ Se.Eq (getId riderId)]
```

### Caching Layer (CachedQueries)

Cached queries implement read-through caching with Redis:

```haskell
-- src-read-only/Storage/CachedQueries/SavedReqLocation.hs:21-36
findByLatLonAndRiderId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
  => Id Person -> LatLong -> m (Maybe SavedReqLocation)
findByLatLonAndRiderId riderId latLong = do
  (Hedis.safeGet $ makeIdKey riderId latLong)    -- 1. Try Redis first
    >>= ( \case
            Just a -> pure (Just a)               -- 2. Cache hit → return
            Nothing ->
              flip whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.setExp (makeIdKey riderId latLong) dataToBeCached expTime
                )                                  -- 3. Cache miss → write to Redis
                /=<< Queries.findByLatLonAndRiderId riderId latLong  -- 4. Fetch from DB
        )

-- Cache invalidation
deleteSavedLocation :: CacheFlow m r => Id Person -> LatLong -> m ()
deleteSavedLocation riderId latLong =
  Hedis.del $ makeIdKey riderId latLong
```

**Cache key convention**: `"CachedQueries:EntityType:" <> ":Field1-" <> value1 <> ":Field2-" <> value2`

**Cross-app cache** (shared between rider-app and driver-app):

```haskell
-- src-read-only/Storage/CachedQueries/DeviceVehicleMapping.hs
findByDeviceIdCached :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r)
  => Text -> m (Maybe DeviceVehicleMapping)
findByDeviceIdCached deviceId = do
  Hedis.withCrossAppRedis (Hedis.safeGet $ key)   -- Uses shared Redis instance
    >>= ( \case
            Just a -> pure (Just a)
            Nothing -> ...
              Hedis.withCrossAppRedis $ Hedis.setExp key dataToBeCached expTime
        )
```

---

## 5. External API Pattern

### BECKN Protocol Calls

BECKN calls use signed HTTP requests with Kafka audit logging:

```haskell
-- app/rider-platform/rider-app/Main/src/SharedLogic/CallBPP.hs:54-74
searchV2 ::
  ( MonadFlow m, CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBFlow m r
  ) =>
  BaseUrl -> API.SearchReqV2 -> Id Merchant -> m API.SearchRes
searchV2 gatewayUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.searchReqContext.contextBapId
    & fromMaybeM (InvalidRequest "BapId is missing")
  -- Signed HTTP call via EulerHS
  res <- callBecknAPIWithSignature' merchantId bapId "search"
    API.searchAPIV2 gatewayUrl internalEndPointHashMap req
  -- Async audit log to Kafka
  fork ("Logging Internal API Call") $ do
    let transactionId = req.searchReqContext.contextTransactionId <&> UUID.toText
    ApiCallLogger.pushInternalApiCallDataToKafka
      "searchV2" "BAP" transactionId (Just req) res
  pure res
```

The same pattern repeats for `selectV2`, `initV2`, `confirmV2`, `cancelV2` — each calls `callBecknAPIWithSignature'` with the appropriate API proxy.

### Euler HTTP Client Pattern

For non-BECKN external APIs, create a Servant client from the API type:

```haskell
-- app/mocks/google/src/Tools/Client.hs:26-41
snapToRoad :: (CoreMetrics m, MonadFlow m)
  => BaseUrl -> Text -> Text -> m SnapToRoadResponse
snapToRoad roadsUrl apiKey path = do
  let eulerClient = Euler.client (Proxy @Roads.SnapToRoadAPI)
      interpolate = True
  callAPI roadsUrl (eulerClient apiKey interpolate path) "snap-to-road"
    (Proxy @Roads.SnapToRoadAPI)
    >>= fromEitherM (\err -> InternalError $ "Failed to call snap-to-road: " <> show err)
```

### External Auth Token with Redis Cache

```haskell
-- app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Bus/EBIX/Auth.hs:52-65
getAuthToken :: (...) => EBIXConfig -> m Text
getAuthToken config = do
  authToken :: Maybe Text <- Hedis.get authTokenKey
  case authToken of
    Nothing -> do
      password <- decrypt config.password
      auth <- callAPI config.networkHostUrl
        (ET.client authAPI $ AuthReq config.username password)
        "authEBIX" authAPI
        >>= fromEitherM (ExternalAPICallError (Just "EBIX_AUTH_API") config.networkHostUrl)
      Hedis.setExp authTokenKey auth.accessToken auth.expiresIn.getSeconds
      return auth.accessToken
    Just token -> return token
```

### OAuth2 with Distributed Lock (prevent thundering herd)

```haskell
-- app/rider-platform/rider-app/Main/src/ExternalBPP/ExternalAPI/Subway/CRIS/Auth.hs:47-82
resetAuthToken :: (...) => CRISConfig -> m Text
resetAuthToken config = do
  lockAcquired <- Hedis.tryLockRedis getCRISTokenRefreshLockKey 30
  if lockAcquired
    then do
      let unlockLock = Hedis.unlockRedis getCRISTokenRefreshLockKey
      tokenRes <- (do
          consumerKey <- decrypt config.consumerKey
          consumerSecret <- decrypt config.consumerSecret
          let basicAuthData = mkBasicAuthData consumerKey consumerSecret
          callAPI config.baseUrl (ET.client authAPI basicAuthData ...) "authCRIS" authAPI
            >>= fromEitherM (ExternalAPICallError (Just "CRIS_AUTH_API") ...)
        ) `finally` unlockLock
      -- Cache at 90% of TTL to avoid edge-case expiry
      Hedis.setExp getCRISTokenKey tokenRes.access_token
        (tokenRes.expires_in * 90 `div` 100)
      return tokenRes.access_token
    else do
      threadDelay 3000000  -- Wait for lock holder to complete
      mbToken <- Hedis.get getCRISTokenKey
      maybe (resetAuthToken config) return mbToken  -- Retry or return
```

### 401 Retry Pattern

```haskell
-- CRIS/Auth.hs:94-154
callCRISAPI config proxy clientFn description = do
  token <- getAuthToken config
  eitherResp <- callApiUnwrappingApiError ... config.baseUrl (clientFn token) ...
  case eitherResp of
    Left err -> if is401Error err
      then do
        freshToken <- resetAuthToken config     -- Force token refresh
        callApiUnwrappingApiError ... (clientFn freshToken) ...  -- Retry once
      else throwError ...
    Right res -> return res
```

---

## 6. Kafka Pattern

### Consumer Architecture (Streamly-based)

Consumers use Streamly for compositional stream processing with batching:

```haskell
-- app/kafka-consumers/src/Consumer/Flow.hs:41-65
runConsumer :: L.FlowRuntime -> AppEnv -> ConsumerType -> Consumer.KafkaConsumer -> IO ()
runConsumer flowRt appEnv consumerType kafkaConsumer = do
  case consumerType of
    AVAILABILITY_TIME -> availabilityConsumer flowRt appEnv kafkaConsumer
    BROADCAST_MESSAGE -> broadcastMessageConsumer flowRt appEnv kafkaConsumer
    PERSON_STATS      -> updateCustomerStatsConsumer flowRt appEnv kafkaConsumer
    LOCATION_UPDATE   -> locationUpdateConsumer flowRt appEnv kafkaConsumer

-- Batch processing with chunking
updateCustomerStatsConsumer :: L.FlowRuntime -> AppEnv -> Consumer.KafkaConsumer -> IO ()
updateCustomerStatsConsumer flowRt appEnv kafkaConsumer = do
  readMesssageWithWaitAndTimeRange kafkaConsumer appEnv
    & S.chunksOf appEnv.kafkaReadBatchSize addToList    -- Batch N messages
    & S.mapM updateCustomerStatsWithFlow                -- Process batch
    & S.delay (fromIntegral $ appEnv.kafkaReadBatchDelay.getSeconds)  -- Throttle
    & S.drain                                            -- Consume stream
```

### Message Types

```haskell
-- app/kafka-consumers/src/Consumer/AvailabilityTime/Types.hs:43-56
data LocationUpdates = LocationUpdates
  { r_id :: Maybe Text,         -- Ride ID
    ts :: UTCTime,               -- Timestamp
    mocid :: Maybe Text,         -- MerchantOperatingCity ID
    pt :: LatLong,               -- Position
    acc :: Maybe Double,         -- GPS accuracy
    ride_status :: RideStatus,   -- ON_RIDE | ON_PICKUP | IDLE
    speed :: Maybe Double,
    da :: Maybe Bool,            -- Driver active flag
    m_id :: Text                 -- Merchant ID
  }
  deriving (Generic, FromJSON, ToJSON, Show)
```

### Producer Pattern

```haskell
-- app/rider-platform/rider-app/Main/src/Lib/JourneyModule/Location.hs:41-64
pushRiderLocationToKafka ::
  ( MonadFlow m, Log m,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]
  ) =>
  Id Journey -> RiderLocationReq -> Maybe Text -> m ()
pushRiderLocationToKafka journeyId req busVehicleNo = do
  let event = RiderLocationEvent
        { journeyId = journeyId.getId,
          latitude = req.latLong.lat,
          longitude = req.latLong.lon,
          timestamp = req.currTime,
          busVehicleNo = busVehicleNo
        }
  let topicName = "rider-location-updates"
  let key = journeyId.getId
  fork "Pushing rider location to Kafka" $ do
    produceMessage (topicName, Just (TE.encodeUtf8 key)) event
      `catch` \(e :: SomeException) ->
        logError $ "Failed to push rider location to Kafka: " <> show e
```

---

## 7. Redis Pattern

### Distributed Locks

```haskell
-- app/rider-platform/rider-app/Main/src/Storage/Queries/SafetySettingsExtra.hs:37-50
upsert :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r)
  => Id Person -> UpdateEmergencyInfo -> m ()
upsert (Id personId) UpdateEmergencyInfo {..} =
  Hedis.withLockRedis (mkSafetySettingsByPersonIdKey personId) 1 $ do  -- 1s TTL
    now <- getCurrentTime
    res <- findOneWithKV [Se.And [Se.Is BeamP.personId $ Se.Eq personId]]
    if isJust res
      then updateWithKV [Se.Set BeamP.updatedAt now, ...] [...]
      else createWithKV safetySettings

mkSafetySettingsByPersonIdKey :: Text -> Text
mkSafetySettingsByPersonIdKey personId = "SafetySettings:PersonId:" <> personId
```

### List Operations with TTL

```haskell
-- Lib/JourneyModule/Location.hs:66-94
addPoint :: (...) => Id Journey -> RiderLocationReq -> Maybe Text -> m ()
addPoint journeyId req busVehicleNo = do
  let key = makeLocationRedisKey journeyId
  lPush key $ NE.singleton req         -- Push to list head
  lTrim key 0 10                        -- Keep only last 10 points
  Hedis.expire key 21600                -- 6-hour TTL
  pushRiderLocationToKafka journeyId req busVehicleNo

getLastThreePoints :: HedisFlow m env => Id Journey -> m [RiderLocationReq]
getLastThreePoints journeyId = do
  currentTime <- getCurrentTime
  points <- lRange (makeLocationRedisKey journeyId) 0 (-1)
  return (take 3 $ filter (recentPoints currentTime) points)

makeLocationRedisKey :: Id person -> Text
makeLocationRedisKey driverId = mconcat ["locations", ":", driverId.getId]
```

### Sorted Sets with Sharding

```haskell
-- app/kafka-consumers/src/Consumer/LocationUpdate/Processor.hs:30-64
processLocationData :: [Text] -> [(LocationUpdates, DriverId)] -> Flow ()
processLocationData enabledMerchantCityIds locationData = do
  let encodedVals = mapFilter
        (\(LocationUpdates{..}, driverId) ->
          (utcToDouble ts, driverId, idToIdHashNumber driverId))
        (\(LocationUpdates{..}, _) ->
          (fromMaybe "" mocid) `elem` enabledMerchantCityIds)
        locationData
  -- Group by shard number
  let encodeValsInShardMap = foldl
        (\acc (ts, dId, dIdHashNumber) ->
          HM.insert dIdHashNumber ((ts, dId) : fromMaybe [] (HM.lookup dIdHashNumber acc)) acc
        ) HM.empty encodedVals
  -- Write to sharded sorted sets
  encodeValsWithShardKey <- mapM
    (\(dIdHashNumber, val) -> (,val) <$> getKeyWithShard dIdHashNumber)
    (HM.toList encodeValsInShardMap)
  void $ mapM (\(key, val) -> Redis.zAdd key val) encodeValsWithShardKey

makeShardKey :: Integer -> Text
makeShardKey shardNo = "driver-last-location-update-{shard-" <> show shardNo <> "}"
```

### Producer Lock Pattern

```haskell
-- lib/producer/src/Producer/Flow.hs:36-77
runProducer :: Flow ()
runProducer = do
  myShardId <- getMyShardKey
  -- Acquire lock before producing, auto-expires in 10s
  Hedis.whenWithLockRedis (getShardedKey producerLockKey myShardId) 10 $ do
    (_, diff) <- withTimeGeneric "producer" $ do
      producerTimestampKey <- asks (.producerTimestampKey)
      startTime <- getTime producerTimestampKey
      endTime <- getCurrentTimestamp
      -- Read scheduled jobs from sorted set
      currentJobs <- Hedis.withNonCriticalCrossAppRedis $
        Hedis.zRangeByScoreByCount myShardSetKey startTime endTime 0 10000
      -- Insert into Redis stream for consumers
      insertIntoStream currentJobs
      -- Cleanup processed range
      Hedis.withNonCriticalCrossAppRedis $
        Hedis.zRemRangeByScore myShardSetKey startTime endTime
```

---

## 8. Error Handling Pattern

### Defining Error Types

Every service defines its errors in `Tools/Error.hs` using four typeclasses:

```haskell
-- app/rider-platform/rider-app/Main/src/Tools/Error.hs:24-42
data CustomerError
  = PersonMobileAlreadyExists Text
  | DeviceTokenNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''CustomerError  -- Exception hierarchy

instance IsBaseError CustomerError where
  toMessage (PersonMobileAlreadyExists phoneNo) =
    Just $ "Mobile number " <> phoneNo <> " already exists with another user."
  toMessage DeviceTokenNotFound = Just "Device Token does not exist."

instance IsHTTPError CustomerError where
  toErrorCode = \case
    PersonMobileAlreadyExists _ -> "PERSON_MOBILE_ALREADY_EXISTS"
    DeviceTokenNotFound -> "DEVICE_TOKEN_NOT_FOUND"
  toHttpCode = \case
    PersonMobileAlreadyExists _ -> E400   -- Maps to HTTP 400
    DeviceTokenNotFound -> E400

instance IsAPIError CustomerError  -- Marker typeclass

-- Another example with different HTTP codes:
data TrackUrlError = InvalidRideRequest | TrackingUrlFailed | BPPServerUnavailable
  deriving (Eq, Show, IsBecknAPIError)

instance IsHTTPError TrackUrlError where
  toHttpCode = \case
    InvalidRideRequest -> E412    -- Precondition Failed
    TrackingUrlFailed -> E500     -- Internal Server Error
    BPPServerUnavailable -> E503  -- Service Unavailable
```

### Throwing Errors

**`fromMaybeM`** — the most common pattern, throws if DB query returns Nothing:

```haskell
person <- QP.findById personId
  >>= fromMaybeM (PersonDoesNotExist personId.getId)

merchant <- QMerc.findById person.merchantId
  >>= fromMaybeM (MerchantNotFound person.merchantId.getId)

riderCfg <- QRC.findByMerchantOperatingCityId merchantOperatingCityId
  >>= fromMaybeM (RiderConfigNotFound merchantOperatingCityId.getId)
```

**`throwError`** — for validation failures:

```haskell
when (returnTime <= startTime) $
  throwError (InvalidRequest "Return time should be greater than start time")

unless (stopCity == originCity) $
  throwError RideNotServiceable

when (tag == pack "") $
  throwError $ InvalidRequest "Location tag cannot be empty"
```

### External API Error Handling

```haskell
-- FromResponse typeclass maps HTTP status to error constructors
instance FromResponse TrackUrlError where
  fromResponse resp = case statusCode $ responseStatusCode resp of
    400 -> Just InvalidRideRequest
    503 -> Just BPPServerUnavailable
    _ -> Just TrackingUrlFailed
```

### Error Response Format

All errors become JSON responses like:

```json
{
  "errorCode": "PERSON_MOBILE_ALREADY_EXISTS",
  "errorMessage": "Mobile number +91XXXXXXXXXX already exists with another user."
}
```

with the appropriate HTTP status code from `toHttpCode`.

---

## 9. Configuration Pattern

### Dhall Configuration Structure

Configs live in `dhall-configs/` with a layered structure:

```
dhall-configs/
  generic/        # Shared defaults
    common.dhall
  dev/            # Development overrides
    common.dhall
    rider-app.dhall
    dynamic-offer-driver-app.dhall
    driver-offer-allocator.dhall
    secrets/      # Encrypted credentials
      common.dhall
      rider-app.dhall
```

### Real Config Example

```dhall
-- dhall-configs/dev/common.dhall:1-57
let sec = ./secrets/common.dhall
let globalCommon = ../generic/common.dhall

let googleCfg =
  { googleMapsUrl = "https://maps.googleapis.com/maps/api/"
  , googleRoadsUrl = "https://roads.googleapis.com/"
  , googleKey = sec.googleKey           -- From secrets
  , useAdvancedDirections = True
  , googleRouteConfig =
    { computeAlternativeRoutes = False
    , routePreference = "TRAFFIC_AWARE_OPTIMAL"
    , url = "https://routes.googleapis.com/"
    }
  }

let SchedulerType = < RedisBased | DbBased >   -- Dhall union type

in  { smsSessionConfig = globalCommon.smsSessionConfig
    , autoMigrate = globalCommon.autoMigrate
    , loggerConfig = globalCommon.loggerConfig // { logToFile = True }
    , schedulerType = SchedulerType
    , ondcGatewayUrl = "http://localhost:8015/v1"
    , nyGatewayUrl = "http://localhost:8015/v1"
    }
```

### Scheduler Config

```dhall
-- dhall-configs/dev/driver-offer-allocator.dhall
let schedulerConfig =
  { esqDBCfg = appCfg.esqDBCfg
  , hedisCfg = appCfg.hedisCfg
  , hedisPrefix = "driver-offer-scheduler"
  , port = +8055
  , loopIntervalSec = +5
  , expirationTime = +60
  , waitBeforeRetry = +1
  , tasksPerIteration = +20
  , graceTerminationPeriod = +10
  , schedulerType = common.schedulerType.RedisBased
  , schedulerSetName = "Scheduled_Jobs"
  , streamName = "Available_Jobs"
  , maxThreads = +10
  }
```

### Config Loading in Haskell

```haskell
-- App.hs:89
appCfg <- configModifier <$> readDhallConfigDefault "rider-app"
-- Reads dhall-configs/dev/rider-app.dhall, deserializes via FromDhall into AppCfg
```

The `AppCfg` record derives `FromDhall` automatically — Dhall field names must match Haskell field names exactly.

### Consumer Config with Custom FromDhall

```haskell
-- app/kafka-consumers/src/Environment.hs:44-73
data ConsumerConfig = ConsumerConfig
  { topicNames :: [TopicName],
    consumerProperties :: !ConsumerProperties
  }

instance FromDhall ConsumerConfig where
  autoWith _ =
    record
      ( ConsumerConfig
          <$> field "topicNames" (map TopicName <$> list strictText)
          <*> field "consumerProperties" customeDecoder
      )
    where
      customeDecoder = record $
        (\a b c d -> a <> logLevel KafkaLogInfo <> b <> c <> compression d)
          . (groupId . ConsumerGroupId) <$> field "groupId" strictText
          <*> (shouldAutoCommit <$> field "autoCommit" (maybe integer))
          <*> (brokersList <$> field "brockers" (map BrokerAddress <$> list strictText))
          <*> field "kafkaCompression" (castCompression <$> auto)
```

---

## 10. Testing Pattern

### Test Suite Structure

Tests use Tasty + Hspec with ordered dependency chains:

```haskell
-- test/src/Mobility/ARDU/Spec.hs:29-58
mkTestTree :: IO TestTree
mkTestTree = do
  hcSpec <- testSpec "HealthCheck" HC.spec
  mapsCfgSpec <- testSpec "MapsConfig" MapsConfig.spec
  sfSpec <- testSpec "SuccessFlow" SF.spec
  dcSpec <- testSpec "CancelFlow" DC.spec
  ndSpec <- testSpec "NearestDrivers" ND.spec
  srSpec <- testSpec "SyncRide" SR.spec
  return $
    testGroup "ARDU"
      [ hcSpec,                                              -- Run first
        after AllSucceed "HealthCheck" $                      -- Then configs
          testGroup "Merchant configs" [mapsCfgSpec],
        after AllSucceed "Merchant configs" $                 -- Then API tests
          testGroup "APIs" [ndSpec, dcSpec, sfSpec, srSpec]
      ]
```

### Test Client Setup

Tests create Servant clients that hit the real running services:

```haskell
-- test/src/Common.hs:37-50
getAppBaseUrl :: BaseUrl
getAppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8013,              -- rider-app port
      baseUrlPath = "/v2"
    }

appBackendClientEnv :: ClientEnv
appBackendClientEnv = mkClientEnv defaultManager getAppBaseUrl

callRiderApp :: (Show a) => ClientM a -> IO a
callRiderApp = runClient' appBackendClientEnv
```

### API Client Generation

```haskell
-- test/src/Common.hs:52-67
searchServices ::
  Text -> SearchReq -> Maybe Version -> Maybe Version -> Maybe Version
  -> Maybe Text -> Maybe (Id Client) -> Maybe Text -> Maybe Bool -> Maybe Bool
  -> ClientM SearchResp
searchServices = client (Proxy :: Proxy SearchAPI)

getQuotes :: Id SearchRequest -> Text -> Maybe Bool -> ClientM GetQuotesRes
getQuotes = client (Proxy :: Proxy QuoteAPI)
```

### Error Verification

```haskell
-- test/src/Common.hs:84-91
verifyError :: Int -> B.ByteString -> Either ClientError a -> IO ()
verifyError expectedCode expectedMessage serverResponse = do
  case serverResponse of
    Left (FailureResponse _ response) -> do
      statusCode (responseStatusCode response) `shouldBe` expectedCode
      BL.toStrict (responseBody response)
        `shouldSatisfy` (expectedMessage `B.isInfixOf`)
    _ -> expectationFailure
           ("Expected " <> B.toString expectedMessage <> " error.")
```

### Signature Auth for Tests

```haskell
-- test/src/Common.hs:93-103
privateKey :: Base64
privateKey = "Lw9M+SHLY+yyTmqPVlbKxgvktZRfuIT8nHyE89Jmf+o="

signRequest :: ToJSON req => req -> POSIXTime -> Text -> Text -> ByteString
signRequest req now orgId keyId =
  let body = BL.toStrict $ J.encode req
      bodyHash = HttpSig.becknSignatureHash body
      headers = [("(created)", ""), ("(expires)", ""), ("digest", "")]
      params = HttpSig.mkSignatureParams orgId keyId now 600 HttpSig.Ed25519
      signature = fromJust $ HttpSig.sign privateKey params bodyHash headers
   in HttpSig.encode $ HttpSig.SignaturePayload signature params
```

### Running Tests

```bash
# Start all external services first
, run-mobility-stack-dev

# Run all integration tests
cabal test all

# Run specific test suite
cabal test rider-app-test
```

Tests are **integration tests** that require running services (Postgres, Redis, Kafka, and the app servers). There is no mocking of the database layer — tests hit real databases.

---

## Quick Reference: Common Patterns Cheat Sheet

| Pattern | Code | Import |
|---------|------|--------|
| Generate UUID | `id <- generateGUID` | `Kernel.Utils.Common` |
| Get current time | `now <- getCurrentTime` | `Kernel.Utils.Common` |
| Find or throw | `x <- QFoo.findById id >>= fromMaybeM (FooNotFound id.getId)` | `Kernel.Utils.Common` |
| Log info | `logInfo "message"` | `Kernel.Utils.Common` |
| Throw error | `throwError $ InvalidRequest "reason"` | `Kernel.Types.Error` |
| Rate limit | `checkSlidingWindowLimit key` | `Kernel.Types.SlidingWindowLimiter` |
| DB insert | `QFoo.create record` | Generated queries |
| DB find one | `findOneWithKV [Se.Is Beam.field $ Se.Eq value]` | `Kernel.Beam.Functions` |
| DB find many | `findAllWithKV [Se.Is Beam.field $ Se.Eq value]` | `Kernel.Beam.Functions` |
| DB update | `updateOneWithKV [Se.Set Beam.f v] [Se.Is Beam.id $ Se.Eq id]` | `Kernel.Beam.Functions` |
| Redis get | `Hedis.safeGet key` | `Kernel.Storage.Hedis` |
| Redis set+TTL | `Hedis.setExp key value expirySeconds` | `Kernel.Storage.Hedis` |
| Redis lock | `Hedis.withLockRedis key ttl action` | `Kernel.Storage.Hedis` |
| Fork async | `fork "description" action` | `Kernel.Utils.Common` |
| Kafka produce | `produceMessage (topic, Just key) event` | `Kernel.Streaming.Kafka` |
| External API | `callAPI url (Euler.client proxy args) "name" proxy >>= fromEitherM ...` | `Kernel.Utils.Servant.Client` |
