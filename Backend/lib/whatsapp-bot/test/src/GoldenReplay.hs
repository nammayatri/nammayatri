{-# LANGUAGE TemplateHaskell #-}

-- | Golden characterization-replay harness — Haskell port of the TypeScript
-- connector's @test/harness.ts@ + @test/golden-fixtures.test.ts@.
--
-- It replays each embedded golden fixture through the WEBHOOK LAYER: every
-- step's raw Meta Cloud API envelope is fed through the REAL kernel decoder
-- ('Kernel.External.Meta.decodeWebhookEnvelope') then 'WhatsappBot.Inbound.parseInbound'
-- (so the goldens exercise the real codecs), resolves the merchant-scoped
-- session, and runs 'WhatsappBot.Engine.handleMessage' against a MOCK
-- 'BotEnv IO' (all handles over IORefs). Each tracker step advances the virtual
-- clock and fires one 'WhatsappBot.Tracker.trackerTick'. For every step it
-- asserts BOTH oracles as strict, ordered, deep-equal on aeson 'Value':
--
--   * @structured(recordedOutbound[sinceStep])   == step.expectOutbound@
--   * @recordedBackend[sinceStep]                == step.expectBackend@
--
-- Ports, faithfully:
--   * @structured()@ (harness.ts:189-197): @{kind,to,merchant,buttons?,link?}@,
--     button DATA-ids only (copy dropped). A LIST send is recorded exactly like
--     a buttons send (@kind="buttons"@, row data-ids under @buttons@) — the TS
--     FakeRecordingConnector overrides @sendWithButtons@ and always tags it
--     @'buttons'@; there is no separate @list@ kind in any fixture.
--   * @normalizeArgs()@ (harness.ts:231-256): per-method arg normalization —
--     drop auth/config/Date; @projPlace@ = @{placeId,lat,lon}@; @getActiveBookings@
--     recorded with @[]@. Baked into each mock method (it records already-normalized
--     args, exactly like the TS spy wrapper).
--   * The canned NY mock (mock-client.ts): every id/string/quote and the
--     ride-progression-by-virtual-elapsed schedule (0s assigned / 6s arrived /
--     12s started / 20s ended).
--   * The knobs (golden-fixtures.test.ts:74-78): @401@ / @empty@ / @gate@.
--   * The step modes: @normal@ / @flush@ / @concurrent@ (scripted MVar interleave).
--
-- INTEGRATOR NOTES (reconcile with the parallel Engine.hs / Tracker.hs):
--   * Assumes @Engine.handleMessage :: Monad m => BotEnv m -> InboundEvent -> m ()@
--     and @Engine.scopedSessionId :: MerchantCtx -> InboundEvent -> Text@.
--   * Assumes the 'TrackerDeps' record shape below (registry + getBookingDetails
--     + sender + sessions + clock). If Tracker.hs defines it differently, only
--     the one construction in 'makeWorld' needs to change.
--   * The 30000ms per-step virtual-advance cap from the TS fake-timer harness is
--     intentionally NOT enforced: the Haskell virtual clock drains poll loops
--     synchronously and every loop is attempt-bounded (flexi 10, regular 6,
--     driver 90) with the happy paths breaking on the first poll, so no fixture
--     advances near 30s. Enforcing an artificial cap would corrupt the mock's
--     time-based ride progression.
--   * @flush@ and @normal@ steps run identically (handleMessage to completion);
--     @sleepMs@ advances the virtual clock instantly, so poll loops drain in-line.
--   * @getBookingDetails@ first-seen is tracked per-fixture (fresh IORef map),
--     which is equivalent to the TS module-level map here because every fixture
--     first-polls @mock-booking-001@ at its @systemTime@ (virtual t=0).
module GoldenReplay
  ( goldenTests,
    copyChecks,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Data.Aeson (Value (String), eitherDecodeStrict', encode, object, (.=))
import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir, makeRelativeToProject)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime, diffUTCTime)
import qualified Kernel.External.Meta as Meta
import Kernel.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import WhatsappBot.Engine (handleMessage, scopedSessionId)
import WhatsappBot.Env
import WhatsappBot.Handles
import WhatsappBot.I18n.En (en)
import WhatsappBot.Inbound (parseInbound)
import WhatsappBot.Tracker (TrackerDeps (..), trackerTick)
import WhatsappBot.Types

-- ===========================================================================
-- Fixture schema (aeson, tolerant of extra keys: description/seed/
-- expectTokenClearedAfter are ignored by the generic decoder).
-- ===========================================================================

data Fixture = Fixture
  { scenario :: Text,
    env :: FixtureEnv,
    knobs :: Maybe [Knob],
    steps :: [Step],
    trackerSteps :: Maybe [TrackerStep]
  }
  deriving (Generic, FromJSON)

data FixtureEnv = FixtureEnv
  { merchant :: Text, -- phone_number_id, e.g. "pn_flexi"/"pn_reg"
    systemTime :: UTCTime -- ISO-8601 seed for the virtual clock
  }
  deriving (Generic, FromJSON)

data Knob = Knob
  { method :: Text,
    behavior :: Text -- "401" | "empty" | "gate"
  }
  deriving (Generic, FromJSON)

data Step = Step
  { note :: Maybe Text,
    inbound :: Value, -- raw Meta webhook envelope
    concurrent :: Maybe Value, -- a second inbound landing mid-step (gated)
    -- NOTE: the fixture's @flush@ flag is intentionally NOT modelled — @normal@
    -- and @flush@ steps run identically here (the virtual clock drains poll loops
    -- in-line), so it is an ignored extra key (see module header).
    expectOutbound :: [Value],
    expectBackend :: [Value]
  }
  deriving (Generic, FromJSON)

data TrackerStep = TrackerStep
  { advanceMs :: Int,
    note :: Maybe Text,
    expectOutbound :: [Value],
    expectBackend :: [Value]
  }
  deriving (Generic, FromJSON)

-- ===========================================================================
-- Recording types + the two projections.
-- ===========================================================================

-- | One recorded outbound send (copy dropped; only what @structured()@ keeps).
data RecordedOut = RecordedOut
  { roKind :: Text,
    roTo :: Text,
    roMerchant :: Text,
    roButtons :: Maybe [Text], -- button/list-row DATA ids only
    roLink :: Maybe Text -- video link only
  }

-- | One recorded backend call with already-NORMALIZED args (harness.ts:231-256).
data RecordedCall = RecordedCall
  { rcMethod :: Text,
    rcArgs :: [Value]
  }

-- | @structured()@ (harness.ts:189-197): @{kind,to,merchant,buttons?,link?}@.
structuredOne :: RecordedOut -> Value
structuredOne r =
  object $
    ["kind" .= roKind r, "to" .= roTo r, "merchant" .= roMerchant r]
      ++ maybe [] (\bs -> ["buttons" .= bs]) (roButtons r)
      ++ maybe [] (\lnk -> ["link" .= lnk]) (roLink r)

-- | Backend call -> @{method,args}@ Value (matches @expectBackend@ entries).
callToValue :: RecordedCall -> Value
callToValue c = object ["method" .= rcMethod c, "args" .= rcArgs c]

-- | @projPlace()@ (harness.ts:223-226): drop everything but @{placeId,lat,lon}@.
projPlace :: BotPlace -> Value
projPlace p = object ["placeId" .= p.placeId, "lat" .= p.lat, "lon" .= p.lon]

-- ===========================================================================
-- The mutable World: recorders, the gate, the virtual clock, the built env.
-- ===========================================================================

data World = World
  { wOut :: IORef [RecordedOut],
    wCalls :: IORef [RecordedCall],
    wReached :: MVar (), -- signalled when a gated backend method is entered
    wRelease :: MVar (), -- released by the harness to un-stall the gate
    wClock :: IORef UTCTime,
    wEnv :: BotEnv IO,
    wDeps :: TrackerDeps IO
  }

-- | Recorder bundle the mock BackendHandle closes over.
data RB = RB
  { rbCalls :: IORef [RecordedCall],
    rbKnobs :: Map.Map Text Text, -- method -> behavior
    rbReached :: MVar (),
    rbRelease :: MVar (),
    rbFirstSeen :: IORef (Map.Map Text UTCTime), -- getBookingDetails progression anchor
    rbClock :: IORef UTCTime
  }

recordOut :: IORef [RecordedOut] -> RecordedOut -> IO ()
recordOut ref r = atomicModifyIORef' ref (\xs -> (xs ++ [r], ()))

recordCall :: IORef [RecordedCall] -> Text -> [Value] -> IO ()
recordCall ref m as = atomicModifyIORef' ref (\xs -> (xs ++ [RecordedCall m as], ()))

-- ===========================================================================
-- Knob logic (golden-fixtures.test.ts:74-78). Each mock method RECORDS first,
-- then routes through a guard so the failing/gated call is still captured in
-- order (exactly like the TS spy wrapper).
-- ===========================================================================

-- | Signal that a gated method was reached, then block until released.
gateWait :: RB -> IO ()
gateWait rb = do
  _ <- tryPutMVar (rbReached rb) ()
  _ <- readMVar (rbRelease rb)
  pure ()

-- | Guard a non-list method: 401 -> Left; gate -> stall then canned; else canned.
guard1 :: RB -> Text -> IO a -> IO (Either BotError a)
guard1 rb m act =
  case Map.lookup m (rbKnobs rb) of
    Just "401" -> pure (Left (BotError "Request failed 401"))
    Just "gate" -> gateWait rb >> (Right <$> act)
    _ -> Right <$> act

-- | Guard a list-returning method: adds the @empty@ behavior.
guardL :: RB -> Text -> IO [a] -> IO (Either BotError [a])
guardL rb m act =
  case Map.lookup m (rbKnobs rb) of
    Just "401" -> pure (Left (BotError "Request failed 401"))
    Just "empty" -> pure (Right [])
    Just "gate" -> gateWait rb >> (Right <$> act)
    _ -> Right <$> act

-- ===========================================================================
-- Canned NY mock data — transcribed VERBATIM from
-- ny-connectors/connectors/src/ny/mock-client.ts.
-- ===========================================================================

-- | TUMKUR center used by getPlaceDetails (mock-client.ts:26).
mkAddress :: Text -> BotAddress
mkAddress a =
  BotAddress
    { area = Just a,
      building = Nothing,
      city = Just "Tumakuru",
      country = Just "India",
      state = Just "Karnataka",
      street = Nothing
    }

cannedSavedLocations :: [BotSavedLocation]
cannedSavedLocations =
  [ BotSavedLocation {tag = "Home", lat = 13.3410, lon = 77.1010, area = Just "Sira Gate", building = Nothing, city = Just "Tumakuru", country = Nothing, state = Nothing, placeId = Just "mock-home"},
    BotSavedLocation {tag = "Work", lat = 13.3290, lon = 77.1230, area = Just "SIT College", building = Nothing, city = Just "Tumakuru", country = Nothing, state = Nothing, placeId = Just "mock-work"}
  ]

cannedPredictions :: Text -> [BotPrediction]
cannedPredictions q =
  [ BotPrediction {description = q <> " — Tumkur Bus Stand", placeId = "mock-place-1", distance = Just 1200},
    BotPrediction {description = q <> " — Amanikere Lake", placeId = "mock-place-2", distance = Just 1200},
    BotPrediction {description = q <> " — SIT College Road", placeId = "mock-place-3", distance = Just 1200}
  ]

cannedPlaceDetails :: Text -> BotPlace
cannedPlaceDetails pid =
  BotPlace {lat = 13.3379, lon = 77.1173, placeId = pid, address = mkAddress "Tumkur Bus Stand"}

cannedReverse :: LatLon -> BotPlace
cannedReverse ll =
  BotPlace {lat = ll.lat, lon = ll.lon, placeId = "mock-pin", address = mkAddress "Shared pin location"}

cannedEstimates :: [BotEstimate]
cannedEstimates =
  [ BotEstimate {estimateId = "mock-est-auto", estimatedFare = 52, serviceTierName = "Auto", vehicleVariant = "AUTO_RICKSHAW", totalFareRange = FareRange {minFare = 52, maxFare = 60}, estimatedPickupDuration = Just 180},
    BotEstimate {estimateId = "mock-est-cab", estimatedFare = 118, serviceTierName = "Cab (Non-AC)", vehicleVariant = "SEDAN", totalFareRange = FareRange {minFare = 118, maxFare = 140}, estimatedPickupDuration = Just 300}
  ]

-- | The default flexi quote (mock-client.ts:158) — also the value the gate knob
-- yields once released, so a gated search proceeds exactly as unstalled.
cannedFlexiQuotes :: [BotQuote]
cannedFlexiQuotes =
  [BotQuote {quoteId = "mock-flexi-quote-auto", serviceTierName = Just "Auto", estimatedFare = Just 40, vehicleVariant = Just "AUTO_RICKSHAW"}]

-- | The assigned driver as returned by getActiveBookings / the base for the
-- getBookingDetails progression (mock-client.ts:173-214).
rideBase :: Text -> BotBookingDetails
rideBase bid =
  BotBookingDetails
    { bookingId = bid,
      rideId = Just "mock-ride-001",
      bookingStatus = "TRIP_ASSIGNED",
      rideStatus = Just "NEW",
      rideStartTime = Nothing,
      rideEndTime = Nothing,
      driverArrivalTime = Nothing,
      driverName = Just "Ravi Kumar",
      driverNumber = Just "9998887776",
      vehicleNumber = Just "KA06 AB 1234",
      rideOtp = Just "4321",
      endOtp = Nothing,
      driverRating = Just 4.9,
      etaMinutes = Just 3,
      computedFare = Nothing,
      chargeableDistanceM = Nothing
    }

cannedActiveBooking :: BotBookingDetails
cannedActiveBooking = rideBase "mock-booking-001"

-- | getBookingDetails progression by VIRTUAL elapsed-since-first-poll
-- (mock-client.ts:194-238): 0s assigned -> 6s arrived -> 12s started -> 20s ended.
bookingProgression :: RB -> Text -> IO (Maybe BotBookingDetails)
bookingProgression rb bid = do
  nowT <- readIORef (rbClock rb)
  firstT <- atomicModifyIORef' (rbFirstSeen rb) $ \m ->
    case Map.lookup bid m of
      Just t -> (m, t)
      Nothing -> (Map.insert bid nowT m, nowT)
  let elapsed = realToFrac (diffUTCTime nowT firstT) :: Double
      arrivalT = addUTCTime 6 firstT
      startT = addUTCTime 12 firstT
      endT = addUTCTime 20 firstT
      b = rideBase bid
      d
        | elapsed >= 20 =
          b
            { bookingStatus = "COMPLETED",
              rideStatus = Just "COMPLETED",
              endOtp = Just "8765",
              driverArrivalTime = Just arrivalT,
              rideStartTime = Just startT,
              rideEndTime = Just endT,
              computedFare = Just 57,
              chargeableDistanceM = Just 4200
            }
        | elapsed >= 12 =
          b
            { rideStatus = Just "INPROGRESS",
              endOtp = Just "8765",
              driverArrivalTime = Just arrivalT,
              rideStartTime = Just startT
            }
        | elapsed >= 6 = b {driverArrivalTime = Just arrivalT}
        | otherwise = b
  pure (Just d)

-- ===========================================================================
-- Mock handles (all over IORefs). Each backend method records its NORMALIZED
-- args, then routes through the knob guard.
-- ===========================================================================

mkBackend :: RB -> BackendHandle IO
mkBackend rb =
  BackendHandle
    { authenticate = \phone -> do
        recordCall (rbCalls rb) "authenticate" [String phone]
        case Map.lookup "authenticate" (rbKnobs rb) of
          Just "401" -> pure (Left (BotError "Request failed 401"))
          Just "gate" -> gateWait rb >> pure (authResult phone)
          _ -> pure (authResult phone),
      getSavedLocations = \_a -> do
        recordCall (rbCalls rb) "getSavedLocations" []
        guardL rb "getSavedLocations" (pure cannedSavedLocations),
      searchPlaces = \_a q _near -> do
        recordCall (rbCalls rb) "searchPlaces" [String q]
        guardL rb "searchPlaces" (pure (cannedPredictions q)),
      getPlaceDetails = \_a pid -> do
        recordCall (rbCalls rb) "getPlaceDetails" [String pid]
        guard1 rb "getPlaceDetails" (pure (cannedPlaceDetails pid)),
      reverseGeocode = \_a ll -> do
        recordCall (rbCalls rb) "reverseGeocode" [toJSON ll.lat, toJSON ll.lon]
        guard1 rb "reverseGeocode" (pure (cannedReverse ll)),
      searchRide = \_a o d -> do
        recordCall (rbCalls rb) "searchRide" [projPlace o, projPlace d]
        guard1 rb "searchRide" (pure "mock-search-1"),
      getEstimates = \_a sid -> do
        recordCall (rbCalls rb) "getEstimates" [String sid]
        guardL rb "getEstimates" (pure cannedEstimates),
      searchFlexi = \_a o -> do
        recordCall (rbCalls rb) "searchFlexi" [projPlace o]
        guard1 rb "searchFlexi" (pure "mock-flexi-search-1"),
      getFlexiQuotes = \_a sid -> do
        recordCall (rbCalls rb) "getFlexiQuotes" [String sid]
        guardL rb "getFlexiQuotes" (pure cannedFlexiQuotes),
      confirmQuote = \_a qid -> do
        recordCall (rbCalls rb) "confirmQuote" [String qid]
        guard1 rb "confirmQuote" (pure "mock-booking-001"),
      selectEstimate = \_a eid -> do
        recordCall (rbCalls rb) "selectEstimate" [String eid]
        guard1 rb "selectEstimate" (pure ()),
      getBookingDetails = \_a bid -> do
        recordCall (rbCalls rb) "getBookingDetails" [String bid]
        guard1 rb "getBookingDetails" (bookingProgression rb bid),
      getActiveBookings = \_a _after -> do
        recordCall (rbCalls rb) "getActiveBookings" []
        guardL rb "getActiveBookings" (pure [cannedActiveBooking]),
      triggerSOS = \_a rid _loc -> do
        recordCall (rbCalls rb) "triggerSOS" [String rid]
        guard1 rb "triggerSOS" (pure "mock-sos-1"),
      markRideAsSafe = \_a sid -> do
        recordCall (rbCalls rb) "markRideAsSafe" [String sid]
        guard1 rb "markRideAsSafe" (pure ()),
      cancelRide = \_a bid st -> do
        recordCall (rbCalls rb) "cancelRide" [String bid, String st]
        guard1 rb "cancelRide" (pure ()),
      updateProfile = \_a upd -> do
        recordCall (rbCalls rb) "updateProfile" [toJSON upd]
        guard1 rb "updateProfile" (pure ())
    }
  where
    -- Any phone authenticates as a returning user EXCEPT one containing "00000"
    -- (the OTP/new-user path); no golden uses such a phone (mock-client.ts:52-62).
    authResult p =
      if "00000" `T.isInfixOf` p
        then Left (BotError "Person not found")
        else Right (BotAuth "mock-person-1")

-- | Recording WaSender. Merchant label is closed over (one merchant per fixture).
-- A buttons/list send is recorded identically (kind="buttons", data-ids only).
mkSender :: IORef [RecordedOut] -> Text -> WaSender IO
mkSender outRef label =
  WaSender
    { sendText = \to _body ->
        recordOut outRef (RecordedOut "text" to label Nothing Nothing) >> pure True,
      sendButtons = \to _body btns ->
        recordOut outRef (RecordedOut "buttons" to label (Just (map (\b -> b.btnId) btns)) Nothing) >> pure True,
      sendLocationRequest = \to _body ->
        recordOut outRef (RecordedOut "location_request" to label Nothing Nothing) >> pure True,
      sendVideo = \to lnk _cap ->
        recordOut outRef (RecordedOut "video" to label Nothing (Just lnk)) >> pure True
    }

-- | In-memory SessionStore (session/manager.ts). @resolveSession@ create-or-refresh
-- (++messageCount); @saveContext@ is a silent no-op if the key is absent. TTL is
-- omitted: virtual advance per fixture (<=~20s) never reaches the 1800s TTL.
mkSessions :: IORef UTCTime -> IORef (Map.Map Text Session) -> SessionStore IO
mkSessions clockRef sessRef =
  SessionStore
    { resolveSession = \key -> do
        nowT <- readIORef clockRef
        atomicModifyIORef' sessRef $ \m ->
          case Map.lookup key m of
            Just s ->
              let s' = s {lastActiveAt = nowT, messageCount = s.messageCount + 1}
               in (Map.insert key s' m, s')
            Nothing ->
              let s = Session {sessionId = key, source = "whatsapp", userId = key, createdAt = nowT, lastActiveAt = nowT, messageCount = 1, metadata = initialContext}
               in (Map.insert key s m, s),
      getContext = \key -> do
        m <- readIORef sessRef
        pure ((\s -> s.metadata) <$> Map.lookup key m),
      saveContext = \key ctx ->
        atomicModifyIORef' sessRef $ \m ->
          case Map.lookup key m of
            Just s -> (Map.insert key (s {metadata = ctx}) m, ())
            Nothing -> (m, ())
    }

-- | In-memory PersonStore (session/token-store.ts). Starts EMPTY: the golden
-- runner does not pre-seed auth, so the first inbound triggers authenticate.
mkPersons :: IORef (Map.Map Text StoredPerson) -> IORef (Set.Set Text) -> PersonStore IO
mkPersons personRef introRef =
  PersonStore
    { getPerson = \k -> Map.lookup k <$> readIORef personRef,
      setPerson = \k p -> atomicModifyIORef' personRef (\m -> (Map.insert k p m, ())),
      getIntroSent = \k -> Set.member k <$> readIORef introRef,
      setIntroSent = \k -> atomicModifyIORef' introRef (\s -> (Set.insert k s, ()))
    }

-- | In-memory RideRegistry (session/ride-registry.ts). @claimStage@ is an atomic
-- claim-once (Redis @SET NX@ analogue); @releaseStage@ undoes it.
mkRegistry :: IORef (Map.Map Text RegisteredRide) -> IORef (Set.Set Text) -> RideRegistry IO
mkRegistry ridesRef claimedRef =
  RideRegistry
    { registerRide = \r -> atomicModifyIORef' ridesRef (\m -> (Map.insert r.bookingId r m, ())),
      getRide = \bid -> Map.lookup bid <$> readIORef ridesRef,
      removeRide = \bid -> atomicModifyIORef' ridesRef (\m -> (Map.delete bid m, ())),
      updateRide = \r -> atomicModifyIORef' ridesRef (\m -> (Map.insert r.bookingId r m, ())),
      claimStage = \bid stage ->
        atomicModifyIORef' claimedRef $ \s ->
          let k = bid <> "\SOH" <> stage
           in if Set.member k s then (s, False) else (Set.insert k s, True),
      releaseStage = \bid stage ->
        atomicModifyIORef' claimedRef (\s -> (Set.delete (bid <> "\SOH" <> stage) s, ())),
      listRides = Map.elems <$> readIORef ridesRef,
      listByUser = \uk -> filter (\r -> r.userKey == uk) . Map.elems <$> readIORef ridesRef,
      hasActiveRide = \uk -> any (\r -> r.userKey == uk) . Map.elems <$> readIORef ridesRef
    }

-- | Virtual clock. @sleepMs@ ADVANCES it (n ms) and does nothing else, so poll
-- loops drain synchronously and the mock progression reads virtual time.
mkClock :: IORef UTCTime -> Clock IO
mkClock clockRef =
  Clock
    { now = readIORef clockRef,
      sleepMs = \n -> modifyIORef' clockRef (addUTCTime (fromIntegral n / 1000))
    }

-- ===========================================================================
-- Fixture merchant registry + BotConfig (test/setup.ts values).
-- ===========================================================================

-- | phone_number_id -> MerchantCtx. "pn_flexi" -> FLEXI (flexi-only),
-- "pn_reg" -> REG (regular-only). Shared flexi defaults from test/setup.ts:
-- service area "Tumkur" radius 15km, rental 10km/60min, intro video + support.
-- (@merchantLabel@ is what @structured()@ projects as @merchant@.)
fixtureMerchant :: Text -> MerchantCtx
fixtureMerchant pn
  | pn == "pn_reg" =
    base {merchantLabel = "REG", rideMode = RideModeRegular, flexiEnabled = False, regularEnabled = True}
  | otherwise =
    base {merchantLabel = "FLEXI", rideMode = RideModeFlexi, flexiEnabled = True, regularEnabled = False}
  where
    base =
      MerchantCtx
        { merchantLabel = "FLEXI",
          rideMode = RideModeFlexi,
          flexiEnabled = True,
          regularEnabled = False,
          flexiBaseFare = Nothing, -- FLEXI_BASE_FARE unset in test/setup.ts
          flexiPerKm = Nothing, -- FLEXI_PER_KM unset in test/setup.ts
          flexiServiceArea = Just "Tumkur",
          flexiServiceRadiusKm = Just 15,
          flexiRentalDistanceM = 10000,
          flexiRentalDurationS = 3600,
          flexiIntroVideoUrl = Just "https://videos.example/intro.mp4",
          flexiSupportPhone = Just "+91 90000 12345",
          nyTrackingUrl = "https://www.nammayatri.in/u?vp=shareRide&rideId={rideId}"
        }

-- | Poll constants (plan L11 / config.ts). Allowlist = test/setup.ts ALLOWED_PHONES
-- (both golden senders are allowlisted; no golden exercises the gate).
fixtureConfig :: MerchantCtx -> BotConfig
fixtureConfig m =
  BotConfig
    { allowedPhones = ["9361176218", "9812345678", "7411122233", "9100000999"],
      merchant = m,
      flexiQuotePollAttempts = 10,
      flexiQuotePollIntervalMs = 2000,
      regularEstimatePollAttempts = 6,
      regularEstimatePollIntervalMs = 2000,
      driverPollAttempts = 90,
      driverPollIntervalMs = 2000,
      driverPollNotifyEvery = 15
    }

-- ===========================================================================
-- World builder.
-- ===========================================================================

makeWorld :: MerchantCtx -> Map.Map Text Text -> UTCTime -> IO World
makeWorld merchantCtx theKnobs t0 = do
  outRef <- newIORef []
  callsRef <- newIORef []
  clockRef <- newIORef t0
  firstSeenRef <- newIORef Map.empty
  sessRef <- newIORef Map.empty
  personRef <- newIORef Map.empty
  introRef <- newIORef Set.empty
  ridesRef <- newIORef Map.empty
  claimedRef <- newIORef Set.empty
  reached <- newEmptyMVar
  release <- newEmptyMVar
  let rb =
        RB
          { rbCalls = callsRef,
            rbKnobs = theKnobs,
            rbReached = reached,
            rbRelease = release,
            rbFirstSeen = firstSeenRef,
            rbClock = clockRef
          }
      backendH = mkBackend rb
      senderH = mkSender outRef merchantCtx.merchantLabel
      sessionsH = mkSessions clockRef sessRef
      personsH = mkPersons personRef introRef
      registryH = mkRegistry ridesRef claimedRef
      clockH = mkClock clockRef
      botEnv =
        BotEnv
          { backend = backendH,
            sender = senderH,
            sessions = sessionsH,
            persons = personsH,
            registry = registryH,
            clock = clockH,
            cfg = fixtureConfig merchantCtx
          }
      -- ASSUMED TrackerDeps shape (reconcile with Tracker.hs — see module note).
      deps =
        TrackerDeps
          { tdRegistry = registryH,
            tdGetBookingDetails = backendH.getBookingDetails,
            tdSender = senderH,
            tdSessions = sessionsH,
            tdClock = clockH
          }
  pure
    World
      { wOut = outRef,
        wCalls = callsRef,
        wReached = reached,
        wRelease = release,
        wClock = clockRef,
        wEnv = botEnv,
        wDeps = deps
      }

-- ===========================================================================
-- Inbound path: raw envelope -> real kernel decoder -> real parseInbound -> head.
-- ===========================================================================

firstEvent :: Value -> IO InboundEvent
firstEvent v =
  case Meta.decodeWebhookEnvelope (Meta.RawByteString (encode v)) of
    Left err -> assertFailure ("webhook decode failed: " ++ err)
    Right envlp ->
      case parseInbound envlp of
        ((_pn, ev) : _) -> pure ev
        [] -> assertFailure "parseInbound produced no events"

-- ===========================================================================
-- Step runners.
-- ===========================================================================

snapshotLen :: World -> IO (Int, Int)
snapshotLen w = do
  o <- readIORef (wOut w)
  c <- readIORef (wCalls w)
  pure (length o, length c)

sliceSince :: World -> (Int, Int) -> IO ([RecordedOut], [RecordedCall])
sliceSince w (lo, lc) = do
  o <- readIORef (wOut w)
  c <- readIORef (wCalls w)
  pure (drop lo o, drop lc c)

ctxLabel :: String -> Int -> Maybe Text -> String
ctxLabel lbl i mnote = lbl <> "[" <> show i <> "]" <> maybe "" (\n -> " " <> T.unpack n) mnote

-- | @concurrent@ step (golden-fixtures.test.ts:88-97), reproduced as a scripted
-- MVar interleave: fork the inbound handler (it stalls at the gated backend
-- call), wait until it is blocked, run the concurrent envelope to completion,
-- release the gate, await the fork. The MERGED ordered backend list is asserted.
runConcurrent :: World -> MerchantCtx -> InboundEvent -> InboundEvent -> IO ()
runConcurrent w merchantCtx ev cev = do
  let botEnv = wEnv w
  done <- newEmptyMVar
  _ <- botEnv.sessions.resolveSession (scopedSessionId merchantCtx ev)
  _ <- forkIO (handleMessage botEnv ev `finally` putMVar done ())
  takeMVar (wReached w) -- forked handler has recorded up to the gate and is blocked
  _ <- botEnv.sessions.resolveSession (scopedSessionId merchantCtx cev)
  handleMessage botEnv cev
  putMVar (wRelease w) () -- un-stall the gate; the fork resumes and finishes
  takeMVar done

runStep :: World -> MerchantCtx -> Int -> Step -> Assertion
runStep w merchantCtx i step = do
  before <- snapshotLen w
  let botEnv = wEnv w
  case step.concurrent of
    Just cinb -> do
      ev <- firstEvent step.inbound
      cev <- firstEvent cinb
      runConcurrent w merchantCtx ev cev
    Nothing -> do
      -- normal AND flush both run to completion (virtual clock drains in-line)
      ev <- firstEvent step.inbound
      _ <- botEnv.sessions.resolveSession (scopedSessionId merchantCtx ev)
      handleMessage botEnv ev
  (gotOut, gotBack) <- sliceSince w before
  let ctx = ctxLabel "step" i step.note
  assertEqual (ctx ++ " outbound") step.expectOutbound (map structuredOne gotOut)
  assertEqual (ctx ++ " backend") step.expectBackend (map callToValue gotBack)

runTrackerStep :: World -> Int -> TrackerStep -> Assertion
runTrackerStep w i ts = do
  before <- snapshotLen w
  modifyIORef' (wClock w) (addUTCTime (fromIntegral ts.advanceMs / 1000))
  trackerTick (wDeps w)
  (gotOut, gotBack) <- sliceSince w before
  let ctx = ctxLabel "trackerStep" i ts.note
  assertEqual (ctx ++ " outbound") ts.expectOutbound (map structuredOne gotOut)
  assertEqual (ctx ++ " backend") ts.expectBackend (map callToValue gotBack)

runFixture :: Fixture -> Assertion
runFixture fx = do
  let merchantCtx = fixtureMerchant fx.env.merchant
      theKnobs = Map.fromList [(k.method, k.behavior) | k <- fromMaybe [] fx.knobs]
  w <- makeWorld merchantCtx theKnobs fx.env.systemTime
  mapM_ (\(i, s) -> runStep w merchantCtx i s) (zip [0 :: Int ..] fx.steps)
  mapM_ (\(i, t) -> runTrackerStep w i t) (zip [0 :: Int ..] (fromMaybe [] fx.trackerSteps))

-- ===========================================================================
-- Fixtures embedded at compile time (file-embed; no Paths_/getDataFileName).
-- ===========================================================================

goldenFiles :: [(FilePath, BS.ByteString)]
goldenFiles = $(makeRelativeToProject "test/resources/golden" >>= embedDir)

mkFixtureTest :: FilePath -> BS.ByteString -> TestTree
mkFixtureTest name bs =
  case eitherDecodeStrict' bs of
    Left err -> testCase name (assertFailure ("fixture decode failed (" ++ name ++ "): " ++ err))
    Right fx -> testCase (T.unpack fx.scenario) (runFixture fx)

-- | One Tasty test per fixture (each covers its steps + trackerSteps), sorted
-- by file name (matches the TS runner's @readdirSync(...).sort()@).
goldenTests :: [TestTree]
goldenTests =
  [ mkFixtureTest name bs
    | (name, bs) <- L.sortOn fst goldenFiles,
      L.isSuffixOf ".json" name
  ]

-- ===========================================================================
-- English copy spot-checks — guard the En table against wording drift
-- (transcribed from ny-connectors/connectors/src/i18n/en.ts).
-- ===========================================================================

copyChecks :: TestTree
copyChecks =
  testGroup "en copy spot-checks" $
    map (\(lbl, expected, actual) -> testCase lbl (assertEqual lbl expected actual)) enCopyCases

enCopyCases :: [(String, Text, Text)]
enCopyCases =
  [ ("welcome", "🙏 Namaskara! I'm your Namma Yatri assistant\n\nReady to book an auto?", en.welcome),
    ("flexiFinding", "🛺 Finding an auto near you…", en.flexiFinding),
    ("flexiCancelSearch", "❌ Cancel search", en.flexiCancelSearch),
    ("flexiNoAuto", "😔 No auto available near you right now. Please try again.", en.flexiNoAuto),
    ("flexiTryAgain", "🔁 Try again", en.flexiTryAgain),
    ("flexiBookAnother", "🛺 Book another", en.flexiBookAnother),
    ("rideTypeFlexi", "🛺 Quick Ride", en.rideTypeFlexi),
    ("rideTypeRegular", "🚗 Ride with destination", en.rideTypeRegular),
    ("regularConfirmButton", "✅ Book auto", en.regularConfirmButton),
    ("pickupConfirmButton", "✅ Confirm pickup", en.pickupConfirmButton)
  ]
