{-# LANGUAGE DeriveAnyClass #-}

-- | The ports (handle interfaces) the pure engine depends on. Each is a record
-- of @m@-actions so the golden suite can inject recording/mock implementations
-- and rider-app can inject in-process 'Flow' adapters — with the same engine
-- code. Backend calls return @Either 'BotError'@ so the engine can reproduce the
-- TS per-call try/catch exactly (@engine.ts@).
--
-- Argument shapes track how the engine calls each dependency in @engine.ts@;
-- they are finalized against the engine port. Merchant identity is closed over
-- by each adapter instance (prod: the resolved merchant; test: the scenario's
-- single merchant), so these methods take only per-call data.
module WhatsappBot.Handles where

import Kernel.Prelude
import WhatsappBot.I18n.Types (SupportedLanguage)
import WhatsappBot.Types

-- | NY backend calls (@ny/client.ts@ methods the engine uses). @BotAuth@ is the
-- resolved rider handle; the adapter maps it to @(Id Person, Id Merchant)@.
data BackendHandle m = BackendHandle
  { -- | 10-digit phone -> resolved rider (prod: find-or-create, no OTP; D2/D3).
    authenticate :: Text -> m (Either BotError BotAuth),
    getSavedLocations :: BotAuth -> m (Either BotError [BotSavedLocation]),
    -- | query text, bias point (origin) -> predictions.
    searchPlaces :: BotAuth -> Text -> Maybe LatLon -> m (Either BotError [BotPrediction]),
    -- | placeId -> resolved place.
    getPlaceDetails :: BotAuth -> Text -> m (Either BotError BotPlace),
    reverseGeocode :: BotAuth -> LatLon -> m (Either BotError BotPlace),
    -- | origin, destination -> searchId (ONE_WAY).
    searchRide :: BotAuth -> BotPlace -> BotPlace -> m (Either BotError Text),
    -- | searchId -> estimates (poll-read; @[]@ until on_search lands).
    getEstimates :: BotAuth -> Text -> m (Either BotError [BotEstimate]),
    -- | origin -> searchId (RENTAL/flexi).
    searchFlexi :: BotAuth -> BotPlace -> m (Either BotError Text),
    -- | searchId -> quotes (poll-read).
    getFlexiQuotes :: BotAuth -> Text -> m (Either BotError [BotQuote]),
    -- | quoteId -> bookingId.
    confirmQuote :: BotAuth -> Text -> m (Either BotError Text),
    -- | estimateId -> () (dispatch; result found by polling active bookings).
    selectEstimate :: BotAuth -> Text -> m (Either BotError ()),
    -- | bookingId -> booking (Nothing when the read fails without list-fallback).
    getBookingDetails :: BotAuth -> Text -> m (Either BotError (Maybe BotBookingDetails)),
    -- | createdAfter filter -> active bookings.
    getActiveBookings :: BotAuth -> Maybe UTCTime -> m (Either BotError [BotBookingDetails]),
    -- | rideId, customer location -> sosId.
    triggerSOS :: BotAuth -> Text -> Maybe LatLon -> m (Either BotError Text),
    markRideAsSafe :: BotAuth -> Text -> m (Either BotError ()),
    -- | bookingId, ride status (drives reasonStage) -> ().
    cancelRide :: BotAuth -> Text -> Text -> m (Either BotError ()),
    updateProfile :: BotAuth -> BotProfileUpdate -> m (Either BotError ())
  }

-- | Outbound WhatsApp surface (the @Connector@ methods, @whatsapp.ts@). The
-- ≤3-no-desc -> reply-buttons / else -> list decision and id/title/desc
-- truncation live in the adapter. @to@ is the recipient; merchant is closed
-- over. Returns delivered? and never throws (@whatsapp.ts:204-231@).
data WaSender m = WaSender
  { sendText :: Text -> Text -> m Bool, -- to, body
    sendButtons :: Text -> Text -> [OutButton] -> m Bool, -- to, body, buttons
    sendLocationRequest :: Text -> Text -> m Bool, -- to, body
    sendVideo :: Text -> Text -> Maybe Text -> m Bool -- to, link, caption
  }

-- | Conversation-context store (@session/manager.ts@). Keyed by the scoped
-- @merchantId:senderId@. @saveContext@ is a silent no-op when the session key is
-- absent/expired (@manager.ts:63-64@), so callers @resolveSession@ first.
data SessionStore m = SessionStore
  { resolveSession :: Text -> m Session, -- create-or-refresh, ++messageCount
    getContext :: Text -> m (Maybe FlowContext),
    saveContext :: Text -> FlowContext -> m ()
  }

-- | Durable per-user record (@session/token-store.ts@): the resolved person
-- handle + language, plus the one-time intro-sent flag. No TTL.
data StoredPerson = StoredPerson
  { personId :: Text,
    language :: Maybe SupportedLanguage
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data PersonStore m = PersonStore
  { getPerson :: Text -> m (Maybe StoredPerson), -- by userKey
    setPerson :: Text -> StoredPerson -> m (),
    getIntroSent :: Text -> m Bool,
    setIntroSent :: Text -> m ()
  }

-- | An entry in the durable ride index the tracker watches
-- (@session/ride-registry.ts@). Carries just enough to poll the booking and push
-- updates to the right user. (Fields finalized against ride-registry.ts.)
data RegisteredRide = RegisteredRide
  { bookingId :: Text,
    userKey :: Text, -- token-store key (source:merchant:sender)
    sessionUserId :: Text, -- scoped session id (merchantId:senderId)
    toPhone :: Text, -- WhatsApp recipient
    merchantLabel :: Text,
    personId :: Text, -- opaque auth handle for backend polls
    rideType :: RideType,
    language :: Maybe SupportedLanguage,
    lastStage :: Maybe Text -- last pushed stage (confirmed/assigned/arrived/started)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Durable booking index for the background tracker (@ride-registry.ts@).
-- @claimStage@ is an atomic claim-once (Redis @SET NX@) returning whether THIS
-- caller won; @releaseStage@ undoes it on a failed send so the next tick retries.
data RideRegistry m = RideRegistry
  { registerRide :: RegisteredRide -> m (),
    getRide :: Text -> m (Maybe RegisteredRide), -- by bookingId
    removeRide :: Text -> m (),
    updateRide :: RegisteredRide -> m (), -- persist stage advance
    claimStage :: Text -> Text -> m Bool, -- bookingId, stage -> won?
    releaseStage :: Text -> Text -> m (),
    listRides :: m [RegisteredRide],
    listByUser :: Text -> m [RegisteredRide], -- by userKey
    hasActiveRide :: Text -> m Bool -- by userKey
  }

-- | Time + delay, so the engine's poll loops are testable under a virtual clock.
-- The engine NEVER touches real time/delay directly — only via this handle.
data Clock m = Clock
  { now :: m UTCTime,
    sleepMs :: Int -> m () -- advances the virtual clock in tests
  }
