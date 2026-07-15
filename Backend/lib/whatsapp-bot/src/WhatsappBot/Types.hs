{-# LANGUAGE DeriveAnyClass #-}

-- | Port-local domain types for the WhatsApp booking engine. These depend ONLY
-- on mobility-core (Kernel.Prelude) — never on rider-app. The prod adapters and
-- the golden mock both construct these as Haskell values (they are NOT decoded
-- from the TS wire JSON), so field names are chosen for clarity, not TS parity.
--
-- Sources: @states.ts@ (FlowState/FlowContext), @ny/client.ts:103-151@ (the NY*
-- DTOs), @config.ts:6-27@ (MerchantConfig), @connectors/whatsapp.ts@ (in/out).
module WhatsappBot.Types where

import Kernel.Prelude
import WhatsappBot.I18n.Types (SupportedLanguage)

-- | @{ lat, lon }@ — a geographic point.
data LatLon = LatLon {lat :: Double, lon :: Double}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Conversation state (@states.ts:4-17@) MINUS @AWAITING_OTP@ (silent
-- onboarding, divergence D2). Serialized to the session store as constructor
-- names — an internal representation, never shared with the TS connector.
data FlowState
  = Idle
  | AwaitingPickup
  | ConfirmingPickup
  | FlexiSearching
  | AwaitingRegularDrop
  | ConfirmingRegularDrop
  | ConfirmingRegularFare
  | RegularSearching
  | Tracking
  | ConfirmingSos
  | ConfirmingMarkSafe
  | ChoosingLanguage
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Deferred action resumed after auth (@states.ts:34@).
data PendingAction = PendingStatus | PendingBook
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Chosen ride type for this booking (@states.ts:35@).
data RideType = Flexi | Regular
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Which ride types a merchant offers (@config.ts:2@).
data RideMode = RideModeFlexi | RideModeRegular | RideModeBoth
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A disambiguation option for a typed drop (@states.ts:27@).
data DestinationOption = DestinationOption
  { description :: Text,
    placeId :: Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Structured address (@NYPlaceDetails.address@, @client.ts:113-120@).
data BotAddress = BotAddress
  { area :: Maybe Text,
    building :: Maybe Text,
    city :: Maybe Text,
    country :: Maybe Text,
    state :: Maybe Text,
    street :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A resolved place (@NYPlaceDetails@, @client.ts:109-121@).
data BotPlace = BotPlace
  { lat :: Double,
    lon :: Double,
    placeId :: Text,
    address :: BotAddress
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | An autocomplete prediction (@NYPlace@, @client.ts:103-107@).
data BotPrediction = BotPrediction
  { description :: Text,
    placeId :: Text,
    distance :: Maybe Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Fare band (@NYEstimate.totalFareRange@, @client.ts:128@).
data FareRange = FareRange {minFare :: Double, maxFare :: Double}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A one-way estimate (@NYEstimate@, @client.ts:123-130@).
data BotEstimate = BotEstimate
  { estimateId :: Text,
    estimatedFare :: Double,
    serviceTierName :: Text,
    vehicleVariant :: Text,
    totalFareRange :: FareRange,
    estimatedPickupDuration :: Maybe Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A flexi/rental quote (@NYFlexiQuote@, @client.ts:134-139@). @quoteId@ is
-- what we confirm.
data BotQuote = BotQuote
  { quoteId :: Text,
    serviceTierName :: Maybe Text,
    estimatedFare :: Maybe Double,
    vehicleVariant :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A saved rider location (@NYSavedLocation@, @client.ts:141-151@).
data BotSavedLocation = BotSavedLocation
  { tag :: Text,
    lat :: Double,
    lon :: Double,
    area :: Maybe Text,
    building :: Maybe Text,
    city :: Maybe Text,
    country :: Maybe Text,
    state :: Maybe Text,
    placeId :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Booking detail the engine + tracker read (@getBookingDetails@ result; fields
-- consumed at @engine.ts:1212-1262,396@ and @flexi-messages.ts:48-128@). The
-- adapter flattens rider-app's booking + @rideList[0]@ into this, applying the
-- TS @ride?.x || booking?.x@ fallbacks. @rideStatus@/@ride*Time@ drive
-- @classifyStage@ (their PRESENCE is what matters). All ride-specifics optional
-- (absent until the ride reaches @TRIP_ASSIGNED@).
data BotBookingDetails = BotBookingDetails
  { bookingId :: Text,
    rideId :: Maybe Text, -- rideList[0].id (SOS target + tracking link; absent until TRIP_ASSIGNED)
    bookingStatus :: Text, -- booking.status
    rideStatus :: Maybe Text, -- rideList[0].status
    rideStartTime :: Maybe UTCTime, -- presence => started
    rideEndTime :: Maybe UTCTime, -- presence => completed
    driverArrivalTime :: Maybe UTCTime, -- presence => arrived
    driverName :: Maybe Text,
    driverNumber :: Maybe Text, -- best dial source (driverNumber || merchantExoPhone)
    vehicleNumber :: Maybe Text,
    rideOtp :: Maybe Text, -- start OTP
    endOtp :: Maybe Text, -- rental end OTP (gates the End-ride button)
    driverRating :: Maybe Double,
    etaMinutes :: Maybe Int,
    computedFare :: Maybe Double,
    chargeableDistanceM :: Maybe Double -- meters (builder converts to km, 1dp)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Resolved rider identity returned by @authenticate@ (D3: opaque handle, no
-- token). The prod adapter carries a rider-app @(Id Person, Id Merchant, Id
-- MerchantOperatingCity)@ behind @personId@; the engine treats it as opaque.
newtype BotAuth = BotAuth {personId :: Text}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Profile fields the engine updates on first registration (@engine.ts:1443@).
newtype BotProfileUpdate = BotProfileUpdate {language :: Maybe Text}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Per-merchant config the engine reads (the engine-relevant subset of
-- @config.ts@ MerchantConfig + the two rental globals). @merchantLabel@ is the
-- value projected as @merchant@ in outbound records (e.g. "FLEXI"/"REG").
data MerchantCtx = MerchantCtx
  { merchantLabel :: Text,
    rideMode :: RideMode,
    flexiEnabled :: Bool,
    regularEnabled :: Bool,
    flexiBaseFare :: Maybe Double,
    flexiPerKm :: Maybe Double,
    flexiServiceArea :: Maybe Text,
    flexiServiceRadiusKm :: Maybe Double,
    flexiRentalDistanceM :: Int,
    flexiRentalDurationS :: Int,
    flexiIntroVideoUrl :: Maybe Text,
    flexiSupportPhone :: Maybe Text,
    nyTrackingUrl :: Text -- ride-share tracking URL template ({rideId} placeholder)
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Conversation context stored as @session.metadata@ (@states.ts:19-44@),
-- MINUS @nyToken@/@authId@ (D2/D3). @personId@ is the opaque auth handle;
-- @selectStartedAt@ anchors the listV2 @createdAfter@ filter.
data FlowContext = FlowContext
  { state :: FlowState,
    personId :: Maybe Text,
    phone :: Maybe Text,
    savedLocations :: Maybe [BotSavedLocation],
    origin :: Maybe BotPlace,
    destination :: Maybe BotPlace,
    destinationOptions :: Maybe [DestinationOption],
    activeBookingId :: Maybe Text,
    selectStartedAt :: Maybe UTCTime,
    cancelRequested :: Maybe Bool,
    sosId :: Maybe Text,
    language :: Maybe SupportedLanguage,
    pendingAction :: Maybe PendingAction,
    rideType :: Maybe RideType,
    flexiSearchId :: Maybe Text,
    flexiQuoteId :: Maybe Text,
    flexiBookingId :: Maybe Text,
    regularSearchId :: Maybe Text,
    regularEstimateId :: Maybe Text,
    regularFare :: Maybe Double
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

initialContext :: FlowContext
initialContext =
  FlowContext
    { state = Idle,
      personId = Nothing,
      phone = Nothing,
      savedLocations = Nothing,
      origin = Nothing,
      destination = Nothing,
      destinationOptions = Nothing,
      activeBookingId = Nothing,
      selectStartedAt = Nothing,
      cancelRequested = Nothing,
      sosId = Nothing,
      language = Nothing,
      pendingAction = Nothing,
      rideType = Nothing,
      flexiSearchId = Nothing,
      flexiQuoteId = Nothing,
      flexiBookingId = Nothing,
      regularSearchId = Nothing,
      regularEstimateId = Nothing,
      regularFare = Nothing
    }

-- | Session envelope (@session/manager.ts:6-14@). Timestamps as 'UTCTime';
-- @userId@ is the scoped @merchantId:senderId@. Never inspected by the golden
-- projection (sessionId is not projected), so representations are internal.
data Session = Session
  { sessionId :: Text,
    source :: Text,
    userId :: Text,
    createdAt :: UTCTime,
    lastActiveAt :: UTCTime,
    messageCount :: Int,
    metadata :: FlowContext
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | The parsed inbound message the engine consumes (@whatsapp.ts:53-113@). The
-- merchant is resolved separately (by phone_number_id) and passed alongside.
data InboundEvent = InboundEvent
  { fromPhone :: Text, -- message.from (also the session key + allowlist source)
    waId :: Maybe Text, -- contact.wa_id
    profileName :: Maybe Text, -- contact.profile.name
    messageId :: Text, -- message.id (dedupe key at the webhook layer)
    kind :: InboundKind
  }
  deriving (Show, Eq, Generic)

-- | Parsed message content. Non-text/interactive/location types are dropped
-- upstream (parse returns Nothing), so the engine never sees them.
data InboundKind
  = InText Text -- text.body
  | InButtonTap Text -- interactive button_reply.id / list_reply.id
  | InLocationPin Double Double (Maybe Text) (Maybe Text) -- lat lon name address
  deriving (Show, Eq, Generic)

-- | One interactive button/row the engine emits. The sender adapter applies the
-- ≤3-no-desc -> reply-buttons / else -> list decision (@whatsapp.ts:124-175@)
-- and the id/title/desc truncation.
data OutButton = OutButton
  { btnId :: Text,
    btnTitle :: Text,
    btnDesc :: Maybe Text
  }
  deriving (Show, Eq, Generic)

-- | A backend-call failure surfaced to the engine as a 'Left'. Mirrors the TS
-- per-call try/catch: the engine matches on the message (e.g. @"401"@) exactly
-- as @engine.ts@ does, and the unreachable 401 branch is not ported (L4).
newtype BotError = BotError {botErrorMessage :: Text}
  deriving (Show, Eq, Generic)
