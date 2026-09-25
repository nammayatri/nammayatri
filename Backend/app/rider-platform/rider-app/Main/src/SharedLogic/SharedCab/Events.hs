-- | Shared-cab Kafka events (`05` §7): PRD B9, headway recalibration and the §14 metrics are computed from these.
-- Emitting is fire-and-forget; a Kafka or config failure is logged and never reaches the caller.
module SharedLogic.SharedCab.Events
  ( EventKind (..),
    Blame (..),
    BoardSource (..),
    DropBy (..),
    SharedCabEvent (..),
    EventFlow,
    eventName,
    sessionEvent,
    bookingEvent,
    emit,
    forSession,
    forBooking,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Text.Encoding as TE
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer (produceMessage)
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import SharedLogic.SharedCab.SessionState (Session)
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))

data Blame = BlameDriver | BlameRider | BlameNone
  deriving (Show, Eq)

data BoardSource = ByCode | ByDriverFallback | ByFallbackR10
  deriving (Show, Eq)

data DropBy = DroppedByDriver | DroppedByRider | DroppedByTick
  deriving (Show, Eq)

-- | Positional on purpose: record fields on a sum type are partial selectors.
data EventKind
  = SessionStarted
  | -- | from route
    RouteChanged Text
  | -- | pause reason
    Paused Text
  | Resumed
  | -- | end reason
    Ended Text
  | BookingCreated
  | BookingCancelled
  | -- | eta minutes, rank
    AllocationCreated (Maybe Int) Int
  | -- | outcome, blame
    AllocationClosed Text Blame
  | Boarded BoardSource
  | -- | from plate, sibling route
    Rebound Text Bool
  | DegradedBoarding
  | Dropped DropBy
  | NoShow
  | SeatLost
  | -- | rule, detail
    InvariantViolation Text Text
  deriving (Show, Eq)

data SharedCabEvent = SharedCabEvent
  { kind :: EventKind,
    vehicleNumber :: Maybe Text,
    bookingId :: Maybe Text,
    routeCode :: Maybe Text,
    driverId :: Maybe Text,
    emittedAt :: UTCTime
  }
  deriving (Show, Eq)

eventName :: EventKind -> Text
eventName = \case
  SessionStarted -> "session_started"
  RouteChanged _ -> "route_changed"
  Paused _ -> "paused"
  Resumed -> "resumed"
  Ended _ -> "ended"
  BookingCreated -> "booking_created"
  BookingCancelled -> "booking_cancelled"
  AllocationCreated _ _ -> "allocation_created"
  AllocationClosed _ _ -> "allocation_closed"
  Boarded _ -> "boarded"
  Rebound _ _ -> "rebound"
  DegradedBoarding -> "degraded_boarding"
  Dropped _ -> "dropped"
  NoShow -> "no_show"
  SeatLost -> "seat_lost"
  InvariantViolation _ _ -> "invariant_violation"

blameText :: Blame -> Text
blameText = \case
  BlameDriver -> "driver"
  BlameRider -> "rider"
  BlameNone -> "none"

boardSourceText :: BoardSource -> Text
boardSourceText = \case
  ByCode -> "code"
  ByDriverFallback -> "driver_fallback"
  ByFallbackR10 -> "fallback_R10"

dropByText :: DropBy -> Text
dropByText = \case
  DroppedByDriver -> "driver"
  DroppedByRider -> "rider"
  DroppedByTick -> "tick"

kindFields :: EventKind -> [(A.Key, A.Value)]
kindFields = \case
  RouteChanged from -> ["fromRoute" .= from]
  Paused reason -> ["reason" .= reason]
  Ended reason -> ["reason" .= reason]
  AllocationCreated eta rank -> ["etaMin" .= eta, "rank" .= rank]
  AllocationClosed outcome blame -> ["outcome" .= outcome, "blame" .= blameText blame]
  Boarded source -> ["source" .= boardSourceText source]
  Rebound from sibling -> ["fromVehicle" .= from, "siblingRoute" .= sibling]
  Dropped by -> ["by" .= dropByText by]
  InvariantViolation rule detail -> ["rule" .= rule, "detail" .= detail]
  _ -> []

-- | Flat JSON: `event`, `at`, the ids, then the kind's own fields.
instance ToJSON SharedCabEvent where
  toJSON e =
    A.object $
      [ "event" .= eventName e.kind,
        "at" .= e.emittedAt,
        "vehicleNumber" .= e.vehicleNumber,
        "bookingId" .= e.bookingId,
        "routeCode" .= e.routeCode,
        "driverId" .= e.driverId
      ]
        <> kindFields e.kind

sessionEvent :: EventKind -> Text -> Text -> Text -> UTCTime -> SharedCabEvent
sessionEvent k plate route driver time =
  SharedCabEvent {kind = k, vehicleNumber = Just plate, bookingId = Nothing, routeCode = Just route, driverId = Just driver, emittedAt = time}

bookingEvent :: EventKind -> Text -> Maybe Text -> Maybe Text -> UTCTime -> SharedCabEvent
bookingEvent k booking plate route time =
  SharedCabEvent {kind = k, vehicleNumber = plate, bookingId = Just booking, routeCode = route, driverId = Nothing, emittedAt = time}

type EventFlow m r = (CacheFlow m r, EsqDBFlow m r, MonadFlow m, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools])

-- | Keyed by plate (else booking) so one cab's events stay ordered on a partition.
emit :: EventFlow m r => Id DMOC.MerchantOperatingCity -> SharedCabEvent -> m ()
emit cityId event = fork "sharedCabEvent" $ do
  result <- withTryCatch "sharedCabEvent" $ do
    topic <- maybe "shared-cab-events" (fromMaybe "shared-cab-events" . (.sharedCabEventsTopic)) <$> getConfig (RiderConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing
    produceMessage (topic, TE.encodeUtf8 <$> maybe event.bookingId Just event.vehicleNumber) event
  either (\e -> logError $ "shared-cab event " <> eventName event.kind <> " not sent: " <> show e) pure result

forSession :: EventFlow m r => EventKind -> Session -> m ()
forSession k s = getCurrentTime >>= emit s.merchantOperatingCityId . sessionEvent k s.vehicleNumber s.routeCode s.driverId

forBooking :: EventFlow m r => EventKind -> DFTB.FRFSTicketBooking -> m ()
forBooking k booking = getCurrentTime >>= emit booking.merchantOperatingCityId . bookingEvent k booking.id.getId booking.vehicleNumber booking.routeCode
