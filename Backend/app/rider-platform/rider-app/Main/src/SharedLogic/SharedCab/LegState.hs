module SharedLogic.SharedCab.LegState
  ( SharedCabState (..),
    SharedCabLegStatus (..),
    CabPosition (..),
    cabTracking,
    CancelReason (..),
    FallbackGate (..),
    fallbackDue,
    fallbackReached,
    fallbackTimeElapsed,
    isSharedCabAgency,
    sharedCabLegMode,
    onlyTransitLeg,
    sharedCabFareTiers,
    deriveSharedCabState,
    isDroppable,
    seatsHeld,
  )
where

import BecknV2.FRFS.Enums (ServiceTierType (SHARED_CAB))
import Data.Aeson (Value (..), defaultOptions, omitNothingFields)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Time (diffUTCTime)
import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.Trip as DTrip
import Kernel.Prelude
import qualified Lib.JourneyModule.State.Types as JMState
import qualified SharedLogic.External.LocationTrackingService.Types as LT

data SharedCabState = FINDING | ALLOCATED | ARRIVING | FALLBACK | BOARDED | DEGRADED | DROPPED | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | Why the leg was cancelled, for "cancelled after missed cabs". NO_CAB_FOUND and SYSTEM cover cancels outside the shared-cab code.
data CancelReason = NO_SHOW_CAP | DRIVER | RIDER | SYSTEM | NO_CAB_FOUND
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | A FINDING booking falls back to "board any cab" after maxAttempts closed allocations or fallbackAfterSec, whichever first.
data FallbackGate = FallbackGate
  { attempts :: Int,
    maxAttempts :: Int,
    findingSince :: UTCTime,
    fallbackAfterSec :: Int
  }
  deriving (Show, Eq)

data CabPosition = CabPosition {lat :: Double, lon :: Double}
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

cabTracking :: UTCTime -> Text -> Text -> LT.VehicleInfo -> (CabPosition, Maybe Int, Maybe Int)
cabTracking now boardStopCode dropStopCode vi = (CabPosition {lat = vi.latitude, lon = vi.longitude}, etaTo boardStopCode, etaTo dropStopCode)
  where
    etaTo code = do
      u <- find (\u' -> u'.stop.stopCode == code && u'.status == LT.Upcoming) =<< vi.upcomingStops
      pure (max 0 (floor (diffUTCTime u.eta now)))

data SharedCabLegStatus = SharedCabLegStatus
  { state :: SharedCabState,
    bookingId :: Text,
    vehicleNumber :: Maybe Text,
    vehicleModel :: Maybe Text,
    driverName :: Maybe Text,
    driverPhotoUrl :: Maybe Text,
    etaToBoardStopSec :: Maybe Int,
    etaToDropStopSec :: Maybe Int,
    position :: Maybe CabPosition,
    cabsComing :: Int,
    boardDeadlineSec :: Maybe Int,
    cancelReason :: Maybe CancelReason
  }
  deriving (Show, Eq, Generic, ToSchema)

-- Nothing is omitted for cancelReason alone (older app builds); other Nothing fields ship null so strict decoders keep their keys.
instance ToJSON SharedCabLegStatus where
  toJSON status = case genericToJSON defaultOptions status of
    Object obj | isNothing status.cancelReason -> Object (KeyMap.delete (Key.fromText "cancelReason") obj)
    v -> v

instance FromJSON SharedCabLegStatus where
  parseJSON = genericParseJSON defaultOptions {omitNothingFields = True}

isSharedCabAgency :: Text -> Bool
isSharedCabAgency agencyGtfsId = gtfsIdtoDomainCode agencyGtfsId == "SHARED_CAB"

-- | Only a journey with exactly one non-walk leg ends when that leg is cancelled.
onlyTransitLeg :: [DTrip.MultimodalTravelMode] -> Bool
onlyTransitLeg modes = length (filter (/= DTrip.Walk) modes) == 1

-- | A booking on the SHARED_CAB tier has the last word on the leg's mode, whatever the search stamped.
sharedCabLegMode :: Maybe ServiceTierType -> DTrip.MultimodalTravelMode -> DTrip.MultimodalTravelMode
sharedCabLegMode (Just SHARED_CAB) _ = DTrip.SharedCab
sharedCabLegMode _ mode = mode

-- | Shared cabs are frequency-based GTFS, so the bus-schedule availability filter returns nothing for them; their fares resolve under the SHARED_CAB tier.
sharedCabFareTiers :: Maybe Text -> Maybe [ServiceTierType]
sharedCabFareTiers mbAgencyGtfsId = [SHARED_CAB] <$ guard (maybe False isSharedCabAgency mbAgencyGtfsId)

fallbackTimeElapsed :: UTCTime -> UTCTime -> Int -> Bool
fallbackTimeElapsed now findingSince fallbackAfterSec = diffUTCTime now findingSince >= fromIntegral fallbackAfterSec

fallbackDue :: UTCTime -> FallbackGate -> Bool
fallbackDue now g = fallbackReached now g.maxAttempts g.attempts g.findingSince g.fallbackAfterSec

fallbackReached :: UTCTime -> Int -> Int -> UTCTime -> Int -> Bool
fallbackReached now maxAttempts attempts findingSince fallbackAfterSec = attempts >= maxAttempts || fallbackTimeElapsed now findingSince fallbackAfterSec

deriveSharedCabState :: UTCTime -> JMState.JourneyBookingStatus -> Maybe Text -> Bool -> Maybe FallbackGate -> Maybe UTCTime -> Maybe SharedCabState
deriveSharedCabState now bookingStatus mbVehicleNumber hasLiveSession fallbackGate arrivalDeadline = case bookingStatus of
  JMState.FRFSBooking DFRFSBooking.CONFIRMED -> Just waiting
  JMState.FRFSTicket DFRFSTicket.ACTIVE -> Just waiting
  JMState.FRFSTicket DFRFSTicket.INPROGRESS -> Just $ if hasLiveSession then BOARDED else DEGRADED
  JMState.FRFSTicket DFRFSTicket.USED -> Just DROPPED
  JMState.FRFSTicket DFRFSTicket.EXPIRED -> Just DROPPED
  JMState.Feedback _ -> Just DROPPED
  JMState.FRFSTicket _ -> Just CANCELLED
  JMState.FRFSBooking status
    | status `elem` [DFRFSBooking.CANCELLED, DFRFSBooking.COUNTER_CANCELLED, DFRFSBooking.CANCEL_INITIATED, DFRFSBooking.RESCHEDULED, DFRFSBooking.FAILED] -> Just CANCELLED
  _ -> Nothing
  where
    waiting
      | isJust mbVehicleNumber = if isJust arrivalDeadline then ARRIVING else ALLOCATED
      | fallbackDue' = FALLBACK
      | otherwise = FINDING
    fallbackDue' = maybe False (fallbackDue now) fallbackGate

isDroppable :: DFRFSTicket.FRFSTicketStatus -> Bool
isDroppable status = status `elem` [DFRFSTicket.ACTIVE, DFRFSTicket.INPROGRESS]

seatsHeld :: [DFRFSTicket.FRFSTicketStatus] -> Int
seatsHeld = length . filter isDroppable
