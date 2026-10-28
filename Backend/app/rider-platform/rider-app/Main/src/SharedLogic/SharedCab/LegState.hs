module SharedLogic.SharedCab.LegState
  ( SharedCabState (..),
    SharedCabLegStatus (..),
    CabPosition (..),
    cabTracking,
    callWindow,
    tripMatches,
    callWindowState,
    CancelReason (..),
    FallbackGate (..),
    fallbackDue,
    fallbackReached,
    fallbackTimeElapsed,
    isSharedCabAgency,
    legNeedsChains,
    sharedCabLegMode,
    sharedCabFareTiers,
    deriveSharedCabState,
    isDroppable,
    seatsHeld,
    UpcomingCab (..),
    upcomingCabsOf,
    SkipVerdict (..),
    skipMinEtaSec,
    decideRiderSkip,
    skipAvailableFor,
  )
where

import BecknV2.FRFS.Enums (ServiceTierType (SHARED_CAB))
import Data.List (sortOn)
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
    maskedCallNumber :: Maybe Text,
    chatChannelId :: Maybe Text,
    cabsComing :: Int,
    boardDeadlineSec :: Maybe Int,
    cancelReason :: Maybe CancelReason,
    preferredCab :: Maybe Text,
    preferredCabLost :: Bool,
    -- | Other cabs on the route that reach the board stop after the assigned one, soonest first (ALLOCATED and ARRIVING only).
    upcomingCabs :: [UpcomingCab],
    -- | The rider may still skip the assigned cab: once per booking, and only while it is more than skipMinEtaSec away.
    skipAvailable :: Bool
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

isSharedCabAgency :: Text -> Bool
isSharedCabAgency agencyGtfsId = gtfsIdtoDomainCode agencyGtfsId == "SHARED_CAB"

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

-- | A leg that is not over still depends on the city's allocation and expiry chains.
legNeedsChains :: SharedCabState -> Bool
legNeedsChains = (`notElem` [DROPPED, CANCELLED])

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

-- | Calling and chatting with the cab's driver is open only while a cab holds the rider's seat.
callWindowState :: SharedCabState -> Bool
callWindowState = (`elem` [ALLOCATED, ARRIVING, BOARDED])

-- | The same window from the booking's own rows, for callers that have no leg status: a confirmed booking on a plate with a held ticket.
callWindow :: DFRFSBooking.FRFSTicketBookingStatus -> [DFRFSTicket.FRFSTicketStatus] -> Maybe Text -> Bool
callWindow bookingStatus ticketStatuses mbPlate = bookingStatus == DFRFSBooking.CONFIRMED && isJust mbPlate && seatsHeld ticketStatuses > 0

-- | A boarded booking (it carries the trip it boarded on) may only reach the cab running that same trip: a rider still marked
-- on board from an earlier run of the plate is not this driver's rider. Before boarding there is no trip and the plate and
-- allocation decide.
tripMatches :: Maybe Text -> Text -> Bool
tripMatches bookingTrip sessionTrip = maybe True (== sessionTrip) bookingTrip

isDroppable :: DFRFSTicket.FRFSTicketStatus -> Bool
isDroppable status = status `elem` [DFRFSTicket.ACTIVE, DFRFSTicket.INPROGRESS]

seatsHeld :: [DFRFSTicket.FRFSTicketStatus] -> Int
seatsHeld = length . filter isDroppable

-- | A cab that reaches the rider's board stop after the assigned one. `cabId` is the same opaque id pick-a-cab lists; never a plate.
data UpcomingCab = UpcomingCab
  { cabId :: Text,
    etaSec :: Int,
    freeSeats :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | At most three cabs that arrive after the assigned one (all of them when its own ETA is unknown), soonest first.
upcomingCabsOf :: Maybe Int -> [UpcomingCab] -> [UpcomingCab]
upcomingCabsOf assignedEtaSec = take 3 . sortOn (.etaSec) . filter (\c -> maybe True (< c.etaSec) assignedEtaSec)

data SkipVerdict = SkipOk | SkipUsed | SkipTooLate
  deriving (Show, Eq)

-- | The assigned cab must be further than this from the board stop for the rider to skip it.
skipMinEtaSec :: Int
skipMinEtaSec = 120

-- | One skip per booking; never after boarding; only while the cab's ETA to the board stop is known and over skipMinEtaSec.
decideRiderSkip :: Bool -> Bool -> Maybe Int -> SkipVerdict
decideRiderSkip used boarded mbEtaSec
  | used = SkipUsed
  | boarded = SkipTooLate
  | maybe True (<= skipMinEtaSec) mbEtaSec = SkipTooLate
  | otherwise = SkipOk

-- | What the app uses to show the skip button.
skipAvailableFor :: SharedCabState -> Bool -> Maybe Int -> Bool
skipAvailableFor st used mbEtaSec = st `elem` [ALLOCATED, ARRIVING] && decideRiderSkip used False mbEtaSec == SkipOk
