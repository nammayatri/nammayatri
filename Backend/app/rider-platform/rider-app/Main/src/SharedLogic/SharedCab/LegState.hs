module SharedLogic.SharedCab.LegState
  ( SharedCabState (..),
    SharedCabLegStatus (..),
    FallbackGate (..),
    isSharedCabAgency,
    deriveSharedCabState,
    isDroppable,
    seatsHeld,
  )
where

import Data.Time (diffUTCTime)
import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Kernel.Prelude
import qualified Lib.JourneyModule.State.Types as JMState

data SharedCabState = FINDING | ALLOCATED | ARRIVING | FALLBACK | BOARDED | DEGRADED | DROPPED | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | FINDING fallback trigger inputs (R16, `05` §3/§7 fallbackAfterSec): a booking still FINDING falls
-- back to "board any cab" once it has burned through maxAttempts closed allocations, or has simply
-- been FINDING for too long (fallbackAfterSec), whichever comes first.
data FallbackGate = FallbackGate
  { attempts :: Int,
    maxAttempts :: Int,
    findingSince :: UTCTime,
    fallbackAfterSec :: Int
  }
  deriving (Show, Eq)

data SharedCabLegStatus = SharedCabLegStatus
  { state :: SharedCabState,
    -- | The FRFS ticket booking id, for booking-level calls such as R19 skip.
    bookingId :: Text,
    vehicleNumber :: Maybe Text,
    vehicleModel :: Maybe Text,
    driverName :: Maybe Text,
    driverPhotoUrl :: Maybe Text,
    etaToBoardStopSec :: Maybe Int,
    etaToDropStopSec :: Maybe Int,
    cabsComing :: Int,
    -- | R17: seconds left to board before the allocated cab's stand/moving timer releases it
    -- (allocKey's expiresAt); set only while ARRIVING, so the app can count down.
    boardDeadlineSec :: Maybe Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | Shared cabs ship in GTFS under the SHARED_CAB agency (agency gtfsId `<feed>:SHARED_CAB`).
isSharedCabAgency :: Text -> Bool
isSharedCabAgency agencyGtfsId = gtfsIdtoDomainCode agencyGtfsId == "SHARED_CAB"

-- | `07` §3 from the `05` §2 encoding: booking/ticket status, the booking's plate, whether that plate has a
-- live session, the FINDING fallback gate (R10/R16) and the ALLOCATED arrival deadline (R17, allocKey's
-- expiresAt when the stand or moving timer is armed). Nothing before the booking is confirmed.
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
      | fallbackDue = FALLBACK
      | otherwise = FINDING
    fallbackDue = maybe False (\g -> g.attempts >= g.maxAttempts || diffUTCTime now g.findingSince >= fromIntegral g.fallbackAfterSec) fallbackGate

-- | Tickets the rider still holds; cancelled or finished ones are left alone when the rider gets down.
isDroppable :: DFRFSTicket.FRFSTicketStatus -> Bool
isDroppable status = status `elem` [DFRFSTicket.ACTIVE, DFRFSTicket.INPROGRESS]

-- | A seat per ticket the rider still holds (`05` decision 1: quantity = ticket rows).
seatsHeld :: [DFRFSTicket.FRFSTicketStatus] -> Int
seatsHeld = length . filter isDroppable
