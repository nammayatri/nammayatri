module SharedLogic.SharedCab.LegState
  ( SharedCabState (..),
    SharedCabLegStatus (..),
    isSharedCabAgency,
    deriveSharedCabState,
    isDroppable,
    seatsHeld,
  )
where

import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Kernel.Prelude
import qualified Lib.JourneyModule.State.Types as JMState

-- | ARRIVING and FALLBACK need the allocation tick (7.4, 7.5); nothing derives them yet.
data SharedCabState = FINDING | ALLOCATED | ARRIVING | FALLBACK | BOARDED | DEGRADED | DROPPED | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data SharedCabLegStatus = SharedCabLegStatus
  { state :: SharedCabState,
    vehicleNumber :: Maybe Text,
    vehicleModel :: Maybe Text,
    driverName :: Maybe Text,
    driverPhotoUrl :: Maybe Text,
    etaToBoardStopSec :: Maybe Int,
    etaToDropStopSec :: Maybe Int,
    cabsComing :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | Shared cabs ship in GTFS under the SHARED_CAB agency (agency gtfsId `<feed>:SHARED_CAB`).
isSharedCabAgency :: Text -> Bool
isSharedCabAgency agencyGtfsId = gtfsIdtoDomainCode agencyGtfsId == "SHARED_CAB"

-- | `07` §3 from the `05` §2 encoding: booking/ticket status, the booking's plate, and whether that plate has a live session.
-- Nothing before the booking is confirmed.
deriveSharedCabState :: JMState.JourneyBookingStatus -> Maybe Text -> Bool -> Maybe SharedCabState
deriveSharedCabState bookingStatus mbVehicleNumber hasLiveSession = case bookingStatus of
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
    waiting = maybe FINDING (const ALLOCATED) mbVehicleNumber

-- | Tickets the rider still holds; cancelled or finished ones are left alone when the rider gets down.
isDroppable :: DFRFSTicket.FRFSTicketStatus -> Bool
isDroppable status = status `elem` [DFRFSTicket.ACTIVE, DFRFSTicket.INPROGRESS]

-- | A seat per ticket the rider still holds (`05` decision 1: quantity = ticket rows).
seatsHeld :: [DFRFSTicket.FRFSTicketStatus] -> Int
seatsHeld = length . filter isDroppable
