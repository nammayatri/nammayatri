-- | Who may cancel a shared-cab booking, and what they get back. The rules are pure so each row is a test.
module SharedLogic.SharedCab.RefundPolicy
  ( CancelBy (..),
    CancelDecision (..),
    DropRoute (..),
    decideCancel,
    cancellableStatus,
    cancelState,
    riderNearStop,
    routeRiderDrop,
  )
where

import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.JourneyModule.State.Types as JMState
import SharedLogic.SharedCab.Allocation.Types (RiderFix (..))
import SharedLogic.SharedCab.LegState (SharedCabState (..), deriveSharedCabState)
import SharedLogic.SharedCab.RefundDecision (Refund (..))
import Tools.Error (SharedCabCancelError (..))

data CancelBy = ByRider | ByDriver
  deriving (Show, Eq)

data CancelDecision = Allowed Refund | Rejected SharedCabCancelError
  deriving (Show, Eq)

-- | A started ride is nobody's self-refund; the driver's cancel refunds in full; a rider beside the waiting cab is sent to the driver;
-- otherwise a full refund unless a no-show is already booked.
decideCancel :: CancelBy -> SharedCabState -> Int -> [DFRFSTicket.FRFSTicketStatus] -> Bool -> CancelDecision
decideCancel by state noShows tickets riderNear
  | rideStarted = Rejected RideStarted
  | by == ByDriver = Allowed FullRefund
  | state == ARRIVING && riderNear = Rejected TalkToDriver
  | noShows > 0 = Allowed NoRefund
  | otherwise = Allowed FullRefund
  where
    rideStarted = state `elem` [BOARDED, DEGRADED, DROPPED] || any (`elem` [DFRFSTicket.INPROGRESS, DFRFSTicket.USED]) tickets

cancelState :: UTCTime -> Maybe Text -> Maybe UTCTime -> SharedCabState
cancelState now plate arrivalDeadline =
  fromMaybe FINDING $ deriveSharedCabState now (JMState.FRFSBooking DFRFSBooking.CONFIRMED) plate True Nothing arrivalDeadline

riderNearStop :: Int -> Int -> UTCTime -> Maybe LatLong -> Maybe RiderFix -> Bool
riderNearStop radiusM maxAgeSec now mbStop = \case
  Just riderFix
    | Just stop <- mbStop,
      diffUTCTime now riderFix.takenAt <= fromIntegral maxAgeSec ->
      distanceBetweenInMeters riderFix.position stop <= fromIntegral radiusM
  _ -> True

cancellableStatus :: DFRFSBooking.FRFSTicketBookingStatus -> Bool
cancellableStatus = (`notElem` [DFRFSBooking.CANCELLED, DFRFSBooking.COUNTER_CANCELLED, DFRFSBooking.CANCEL_INITIATED, DFRFSBooking.FAILED, DFRFSBooking.RESCHEDULED])

data DropRoute = MarkDropped | CancelInstead | NothingToDrop
  deriving (Show, Eq)

routeRiderDrop :: [DFRFSTicket.FRFSTicketStatus] -> DropRoute
routeRiderDrop tickets
  | DFRFSTicket.INPROGRESS `elem` tickets = MarkDropped
  | DFRFSTicket.ACTIVE `elem` tickets = CancelInstead
  | otherwise = NothingToDrop
