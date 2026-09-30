{-# LANGUAGE TemplateHaskell #-}

-- | R54: who may cancel a shared-cab booking, and what they get back. The rules are pure so each row is a test.
module SharedLogic.SharedCab.RefundPolicy
  ( CancelBy (..),
    CancelDecision (..),
    SharedCabCancelError (..),
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
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.JourneyModule.State.Types as JMState
import SharedLogic.SharedCab.Allocation.Types (RiderFix (..))
import SharedLogic.SharedCab.LegState (SharedCabState (..), deriveSharedCabState)
import SharedLogic.SharedCab.RefundDecision (Refund (..))

data CancelBy = ByRider | ByDriver
  deriving (Show, Eq)

data SharedCabCancelError
  = -- | the cab is waiting at the stop and the rider is beside it: the driver decides
    TalkToDriver
  | -- | a seat has boarded (or the ride is over): no self-cancel, no self-refund
    RideStarted
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SharedCabCancelError

instance IsBaseError SharedCabCancelError where
  toMessage = \case
    TalkToDriver -> Just "Your cab is waiting at the stop. Please talk to your driver."
    RideStarted -> Just "This shared cab ride has started and can't be cancelled."

instance IsHTTPError SharedCabCancelError where
  toErrorCode = \case
    TalkToDriver -> "SHARED_CAB_TALK_TO_DRIVER"
    RideStarted -> "SHARED_CAB_RIDE_STARTED"
  toHttpCode = \case
    TalkToDriver -> E409
    RideStarted -> E409

instance IsAPIError SharedCabCancelError

data CancelDecision = Allowed Refund | Rejected SharedCabCancelError
  deriving (Show, Eq)

-- | The row table: a started ride is nobody's self-refund; the driver's cancel always refunds in full; a rider beside
-- the waiting cab is sent to the driver; otherwise the rider is refunded in full unless a no-show is already booked
-- against this booking (which stays allocatable: one booking, one ticket).
decideCancel :: CancelBy -> SharedCabState -> Int -> [DFRFSTicket.FRFSTicketStatus] -> Bool -> CancelDecision
decideCancel by state noShows tickets riderNear
  | rideStarted = Rejected RideStarted
  | by == ByDriver = Allowed FullRefund
  | state == ARRIVING && riderNear = Rejected TalkToDriver
  | noShows > 0 = Allowed NoRefund
  | otherwise = Allowed FullRefund
  where
    rideStarted = state `elem` [BOARDED, DEGRADED, DROPPED] || any (`elem` [DFRFSTicket.INPROGRESS, DFRFSTicket.USED]) tickets

-- | FINDING / ALLOCATED / ARRIVING from the booking's plate and the allocation key's arrival deadline, the same
-- derivation as the leg status.
cancelState :: UTCTime -> Maybe Text -> Maybe UTCTime -> SharedCabState
cancelState now plate arrivalDeadline =
  fromMaybe FINDING $ deriveSharedCabState now (JMState.FRFSBooking DFRFSBooking.CONFIRMED) plate True Nothing arrivalDeadline

-- | Within `radiusM` of the board stop by the rider's latest fix. Unknown or stale counts as near (anti-cheat).
riderNearStop :: Int -> Int -> UTCTime -> Maybe LatLong -> Maybe RiderFix -> Bool
riderNearStop radiusM maxAgeSec now mbStop = \case
  Just riderFix
    | Just stop <- mbStop,
      diffUTCTime now riderFix.takenAt <= fromIntegral maxAgeSec ->
      distanceBetweenInMeters riderFix.position stop <= fromIntegral radiusM
  _ -> True

-- | A booking another cancel already ended (or started ending) is not cancelled again: the refund would run twice.
cancellableStatus :: DFRFSBooking.FRFSTicketBookingStatus -> Bool
cancellableStatus = (`notElem` [DFRFSBooking.CANCELLED, DFRFSBooking.COUNTER_CANCELLED, DFRFSBooking.CANCEL_INITIATED])

data DropRoute = MarkDropped | CancelInstead | NothingToDrop
  deriving (Show, Eq)

-- | "I got down": a boarded seat is dropped; one that never boarded is a cancel (and so meets its rules), never a
-- consumed ticket.
routeRiderDrop :: [DFRFSTicket.FRFSTicketStatus] -> DropRoute
routeRiderDrop tickets
  | DFRFSTicket.INPROGRESS `elem` tickets = MarkDropped
  | DFRFSTicket.ACTIVE `elem` tickets = CancelInstead
  | otherwise = NothingToDrop
