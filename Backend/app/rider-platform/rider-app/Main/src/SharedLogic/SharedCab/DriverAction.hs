{-# LANGUAGE TemplateHaskell #-}

-- | `04` §4 D5 / D7: what the driver can do to a rider's booking from the allocation card.
module SharedLogic.SharedCab.DriverAction
  ( DriverAction (..),
    SharedCabDriverActionError (..),
    decideDriverAction,
    runDriverAction,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.SharedCab.Allocation as Allocation
import SharedLogic.SharedCab.Allocation.Types (AllocationOutcome (DriverCancelled), defaultAllocationConfig)
import SharedLogic.SharedCab.Booking (isSharedCabBooking, markDropped, withBookingLock)
import SharedLogic.SharedCab.LegState (isDroppable)
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), ownedSession)
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking

data SharedCabDriverActionError
  = BookingNotOnThisCab
  | BookingNotLive
  | BookingAlreadyBoarded
  | BookingNotBoarded
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SharedCabDriverActionError

instance IsBaseError SharedCabDriverActionError where
  toMessage = \case
    BookingNotOnThisCab -> Just "This booking isn't on your cab."
    BookingNotLive -> Just "This booking has no seat left to act on."
    BookingAlreadyBoarded -> Just "The rider is already on board; mark them dropped instead."
    BookingNotBoarded -> Just "The rider hasn't boarded yet."

instance IsHTTPError SharedCabDriverActionError where
  toErrorCode = \case
    BookingNotOnThisCab -> "SHARED_CAB_BOOKING_NOT_ON_THIS_CAB"
    BookingNotLive -> "SHARED_CAB_BOOKING_NOT_LIVE"
    BookingAlreadyBoarded -> "SHARED_CAB_BOOKING_ALREADY_BOARDED"
    BookingNotBoarded -> "SHARED_CAB_BOOKING_NOT_BOARDED"
  toHttpCode = \case
    BookingNotOnThisCab -> E404
    BookingNotLive -> E409
    BookingAlreadyBoarded -> E409
    BookingNotBoarded -> E409

instance IsAPIError SharedCabDriverActionError

data DriverAction
  = -- | Cancel an allocation the rider hasn't boarded: the booking goes back to FINDING.
    DriverCancel
  | -- | "Boarded without code" (05 §4 table): the driver's fallback when the rider can't type the code.
    DriverBoarded
  | -- | The driver marks the rider off at the drop.
    DriverDropped
  deriving (Show, Eq)

-- | The booking must be on this cab (`plate` canonical). Cancel needs a seat still waiting and none boarded (R7);
-- boarding needs a seat still held (a repeat tap is a no-op); a drop needs a seat on board.
decideDriverAction :: DriverAction -> Text -> Maybe Text -> [DFRFSTicket.FRFSTicketStatus] -> Either SharedCabDriverActionError ()
decideDriverAction action plate bookingPlate tickets
  | (canonicalisePlate <$> bookingPlate) /= Just plate = Left BookingNotOnThisCab
  | otherwise = case action of
    DriverCancel
      | boarded -> Left BookingAlreadyBoarded
      | not waiting -> Left BookingNotLive
      | otherwise -> Right ()
    DriverBoarded
      | not (any isDroppable tickets) -> Left BookingNotLive
      | otherwise -> Right ()
    DriverDropped
      | not boarded -> Left BookingNotBoarded
      | otherwise -> Right ()
  where
    boarded = DFRFSTicket.INPROGRESS `elem` tickets
    waiting = DFRFSTicket.ACTIVE `elem` tickets

-- | Plate lock, then booking lock (05 §2 lock order); the driver must own the cab's live session.
-- Returns the session to render. A drop may apply an `afterLastDrop` route change, so re-read the session after.
runDriverAction :: DriverAction -> Text -> Text -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow Session
runDriverAction action driver rawPlate bookingId = do
  mbDropped <- Session.withPlateLock plate $ do
    s <- Session.readSession plate >>= either throwError pure . ownedSession driver
    booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM BookingNotOnThisCab
    unless (isSharedCabBooking booking && booking.status == DFRFSTicketBookingStatus.CONFIRMED) $ throwError BookingNotLive
    -- markDropped and the allocation release take the booking lock themselves; it isn't re-entrant. Holding the
    -- plate lock keeps a boarding onto this cab from landing between the decision and the write.
    case action of
      DriverDropped -> do
        decide booking
        markDropped booking
        pure (Just booking)
      -- TODO(7.4): count the DRIVER_CANCELLED miss on the session (consecutiveMisses, absent pause); nothing counts
      -- misses yet. TODO(7.6): emit allocation_closed {blame: driver}.
      DriverCancel -> do
        decide booking
        void $ Allocation.releaseSharedCabAllocation defaultAllocationConfig booking.id plate DriverCancelled
        pure Nothing
      DriverBoarded -> withBookingLock booking.id $ do
        decide booking
        board s booking
        pure Nothing
  whenJust mbDropped $ \_ -> Session.applyQueuedRoute plate
  Session.getSession plate >>= fromMaybeM BookingNotOnThisCab
  where
    plate = canonicalisePlate rawPlate
    decide booking = do
      tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
      either throwError pure $ decideDriverAction action plate booking.vehicleNumber (map (.status) tickets)
    -- A boarded booking is done with allocation: its timer key goes. TODO(7.6): emit boarded {source: driver_fallback}.
    board s booking = do
      tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
      forM_ (filter ((== DFRFSTicket.ACTIVE) . (.status)) tickets) $ \ticket ->
        QFRFSTicket.updateStatusByTBookingIdAndTicketNumber DFRFSTicket.INPROGRESS (Just plate) booking.id ticket.ticketNumber
      QFRFSTicketBooking.updateVehicleTripId (Just s.vehicleTripId) booking.id
      shared $ Redis.del (Allocation.allocKey booking.id.getId)
    -- Booking.shared once R14 exports it: the allocation keys live unprefixed in the cross-app master cell.
    shared = Redis.runInMasterCloudRedisCellWithCrossAppRedis . Redis.withMasterRedis
