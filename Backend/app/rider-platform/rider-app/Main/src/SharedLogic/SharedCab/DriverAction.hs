{-# LANGUAGE TemplateHaskell #-}

-- | `04` §4 D5 / D7: what the driver can do to a rider's booking from the allocation card.
module SharedLogic.SharedCab.DriverAction
  ( DriverAction (..),
    SharedCabDriverActionError (..),
    decideDriverAction,
    requireReason,
    runDriverAction,
  )
where

import qualified Data.Text as T
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Base as JM
import qualified SharedLogic.SharedCab.Allocation as Allocation
import SharedLogic.SharedCab.Allocation.Types (AllocationOutcome (RouteChanged))
import SharedLogic.SharedCab.Booking (isSharedCabBooking, markDropped, shared, withBookingLock)
import SharedLogic.SharedCab.Cancel (CancelStage (ConfirmCancel), withSharedCabCancel)
import qualified SharedLogic.SharedCab.Events as Events
import SharedLogic.SharedCab.LegState (isDroppable)
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import SharedLogic.SharedCab.RefundPolicy (CancelBy (ByDriver))
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState (Session (..), ownedSession)
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import Tools.Error (SharedCabSessionError (SessionNotFound))

data SharedCabDriverActionError
  = BookingNotOnThisCab
  | BookingNotLive
  | BookingAlreadyBoarded
  | BookingNotBoarded
  | CancelReasonRequired
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''SharedCabDriverActionError

instance IsBaseError SharedCabDriverActionError where
  toMessage = \case
    BookingNotOnThisCab -> Just "This booking isn't on your cab."
    BookingNotLive -> Just "This booking has no seat left to act on."
    BookingAlreadyBoarded -> Just "The rider is already on board; mark them dropped instead."
    BookingNotBoarded -> Just "The rider hasn't boarded yet."
    CancelReasonRequired -> Just "Say why you are cancelling this rider."

instance IsHTTPError SharedCabDriverActionError where
  toErrorCode = \case
    BookingNotOnThisCab -> "SHARED_CAB_BOOKING_NOT_ON_THIS_CAB"
    BookingNotLive -> "SHARED_CAB_BOOKING_NOT_LIVE"
    BookingAlreadyBoarded -> "SHARED_CAB_BOOKING_ALREADY_BOARDED"
    BookingNotBoarded -> "SHARED_CAB_BOOKING_NOT_BOARDED"
    CancelReasonRequired -> "SHARED_CAB_CANCEL_REASON_REQUIRED"
  toHttpCode = \case
    BookingNotOnThisCab -> E404
    BookingNotLive -> E409
    BookingAlreadyBoarded -> E409
    BookingNotBoarded -> E409
    CancelReasonRequired -> E400

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

-- | R54: a driver's cancel says why.
requireReason :: Maybe Text -> Either SharedCabDriverActionError Text
requireReason mbReason = maybe (Left CancelReasonRequired) Right (mfilter (not . T.null) (T.strip <$> mbReason))

-- | Plate lock, then booking lock (05 §2 lock order); the driver must own the cab's live session.
-- Returns the session to render. A drop may apply an `afterLastDrop` route change, so re-read the session after.
runDriverAction :: DriverAction -> Text -> Text -> Maybe Text -> Id DFRFSTicketBooking.FRFSTicketBooking -> Environment.Flow Session
runDriverAction action driver rawPlate mbReason bookingId = do
  mbCancelReason <- case action of
    DriverCancel -> Just <$> either throwError pure (requireReason mbReason)
    _ -> pure Nothing
  (mbDropped, mbCancel) <- Session.withPlateLock plate $ do
    s <- Session.readSession plate >>= either throwError pure . ownedSession driver
    booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM BookingNotOnThisCab
    unless (isSharedCabBooking booking && booking.status == DFRFSTicketBookingStatus.CONFIRMED) $ throwError BookingNotLive
    -- markDropped and the allocation release take the booking lock themselves; it isn't re-entrant. Holding the
    -- plate lock keeps a boarding onto this cab from landing between the decision and the write.
    case action of
      DriverDropped -> do
        decide booking
        markDropped Events.DroppedByDriver booking
        pure (Just booking, Nothing)
      -- the cancel's refund goes out to the payment service: it runs after the plate lock is dropped
      DriverCancel -> do
        decide booking
        pure (Nothing, Just booking)
      DriverBoarded -> withBookingLock booking.id $ do
        decide booking
        board s booking
        pure (Nothing, Nothing)
  forM_ ((,) <$> mbCancelReason <*> mbCancel) $ uncurry cancelByDriver
  whenJust mbDropped $ \_ -> do
    switched <- Session.applyQueuedRoute plate
    when switched $ Allocation.releaseUnboarded plate RouteChanged
  Session.getSession plate >>= fromMaybeM SessionNotFound
  where
    plate = canonicalisePlate rawPlate
    -- the booking lock inside re-decides (a boarding since the plate lock flips a ticket, and the cancel is refused)
    cancelByDriver reason booking = do
      leg <- QJourneyLeg.findByLegSearchId (Just booking.searchId.getId) >>= fromMaybeM (InvalidRequest "No journey leg for this booking")
      withSharedCabCancel ByDriver ConfirmCancel (Just reason) stillOnThisCab booking $
        JM.cancelLeg leg (SCR.CancellationReasonCode "") False Nothing
    stillOnThisCab fresh = do
      unless (fresh.status == DFRFSTicketBookingStatus.CONFIRMED) $ throwError BookingNotLive
      unless ((canonicalisePlate <$> fresh.vehicleNumber) == Just plate) $ throwError BookingNotOnThisCab
    decide booking = do
      tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
      either throwError pure $ decideDriverAction action plate booking.vehicleNumber (map (.status) tickets)
    -- A boarded booking is done with allocation: its timer key goes.
    -- TODO(05 §2): write the journey leg's finalBoardedBusNumber / busTagNumber as the code path (8.1) does.
    board s booking = do
      tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
      forM_ (filter ((== DFRFSTicket.ACTIVE) . (.status)) tickets) $ \ticket ->
        QFRFSTicket.updateStatusByTBookingIdAndTicketNumber DFRFSTicket.INPROGRESS (Just plate) booking.id ticket.ticketNumber
      QFRFSTicketBooking.updateVehicleTripId (Just s.vehicleTripId) booking.id
      shared $ Redis.del (Allocation.allocKey booking.id.getId)
      now <- getCurrentTime
      Events.emit s.merchantOperatingCityId . Events.withTrip (Just s.vehicleTripId.getId) $
        Events.bookingEvent (Events.Boarded Events.ByDriverFallback) booking.id.getId (Just plate) (Just s.routeCode) now
