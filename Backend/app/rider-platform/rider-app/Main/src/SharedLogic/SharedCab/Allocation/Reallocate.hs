-- | Closing an allocation under the booking lock: back to FINDING with a bumped attempt count, or the rider's last no-show cancelling the booking.
module SharedLogic.SharedCab.Allocation.Reallocate
  ( Closed (..),
    closeAllocation,
    CloseResult (..),
    closeLocked,
    CloseStep (..),
    closeSteps,
    afterClose,
    releaseCancelledBooking,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (..))
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSCancelEffects as FRFSCancelEffects
import qualified SharedLogic.FRFSCancelJourney as FRFSCancelJourney
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import SharedLogic.SharedCab.Allocation.Claim (AllocFlow, claimFallbackPush, findLive)
import SharedLogic.SharedCab.Allocation.Select (LiveBooking (..), closable, crossedMaxAttempts, isRescheduledChild)
import SharedLogic.SharedCab.Allocation.Types
import SharedLogic.SharedCab.Booking (recordCancelReason, shared, withBookingLock)
import SharedLogic.SharedCab.BookingState
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.Keys (fallbackPushedKey)
import SharedLogic.SharedCab.LegState (CancelReason (NO_SHOW_CAP))
import qualified SharedLogic.SharedCab.NoShow as NoShow
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.RefundDecision (Refund (..), gateByPayment, refundAmounts)
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.Person as QPerson

data Closed = Closed
  { closedCity :: Id DMOC.MerchantOperatingCity,
    result :: CloseResult
  }

data CloseResult
  = -- | back to FINDING
    Reallocated
  | -- | back to FINDING, and this close crossed maxAttempts: the rider is offered any cab
    ReallocatedIntoFallback
  | -- | the rider's last allowed no-show: the booking is cancelled, not FINDING
    AutoCancelled
  deriving (Show, Eq)

-- | The one close path: close under the booking lock (`inLock` runs there too, only if this call won the close), then the effects
-- outside it. The caller re-ticks the city.
closeAllocation :: AllocFlow m r => Config.SharedCabTunables -> Id DFTB.FRFSTicketBooking -> Text -> AllocationOutcome -> m () -> m (Maybe Closed)
closeAllocation cfg bookingId plate outcome inLock = do
  closed <- withBookingLock bookingId $ do
    mbClosed <- closeLocked cfg bookingId plate outcome
    when (isJust mbClosed) inLock
    pure mbClosed
  afterClose cfg bookingId plate outcome closed
  pure closed

-- | A won close after its CAS: the seats go back on every close, the rider's last no-show cancelling the booking included.
data CloseStep = GiveBackSeats | CancelForNoShows | FindAgain
  deriving (Show, Eq)

closeSteps :: Int -> Int -> Blame -> [CloseStep]
closeSteps maxNoShows noShows blame =
  GiveBackSeats : [if NoShow.actionAfterClose blame maxNoShows noShows == NoShow.AutoCancel then CancelForNoShows else FindAgain]

-- | Inside the booking lock: CAS plate -> null (back to FINDING), clear the alloc key, bump attempts. Nothing when another closer won.
closeLocked ::
  (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  Config.SharedCabTunables ->
  Id DFTB.FRFSTicketBooking ->
  Text ->
  AllocationOutcome ->
  m (Maybe Closed)
closeLocked cfg bookingId expectedPlate outcome =
  findLive bookingId >>= \case
    Just (LiveBooking b statuses) | closable expectedPlate b.status b.vehicleNumber statuses -> Just <$> close b
    _ -> pure Nothing
  where
    close b = do
      case blameFor outcome of
        BlameRider -> QFRFSTicketBooking.releaseAllocatedVehicle Nothing (NoShow.noShowsAfter BlameRider b.sharedCabNoShows) b.id (Just expectedPlate)
        _ -> QFRFSTicketBooking.updateAllocatedVehicle Nothing b.id (Just expectedPlate)
      let steps = closeSteps cfg.maxNoShows b.sharedCabNoShows (blameFor outcome)
      when (GiveBackSeats `elem` steps) $ Seats.releaseBooking bookingId
      if CancelForNoShows `elem` steps
        then do
          cancelForNoShows b
          clearBookingState bookingId
          pure Closed {closedCity = b.merchantOperatingCityId, result = AutoCancelled}
        else reallocate b
    reallocate b = do
      clearAlloc bookingId
      when (skipsPlateOnClose outcome) $ addSkipped (allocKeyTtl cfg) bookingId expectedPlate
      -- Known limitation: a Redis flush resets attempts.
      attemptsBefore <- readAttempts bookingId
      let attemptsNow = attemptsBefore + (if countsTowardAttempts outcome then 1 else 0)
      writeAttempts (allocKeyTtl cfg) bookingId attemptsNow
      let fallbackJustTriggered = crossedMaxAttempts cfg.maxAttempts attemptsBefore attemptsNow
      now <- getCurrentTime
      writeFindingSince (allocKeyTtl cfg) bookingId now
      when (attemptsNow < cfg.maxAttempts) $ shared $ Redis.del (fallbackPushedKey bookingId)
      when fallbackJustTriggered $
        logWarning $ "shared-cab booking " <> bookingId.getId <> " exhausted allocation attempts (" <> show attemptsNow <> "); fallback surface"
      pure Closed {closedCity = b.merchantOperatingCityId, result = if fallbackJustTriggered then ReallocatedIntoFallback else Reallocated}

-- | The rider's last allowed no-show: cancelled by the system, fare kept (marking it refund-pending would make the payment
-- service refund). SMS and Google Wallet flips are skipped: they need the API Flow the scheduler env lacks. The row flips
-- to CANCELLED first, so a later failure can only lose an observability step (each effect is wrapped in withTryCatch).
cancelForNoShows :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Redis.HedisFlow m r, EncFlow m r) => DFTB.FRFSTicketBooking -> m ()
cancelForNoShows b = do
  void $ QFRFSTicketBooking.updateStatusById CANCELLED b.id
  void $ QFRFSTicket.updateAllStatusByBookingId DFRFSTicket.CANCELLED b.id
  void $ QFRFSRecon.updateStatusByTicketBookingId (Just DFRFSTicket.CANCELLED) b.id
  decision <- gateByPayment b NoRefund
  let (charges, refundAmount) = refundAmounts (fromMaybe b.totalPrice.amount b.overriddenAmount) decision
  QFRFSTicketBooking.updateRefundCancellationChargesAndIsCancellableByBookingId (Just refundAmount) (Just charges) (Just True) b.id
  void . withTryCatch "sharedCab:cancelForNoShows:cancelJourney" $
    FRFSCancelJourney.cancelJourneyIfOnlyTransitLeg b.searchId.getId
  withTryCatch "sharedCab:cancelForNoShows:recordCancelReason" (recordCancelReason b.id NO_SHOW_CAP)
    >>= either (\e -> logError $ "shared-cab cancel-reason not recorded for booking " <> b.id.getId <> ": " <> show e) pure
  releaseCancelledBooking b

-- | Side effects shared by every system cancel the tick does (the shared effects of any FRFS cancel).
releaseCancelledBooking :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => DFTB.FRFSTicketBooking -> m ()
releaseCancelledBooking b = do
  quantity <- FRFSPassOverride.ticketQuantityForBooking b
  mbPerson <- QPerson.findById b.riderId
  FRFSCancelEffects.releaseCancelledBooking True (not (isRescheduledChild b.parentBookingId)) b mbPerson quantity

afterClose :: AllocFlow m r => Config.SharedCabTunables -> Id DFTB.FRFSTicketBooking -> Text -> AllocationOutcome -> Maybe Closed -> m ()
afterClose cfg bookingId plate outcome closed =
  whenJust closed $ \Closed {result} -> do
    mbBooking <- QFRFSTicketBooking.findById bookingId
    case result of
      AutoCancelled -> do
        mapM_ (\b -> Notify.notifyBookingCancelled b.sharedCabNoShows b) mbBooking
      Reallocated -> notifyReassignment mbBooking
      ReallocatedIntoFallback -> do
        notifyReassignment mbBooking
        whenM (claimFallbackPush cfg bookingId) $ mapM_ Notify.notifyBoardAny mbBooking
    when (outcome `elem` [DriverDeclined, OfferTimeout]) $ onOfferLost bookingId plate mbBooking
  where
    notifyReassignment mbBooking = forM_ (Notify.reassignReasonFor outcome) $ \reason -> mapM_ (Notify.notifyReassigned reason) mbBooking

-- | A driver declined an offer or let it lapse (the driver app counts the decline). A rider who picked this cab hears once that it is gone;
-- a rider who did not pick it was never told of it. Best effort: the booking is already back to FINDING.
onOfferLost :: AllocFlow m r => Id DFTB.FRFSTicketBooking -> Text -> Maybe DFTB.FRFSTicketBooking -> m ()
onOfferLost bookingId plate mbBooking = do
  withTryCatch "sharedCabOfferLost:picked" pickedLost >>= either (\e -> logError $ "shared-cab picked-cab loss failed for booking " <> bookingId.getId <> ": " <> show e) pure
  where
    pickedLost = do
      mbPreferred <- readPreferred bookingId
      dropped <- readPreferenceDropped bookingId
      when (mbPreferred == Just plate && not dropped) $ do
        cfg <- Config.getTunables . (.merchantOperatingCityId) =<< maybe (throwError (InternalError "booking gone")) pure mbBooking
        writePreferenceDropped (allocKeyTtl cfg) bookingId
        mapM_ (Notify.notifyReassigned Notify.CAB_PULLED) mbBooking
