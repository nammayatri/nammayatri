-- | Closing an allocation under the booking lock: back to FINDING with a bumped attempt count, or the rider's last no-show cancelling the booking.
module SharedLogic.SharedCab.Allocation.Reallocate
  ( Closed (..),
    closeLocked,
    afterClose,
    releaseCancelledBooking,
  )
where

import qualified Domain.Types.FRFSTicketBooking as DFTB
import Domain.Types.FRFSTicketBookingStatus (FRFSTicketBookingStatus (..))
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PersonPTStats as DPUS
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSCancelJourney as FRFSCancelJourney
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import qualified SharedLogic.PersonPTStats as SPUS
import SharedLogic.SharedCab.Allocation.Claim (AllocFlow, claimFallbackPush, clearAllocationKeys, readAttempts)
import SharedLogic.SharedCab.Allocation.Select (closable, crossedMaxAttempts, isRescheduledChild)
import SharedLogic.SharedCab.Allocation.Types
import SharedLogic.SharedCab.Booking (recordCancelReason, shared)
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Events as Events
import qualified SharedLogic.SharedCab.Invariants as Invariants
import SharedLogic.SharedCab.Keys (allocKey, attemptsKey, fallbackPushedKey, findingSinceKey, skippedKey)
import SharedLogic.SharedCab.LegState (CancelReason (NO_SHOW_CAP))
import qualified SharedLogic.SharedCab.NoShow as NoShow
import qualified SharedLogic.SharedCab.Notify as Notify
import SharedLogic.SharedCab.RefundDecision (Refund (..), gateByPayment, refundAmounts, refundWord)
import qualified SharedLogic.SharedCab.Session as Session
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.FRFSRecon as QFRFSRecon
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonStats as QPS

data Closed = Closed
  { closedCity :: Id DMOC.MerchantOperatingCity,
    fallbackJustTriggered :: Bool, -- this close crossed maxAttempts
    autoCancelled :: Bool, -- this close was the rider's last allowed no-show: the booking is cancelled, not FINDING
    heldTrip :: Maybe Text -- the trip the allocation was made to, from its alloc key
  }

-- | Inside the booking lock: CAS plate -> null (back to FINDING), clear the alloc key, bump attempts. Nothing when another closer won.
closeLocked ::
  (MonadFlow m, Redis.HedisFlow m r, CacheFlow m r, EsqDBFlow m r, EncFlow m r) =>
  Config.SharedCabTunables ->
  Id DFTB.FRFSTicketBooking ->
  Text ->
  AllocationOutcome ->
  m (Maybe Closed)
closeLocked cfg bookingId expectedPlate outcome =
  QFRFSTicketBooking.findById bookingId >>= \case
    Nothing -> pure Nothing
    Just b -> do
      statuses <- map (.status) <$> QFRFSTicket.findAllByTicketBookingId b.id
      if closable expectedPlate b.status b.vehicleNumber statuses then Just <$> close b else pure Nothing
  where
    close b = do
      case blameFor outcome of
        BlameRider -> QFRFSTicketBooking.releaseAllocatedVehicle Nothing (NoShow.noShowsAfter BlameRider b.sharedCabNoShows) b.id (Just expectedPlate)
        _ -> QFRFSTicketBooking.updateAllocatedVehicle Nothing b.id (Just expectedPlate)
      heldTrip <- shared $ (>>= (.vehicleTripId)) <$> Redis.safeGet @AllocationState (allocKey bookingId)
      if NoShow.actionAfterClose (blameFor outcome) cfg.maxNoShows b.sharedCabNoShows == NoShow.AutoCancel
        then do
          cancelForNoShows b
          clearAllocationKeys bookingId
          pure Closed {closedCity = b.merchantOperatingCityId, fallbackJustTriggered = False, autoCancelled = True, heldTrip}
        else reallocate b heldTrip
    reallocate b heldTrip = do
      shared $ Redis.del (allocKey bookingId)
      when (skipsPlateOnClose outcome) $ shared $ Redis.sAddExp (skippedKey bookingId) [expectedPlate] cfg.findingTimeoutSec
      -- Known limitation: a Redis flush resets attempts.
      attemptsBefore <- readAttempts (attemptsKey bookingId)
      let attemptsNow = attemptsBefore + (if countsTowardAttempts outcome then 1 else 0)
      shared $ Redis.setExp (attemptsKey bookingId) attemptsNow cfg.findingTimeoutSec
      let fallbackJustTriggered = crossedMaxAttempts cfg.maxAttempts attemptsBefore attemptsNow
      now <- getCurrentTime
      shared $ Redis.setExp (findingSinceKey bookingId) now cfg.findingTimeoutSec
      when (attemptsNow < cfg.maxAttempts) $ shared $ Redis.del (fallbackPushedKey bookingId)
      when fallbackJustTriggered $
        logWarning $ "shared-cab booking " <> bookingId.getId <> " exhausted allocation attempts (" <> show attemptsNow <> "); fallback surface"
      pure Closed {closedCity = b.merchantOperatingCityId, fallbackJustTriggered, autoCancelled = False, heldTrip}

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

-- | Side effects shared by every system cancel the tick does. Not crash-idempotent for the counters: a crash before them leaves
-- stats un-reversed rather than reversed twice.
releaseCancelledBooking :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r) => DFTB.FRFSTicketBooking -> m ()
releaseCancelledBooking b = do
  quantity <- FRFSPassOverride.ticketQuantityForBooking b
  mbPerson <- QPerson.findById b.riderId
  whenJust b.overrideAppliedEntityId $ \entityId -> do
    void . withTryCatch "sharedCab:releaseCancelledBooking:refundPassOverrideTrip" $
      FRFSPassOverride.refundPassOverrideTrip b (Id entityId) quantity
    void . withTryCatch "sharedCab:releaseCancelledBooking:releaseBookedTrip" $
      whenJust mbPerson $ \person ->
        FRFSPassOverride.releaseBookedTrip person (Id entityId) b.id.getId (fromMaybe b.createdAt b.startTime)
  void . withTryCatch "sharedCab:releaseCancelledBooking:clearPSCache" $ CQP.clearPSCache b.riderId
  case mbPerson of
    Nothing -> logError $ "shared-cab releaseCancelledBooking: person " <> b.riderId.getId <> " not found, stats reversal skipped"
    Just person -> do
      reverseStatsResult <-
        withTryCatch "sharedCab:releaseCancelledBooking:reversePersonPTStats" $ do
          purchaseEvent <- SPUS.mkPurchaseEvent person (Just b.vehicleType) b.serviceTierType DPUS.TICKET Nothing (Just quantity) b.merchantId b.merchantOperatingCityId
          SPUS.reversePurchase purchaseEvent
      either (\e -> logError $ "Failed to reverse PersonPTStats for booking " <> b.id.getId <> ": " <> show e) pure reverseStatsResult
  unless (isRescheduledChild b.parentBookingId) . void . withTryCatch "sharedCab:releaseCancelledBooking:ticketsBooked" $
    QPS.incrementTicketsBookedInEvent b.riderId (- quantity)

eventBlame :: Blame -> Events.Blame
eventBlame = \case
  BlameDriver -> Events.BlameDriver
  BlameRider -> Events.BlameRider
  BlameNone -> Events.BlameNone

afterClose :: AllocFlow m r => Config.SharedCabTunables -> Id DFTB.FRFSTicketBooking -> Text -> AllocationOutcome -> Maybe Closed -> m ()
afterClose cfg bookingId plate outcome closed =
  whenJust closed $ \Closed {closedCity = cityId, fallbackJustTriggered, autoCancelled, heldTrip} -> do
    now <- getCurrentTime
    trip <- fmap (getId . (.vehicleTripId)) <$> Session.readSession plate
    Events.emit cityId . Events.withTrip trip $
      Events.bookingEvent (Events.AllocationClosed (outcomeText outcome) (eventBlame (blameFor outcome))) bookingId.getId (Just plate) Nothing now
    Invariants.checkBooking bookingId
    Invariants.checkCab plate
    mbBooking <- QFRFSTicketBooking.findById bookingId
    withTryCatch "sharedCabMisses" (NoShow.record (blameFor outcome) (mbBooking <&> (.riderId)) (Id <$> heldTrip))
      >>= either (\e -> logError $ "shared-cab miss count bump failed for booking " <> bookingId.getId <> ": " <> show e) pure
    if autoCancelled
      then do
        decision <- maybe (pure NoRefund) (`gateByPayment` NoRefund) mbBooking
        Events.emit cityId $ Events.bookingEvent (Events.BookingCancelled "system" (refundWord decision) (Just "max_no_shows")) bookingId.getId (Just plate) Nothing now
        mapM_ (\b -> Notify.notifyBookingCancelled b.sharedCabNoShows b) mbBooking
      else forM_ (Notify.reassignReasonFor outcome) $ \reason ->
        mapM_ (Notify.notifyReassigned reason) mbBooking
    when fallbackJustTriggered $
      whenM (claimFallbackPush cfg bookingId) $
        mapM_ Notify.notifyBoardAny mbBooking
