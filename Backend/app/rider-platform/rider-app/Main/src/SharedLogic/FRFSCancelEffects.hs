-- | What a cancelled FRFS booking gives back: the pass trip, the segmentation counters and the rider's cached stats. One copy,
-- for the BPP cancel callback and for the system cancels (shared-cab no-show, finding timeout), so they cannot drift.
module SharedLogic.FRFSCancelEffects (releaseCancelledBooking) where

import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.Person as DP
import qualified Domain.Types.PersonPTStats as DPUS
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.FRFSPassOverride as FRFSPassOverride
import qualified SharedLogic.PersonPTStats as SPUS
import qualified Storage.CachedQueries.Person as CQP
import qualified Storage.Queries.PersonStats as QPS

-- | @refundTrip@: the pass trip went out at confirm and comes back (the caller decides whether the booking ever spent it).
-- @decrementTicketsBooked@: the rider's tickets-booked counter is reversed (a rescheduled child never counted its own).
-- Each step but the last is tried alone, so one failure skips nothing after it. Not crash-idempotent for the counters.
releaseCancelledBooking ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m, EncFlow m r) =>
  Bool ->
  Bool ->
  DFRFSTicketBooking.FRFSTicketBooking ->
  Maybe DP.Person ->
  Int ->
  m ()
releaseCancelledBooking refundTrip decrementTicketsBooked booking mbPerson quantity = do
  whenJust booking.overrideAppliedEntityId $ \entityId -> do
    when refundTrip $
      void . withTryCatch "FRFSCancelEffects:refundPassOverrideTrip" $
        FRFSPassOverride.refundPassOverrideTrip booking (Id entityId) quantity
    void . withTryCatch "FRFSCancelEffects:releaseBookedTrip" $
      whenJust mbPerson $ \person ->
        FRFSPassOverride.releaseBookedTrip person (Id entityId) booking.id.getId (fromMaybe booking.createdAt booking.startTime)
  void . withTryCatch "FRFSCancelEffects:clearPSCache" $ CQP.clearPSCache booking.riderId
  case mbPerson of
    Nothing -> logError $ "FRFSCancelEffects: person " <> booking.riderId.getId <> " not found, stats reversal skipped"
    Just person ->
      withTryCatch "FRFSCancelEffects:reversePersonPTStats" (reverseStats person)
        >>= either (\e -> logError $ "Failed to reverse PersonPTStats for booking " <> booking.id.getId <> ": " <> show e) pure
  when decrementTicketsBooked . void $ QPS.incrementTicketsBookedInEvent booking.riderId (negate quantity)
  where
    reverseStats person = do
      purchaseEvent <- SPUS.mkPurchaseEvent person (Just booking.vehicleType) booking.serviceTierType DPUS.TICKET Nothing (Just quantity) booking.merchantId booking.merchantOperatingCityId
      SPUS.reversePurchase purchaseEvent
