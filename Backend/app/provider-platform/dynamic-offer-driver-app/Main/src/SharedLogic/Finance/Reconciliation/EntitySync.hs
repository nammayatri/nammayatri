{-
  Shared implementations of the framework's syncSourceStatus hook, one per
  entity table that carries a reconciliationStatus JSON map.

  Recipe modules pass their own ReconciliationSpec into these helpers so
  the map is keyed correctly. Example wiring inside a Recipe:

      let mySpec = ReconT.ReconciliationSpec ...
      Recipe
        { spec = mySpec,
          ...
          syncSourceStatus = Just (EntitySync.syncBookingStatus mySpec)
        }

  New entity types get their own helper here — keep the framework's Recipe
  API generic; recipes stay one line to wire.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.EntitySync
  ( syncBookingStatus,
    syncSubscriptionStatus,
  )
where

import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Reconciliation.StatusMap as StatusMap
import qualified Lib.Finance.Reconciliation.Types as ReconT
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.SubscriptionPurchase as QSubPurchase

-- | Merge @(specKey -> status)@ into a booking's @reconciliationStatus@
--   JSON map. No-op when 'srcEntityId' is Nothing or the booking has been
--   deleted since the chunk started.
syncBookingStatus ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.ReconciliationSpec ->
  ReconT.SourceRecord ->
  ReconT.ReconciliationStatus ->
  m ()
syncBookingStatus spec src status = case src.srcEntityId of
  Nothing -> pure ()
  Just entityId -> do
    mbBooking <- QBooking.findById (Id entityId)
    forM_ mbBooking $ \booking -> do
      let existing = StatusMap.fromJsonValue booking.reconciliationStatus
          updated = StatusMap.upsertBySpec spec status existing
      QBooking.updateReconciliationStatus booking.id (Just (StatusMap.toJsonValue updated))

-- | Same shape, but for subscription_purchase.
syncSubscriptionStatus ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.ReconciliationSpec ->
  ReconT.SourceRecord ->
  ReconT.ReconciliationStatus ->
  m ()
syncSubscriptionStatus spec src status = case src.srcEntityId of
  Nothing -> pure ()
  Just entityId -> do
    mbSub <- QSubPurchase.findByPrimaryKey (Id entityId)
    forM_ mbSub $ \sub -> do
      let existing = StatusMap.fromJsonValue sub.reconciliationStatus
          updated = StatusMap.upsertBySpec spec status existing
      QSubPurchase.updateReconciliationStatus (Just (StatusMap.toJsonValue updated)) sub.id
