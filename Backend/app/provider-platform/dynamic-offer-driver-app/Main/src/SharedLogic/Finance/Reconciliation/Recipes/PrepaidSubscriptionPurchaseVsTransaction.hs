{-
  Prepaid Subscription :: SUBSCRIPTION_PURCHASE ↔ LEDGER (consumption).

  Checks that every rupee of a subscription's granted credit
  (@planRideCredit@) is accounted for by the ledger — either consumed via
  ride debits ('RideSubscriptionDebit') or transferred to the expiry
  bucket ('ExpiryCreditTransfer'). Only SETTLED ledger entries count.

  Semantics per subscription status (mapped via 'Lifecycle'):

    * @PENDING@ / @ACTIVE@ → 'InFlight' → 'AWAITING_SETTLEMENT'. Consumption
      is still in progress; the sweep re-checks once the sub becomes
      terminal — no mismatch is raised while credits can still be used.
    * @EXPIRED@ / @EXHAUSTED@ → 'Settled' → check
      @planRideCredit == sum(settled ride debits) + sum(settled expiry
      transfers)@. Leftover credit at expiry must have landed in the
      expiry bucket, so a MISMATCH here is a real accounting gap.
    * @FAILED@ → 'Cancelled'. @srcAmount = 0@; any consumption against a
      failed sub is unexpected and surfaces as a MISMATCH.

  Ports the SUBSCRIPTION_PURCHASE_VS_SUBSCRIPTION_TRANSACTION recon from
  PR #15484's @calculateSubscriptionConsumedAmount@.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.PrepaidSubscriptionPurchaseVsTransaction
  ( recipe,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (nub)
import Data.Time (nominalDay)
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.SubscriptionPurchase as DSP
import Kernel.Prelude
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common (EsqDBFlow, HighPrecMoney, MonadFlow)
import Kernel.Types.Id (Id (..))
import qualified Lib.Finance.Domain.Types.LedgerEntry as DLE
import Lib.Finance.Reconciliation.Recipe (Recipe (..), defaultClassify)
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerExtra
import SharedLogic.Finance.Prepaid
  ( expiryCreditTransferReferenceType,
    prepaidRideDebitReferenceType,
  )
import SharedLogic.Finance.Reconciliation.SubscriptionMeta (subscriptionMetaPairs)
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SubscriptionPurchaseExtra as QSubPurchaseExtra

recipe ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Recipe m
recipe =
  Recipe
    { spec = ReconT.ReconciliationSpec ReconT.PREPAID_SUBSCRIPTION ReconT.SUBSCRIPTION_PURCHASE ReconT.LEDGER,
      chunkPlan = ReconT.ByHour,
      -- Ledger posts (ride debits + expiry transfers) happen inline with
      -- the sub lifecycle, but expiry only fires when the sub reaches
      -- EXPIRED/EXHAUSTED. Terminal transitions are what a sweep pass
      -- actually resolves, so lean on 'AWAITING_SETTLEMENT' + the sweep
      -- rather than trying to buffer around when they'll happen.
      settlementBuffer = nominalDay,
      grouping = ReconT.Individual,
      fetchSourceChunk = fetchSources,
      fetchTargetsById = fetchTargets,
      -- TODO: sweep re-fetch. Would re-run 'fetchSources'-equivalent by
      -- sub id + rebuild the composite target. Deferred; force-close on
      -- age still bounds the OPEN pool.
      fetchSourcesByIds = \_ _ -> pure [],
      sweepInterval = 4 * nominalDay,
      -- Longer maxOpenAge than internal recipes: ACTIVE subs can sit
      -- InFlight for the duration of their plan (weeks) before the sweep
      -- has anything to close them on.
      maxOpenAge = 90 * nominalDay,
      -- Data-quality orphan check: an @ExpiryCreditTransfer@ ledger entry
      -- whose referenced subscription doesn't exist in the DB at all.
      -- Chunk-boundary cases (sub was purchased in a prior chunk) are
      -- filtered out inside 'fetchOrphans' via a bulk existence check, so
      -- only genuine "expiry for a phantom sub" cases surface.
      fetchOrphanTargets = Just fetchOrphans,
      classify = defaultClassify,
      syncSourceStatus = Nothing
    }

fetchSources ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  ReconT.DateRange ->
  m [ReconT.SourceRecord]
fetchSources scope range = do
  -- All statuses (not just ACTIVE/EXPIRED/EXHAUSTED): PENDING rows must be
  -- visible as AWAITING_SETTLEMENT so the sweep revisits them once they
  -- settle; FAILED rows must be visible so an unexpected consumption on
  -- one is caught rather than silently dropped.
  purchases <-
    QSubPurchaseExtra.findAllStatusByDateRange
      (Id scope.merchantOperatingCityId)
      range.from
      range.to
  forM purchases $ \sub -> do
    extraPairs <- subscriptionMetaPairs sub
    let lifecycle = statusToLifecycle sub.status
        -- Failed subs owe zero — any consumption at all becomes a
        -- MISMATCH via the sum comparison in defaultClassify.
        srcAmt = case lifecycle of
          ReconT.Cancelled -> 0
          _ -> sub.planRideCredit
    pure $
      ReconT.SourceRecord
        { srcId = sub.id.getId,
          srcEntityId = Just sub.id.getId,
          srcPartyId = Just sub.ownerId,
          srcAmount = srcAmt,
          srcMatchKey = Just sub.id.getId,
          srcComponent = Just "SUBSCRIPTION_CONSUMPTION",
          srcMeta =
            Just . A.object $
              [ "status" .= (show sub.status :: Text),
                "purchaseTimestamp" .= sub.purchaseTimestamp,
                "planRideCredit" .= sub.planRideCredit,
                "planFee" .= sub.planFee
              ]
                <> extraPairs,
          srcTimestamp = sub.purchaseTimestamp,
          srcLifecycle = lifecycle
        }

statusToLifecycle :: DSP.SubscriptionPurchaseStatus -> ReconT.Lifecycle
statusToLifecycle = \case
  DSP.PENDING -> ReconT.InFlight
  DSP.ACTIVE -> ReconT.InFlight -- still consuming; sweep re-checks on transition
  DSP.EXPIRED -> ReconT.Settled
  DSP.EXHAUSTED -> ReconT.Settled
  DSP.FAILED -> ReconT.Cancelled

fetchTargets ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  HS.HashSet Text ->
  m [ReconT.TargetRecord]
fetchTargets _scope subIds = do
  let subIdList = HS.toList subIds
  -- Per-sub fetch: rides carry the sub id in an array column, so a bulk
  -- "give me all rides whose subscriptionPurchaseIds ∩ subIds ≠ ∅" would
  -- want a Postgres array intersection Beam doesn't expose ergonomically.
  -- TODO: add a bulk 'findAllBySubscriptionPurchaseIds' helper if this
  -- becomes a hotspot; chunk size (1h of purchases) keeps N small today.
  ridesBySub :: [(Text, [DR.Ride])] <-
    forM subIdList $ \subId -> do
      rides <- QRide.findAllBySubscriptionPurchaseId (Id subId)
      pure (subId, rides)
  let allBookingIds = concatMap (\(_, rs) -> map (.bookingId.getId) rs) ridesBySub

  -- Bulk: ride-debit ledger entries keyed by booking id.
  debitEntries <-
    QLedgerExtra.findByReferenceTypesAndReferenceIds
      [prepaidRideDebitReferenceType]
      allBookingIds
  let debitsByBooking :: HM.HashMap Text HighPrecMoney
      debitsByBooking =
        HM.fromListWith
          (+)
          [(e.referenceId, e.amount) | e <- debitEntries, e.status == DLE.SETTLED]

  -- Bulk: expiry-credit-transfer ledger entries keyed by sub id.
  expiryEntries <-
    QLedgerExtra.findByReferenceTypesAndReferenceIds
      [expiryCreditTransferReferenceType]
      subIdList
  let expiriesBySub :: HM.HashMap Text HighPrecMoney
      expiriesBySub =
        HM.fromListWith
          (+)
          [(e.referenceId, e.amount) | e <- expiryEntries, e.status == DLE.SETTLED]

  -- One composite TargetRecord per sub. tgtAmount = consumed = debits +
  -- expiries. The sum comparison in defaultClassify then checks this
  -- against the source's planRideCredit.
  pure $
    flip map ridesBySub $ \(subId, rides) ->
      let debitTotal = sum [HM.lookupDefault 0 r.bookingId.getId debitsByBooking | r <- rides]
          expiryTotal = HM.lookupDefault 0 subId expiriesBySub
          consumed = debitTotal + expiryTotal
       in ReconT.TargetRecord
            { -- No natural PK for a composite target; use the sub id so
              -- the entry's 'targetRecordId' points at something audit-
              -- friendly (the underlying entity being reconciled).
              tgtId = subId,
              tgtMatchKey = subId,
              tgtAmount = consumed,
              tgtMeta =
                Just . A.object $
                  [ "debitTotal" .= debitTotal,
                    "expiryTotal" .= expiryTotal,
                    "rideCount" .= length rides,
                    "settledOnly" .= True
                  ],
              tgtSettlementId = Nothing,
              tgtSettlementDate = Nothing,
              tgtSettlementMode = Nothing,
              tgtRrn = Nothing,
              tgtTransactionDate = Nothing
            }

-- | An ExpiryCreditTransfer ledger entry landing in the chunk whose
--   referenced subscription doesn't exist anywhere in @subscription_purchase@
--   is a data-quality issue — expired credit for a phantom subscription.
--
--   Two-step filter:
--
--     1. Skip entries whose sub is already in @seenSubIds@ (that sub was
--        picked up by 'fetchSourceChunk' this pass; not an orphan).
--     2. For the remainder, bulk-lookup @subscription_purchase@ to
--        distinguish "chunk-boundary artifact" (sub was purchased in an
--        earlier chunk, still exists) from a real phantom.
--
--   Only the phantom case surfaces as MISSING_IN_SOURCE. Chunk-boundary
--   artifacts stay out of the entry table entirely, avoiding the noise of
--   B2 sweep having to close them all as RESOLVED_BY_SOURCE later.
--
--   Ride-debit orphans (a RideSubscriptionDebit with an unknown sub) are
--   deliberately not covered — reference_id points at a booking, not a
--   sub, so the phantom check would need an extra ride → sub join.
--   Follow-up if data quality on that side matters.
fetchOrphans ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  ReconT.DateRange ->
  HS.HashSet Text ->
  m [ReconT.TargetRecord]
fetchOrphans scope range seenSubIds = do
  entries <-
    QLedgerExtra.findByReferenceTypesAndDateRange
      [expiryCreditTransferReferenceType]
      scope.merchantOperatingCityId
      range.from
      range.to
  let candidateSubIds =
        nub $
          mapMaybe
            (\e -> if HS.member e.referenceId seenSubIds then Nothing else Just e.referenceId)
            entries
  if null candidateSubIds
    then pure []
    else do
      existingSubs <- QSubPurchaseExtra.findByIds candidateSubIds
      let existingSubIdSet :: HS.HashSet Text
          existingSubIdSet = HS.fromList (map (.id.getId) existingSubs)
          -- Union with seenSubIds so entries whose sub is in this chunk's
          -- source list don't get mis-flagged as phantoms — existingSubIdSet
          -- only covers the candidate (non-seen) sub ids we looked up.
          knownSubs = HS.union seenSubIds existingSubIdSet
          phantomEntries = filter (\e -> not (HS.member e.referenceId knownSubs)) entries
      pure $ map orphanTarget phantomEntries

orphanTarget :: DLE.LedgerEntry -> ReconT.TargetRecord
orphanTarget e =
  ReconT.TargetRecord
    { tgtId = e.id.getId,
      -- Points at the phantom sub id, which the sweep uses to check for
      -- any source-side twin (there won't be one — the sub doesn't exist —
      -- so the orphan stays open until it ages out).
      tgtMatchKey = e.referenceId,
      tgtAmount = e.amount,
      tgtMeta =
        Just . A.object $
          [ "referenceType" .= e.referenceType,
            "entryType" .= (show e.entryType :: Text),
            "status" .= (show e.status :: Text),
            "phantomSubscriptionId" .= e.referenceId
          ],
      tgtSettlementId = e.settlementId,
      tgtSettlementDate = e.settledAt,
      tgtSettlementMode = Nothing,
      tgtRrn = Nothing,
      tgtTransactionDate = Just e.timestamp
    }
