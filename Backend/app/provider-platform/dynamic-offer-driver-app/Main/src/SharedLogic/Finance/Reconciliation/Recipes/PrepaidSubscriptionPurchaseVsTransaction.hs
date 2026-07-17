{-
  Prepaid Subscription :: SUBSCRIPTION_PURCHASE ↔ LEDGER (consumption).

  Checks that every rupee of a subscription's granted credit
  (@planRideCredit@) is accounted for on the ledger:

      planRideCredit  ==  consumed + remaining

  where

      consumed   = sum(settled RideSubscriptionDebit for the sub's rides)
                 + sum(settled ExpiryCreditTransfer for the sub)
      remaining  = 'getSubscriptionRemainingAvailableBalance' (Just for
                   ACTIVE subs — FIFO wallet slice; Nothing for terminal
                   subs, interpreted as 0)

  Discovery: every ACTIVE prepaid sub in the merchant operating city is
  reconciled each chunk, regardless of whether it had ledger activity in
  the chunk. Chunking by purchase date would silently miss subs bought
  weeks ago that are still consuming credit today; chunking by ledger
  activity would silently miss ACTIVE subs that stopped transacting when
  they shouldn't have. Reading all ACTIVE subs is the only signal that
  covers both cases cleanly.

  Semantics per subscription status (mapped via 'Lifecycle'):

    * @PENDING@ → 'InFlight' → 'AWAITING_SETTLEMENT'. No credit granted
      yet; nothing to check. Never enters the source list because
      discovery filters to ACTIVE.
    * @ACTIVE@ → 'Settled'. Check @planRideCredit == consumed + remaining@.
    * @EXPIRED@ / @EXHAUSTED@ / @FAILED@: excluded from discovery, since
      the sub is out of scope for ongoing consumption checks. A sub that
      just transitioned to terminal status in this chunk is caught on
      the next sweep pass — its previous MATCHED entry is closed with
      @closeReason = "AGED_OUT"@ eventually.

  Ports the SUBSCRIPTION_PURCHASE_VS_SUBSCRIPTION_TRANSACTION recon from
  PR #15484's @calculateSubscriptionConsumedAmount@, extended to include
  @remaining@ so long-running ACTIVE subs are reconciled continuously.
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
import qualified Domain.Types.Extra.Plan as DPlan
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
  ( attributableRideDebitAmount,
    expiryCreditTransferReferenceType,
    getSubscriptionRemainingAvailableBalance,
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
      -- the sub lifecycle, so a 1-day buffer is comfortable.
      settlementBuffer = nominalDay,
      grouping = ReconT.Individual,
      fetchSourceChunk = fetchSources,
      fetchTargetsById = fetchTargets,
      -- Sweep re-fetches subs by id and rebuilds SourceRecord shape.
      fetchSourcesByIds = fetchSourcesByIdsImpl,
      sweepInterval = 4 * nominalDay,
      -- Only PENDING subs and unresolved mismatches remain OPEN — ACTIVE
      -- subs match at chunk time via the @remaining@ term.
      maxOpenAge = 90 * nominalDay,
      -- Data-quality orphan check: expired credit for a phantom sub.
      fetchOrphanTargets = Just fetchOrphans,
      classify = defaultClassify,
      syncSourceStatus = Nothing
    }

-- ─── Chunk-time discovery: all ACTIVE prepaid subs ────────────────────

fetchSources ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  ReconT.DateRange ->
  m [ReconT.SourceRecord]
fetchSources scope _range = do
  subs <-
    QSubPurchaseExtra.findAllActiveInMerchantOpCityAndServiceName
      (Id scope.merchantOperatingCityId)
      DPlan.PREPAID_SUBSCRIPTION
  buildSourceRecords subs

-- | Shared 'SourceRecord' builder — same shape whether the sub came from
--   chunk-time discovery or from the sweep's 'fetchSourcesByIds' re-fetch.
buildSourceRecords ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  [DSP.SubscriptionPurchase] ->
  m [ReconT.SourceRecord]
buildSourceRecords subs = forM subs $ \sub -> do
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

-- | Sweep re-fetch. OPEN entries already carry the sub id in
--   'sourceRecordId', so a direct 'findByIds' + shared builder is enough.
fetchSourcesByIdsImpl ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  [Text] ->
  m [ReconT.SourceRecord]
fetchSourcesByIdsImpl _scope [] = pure []
fetchSourcesByIdsImpl _scope subIds = do
  subs <- QSubPurchaseExtra.findByIds subIds
  buildSourceRecords subs

statusToLifecycle :: DSP.SubscriptionPurchaseStatus -> ReconT.Lifecycle
statusToLifecycle = \case
  DSP.PENDING -> ReconT.InFlight -- no credit granted yet; nothing to check
  DSP.ACTIVE -> ReconT.Settled -- reconciled continuously via (consumed + remaining)
  DSP.EXPIRED -> ReconT.Settled
  DSP.EXHAUSTED -> ReconT.Settled
  DSP.FAILED -> ReconT.Cancelled

-- ─── Targets: consumed + remaining, per sub ───────────────────────────

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

  -- Sub rows are needed for the remaining-balance calc: only ACTIVE subs
  -- have a live wallet slice; terminal statuses return Nothing.
  subs <- QSubPurchaseExtra.findByIds subIdList
  let subsById :: HM.HashMap Text DSP.SubscriptionPurchase
      subsById = HM.fromList [(s.id.getId, s) | s <- subs]

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
  let settledDebits = filter ((== DLE.SETTLED) . (.status)) debitEntries
      debitsByBookingId :: HM.HashMap Text [DLE.LedgerEntry]
      debitsByBookingId =
        HM.fromListWith (++) [(e.referenceId, [e]) | e <- settledDebits]

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

  -- One composite TargetRecord per sub. tgtAmount = consumed + remaining,
  -- so the sum-check in defaultClassify hits @planRideCredit@ regardless
  -- of whether the sub is still ACTIVE or has reached EXPIRED/EXHAUSTED.
  forM ridesBySub $ \(subId, rides) -> do
    debitTotal <-
      fmap sum $
        forM rides $ \ride -> do
          let entries = HM.lookupDefault [] ride.bookingId.getId debitsByBookingId
          fmap sum $ forM entries $ attributableRideDebitAmount (Id subId)
    let expiryTotal = HM.lookupDefault 0 subId expiriesBySub
        consumed = debitTotal + expiryTotal
    mbRemaining <- case HM.lookup subId subsById of
      Just sub -> getSubscriptionRemainingAvailableBalance sub consumed
      Nothing -> pure Nothing
    let remaining = fromMaybe 0 mbRemaining
        accountedFor = consumed + remaining
    pure $
      ReconT.TargetRecord
        { -- No natural PK for a composite target; use the sub id so the
          -- entry's 'targetRecordId' points at something audit-friendly.
          tgtId = subId,
          tgtMatchKey = subId,
          tgtAmount = accountedFor,
          tgtMeta =
            Just . A.object $
              [ "debitTotal" .= debitTotal,
                "expiryTotal" .= expiryTotal,
                "remainingRideCredit" .= remaining,
                "consumed" .= consumed,
                "accountedFor" .= accountedFor,
                "rideCount" .= length rides,
                "settledOnly" .= True
              ],
          tgtSettlementId = Nothing,
          tgtSettlementDate = Nothing,
          tgtSettlementMode = Nothing,
          tgtRrn = Nothing,
          tgtTransactionDate = Nothing
        }

-- ─── Orphan: phantom-subscription expiry credit ───────────────────────

-- | An ExpiryCreditTransfer ledger entry landing in the chunk whose
--   referenced subscription doesn't exist anywhere in @subscription_purchase@
--   is a data-quality issue — expired credit for a phantom subscription.
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
          -- source list don't get mis-flagged as phantoms.
          knownSubs = HS.union seenSubIds existingSubIdSet
          phantomEntries = filter (\e -> not (HS.member e.referenceId knownSubs)) entries
      pure $ map orphanTarget phantomEntries

orphanTarget :: DLE.LedgerEntry -> ReconT.TargetRecord
orphanTarget e =
  ReconT.TargetRecord
    { tgtId = e.id.getId,
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
