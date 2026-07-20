{-
  Prepaid Subscription :: DSSR ↔ SubscriptionCredit ledger.

  For every ACTIVE subscription_purchase in the chunk (keyed on
  purchaseTimestamp):
    * expected = subscription.planRideCredit
    * actual   = first ledger entry with reference_type = 'SubscriptionCredit'
                 and reference_id = subscription.id

  Ports doReconciliationDssrVsSubscription + processDssrVsSubscription from
  the retired SharedLogic.Finance.Reconciliation module.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.PrepaidDssrVsSubscription
  ( recipe,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Time (nominalDay)
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.LedgerEntry as LedgerEntry
import Lib.Finance.Reconciliation.Recipe (Recipe (..), defaultClassify)
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerExtra
import SharedLogic.Finance.Prepaid (subscriptionCreditReferenceType)
import qualified SharedLogic.Finance.Reconciliation.EntitySync as EntitySync
import SharedLogic.Finance.Reconciliation.SubscriptionMeta (subscriptionMetaPairs)
import qualified Storage.Queries.SubscriptionPurchase as QSubPurchase

recipe ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Recipe m
recipe =
  Recipe
    { spec = mySpec,
      chunkPlan = ReconT.ByHour,
      -- Internal-only recon: SubscriptionCredit ledger posts inline.
      settlementBuffer = nominalDay,
      grouping = ReconT.Individual,
      fetchSourceChunk = fetchSources,
      fetchTargetsById = fetchTargets,
      -- TODO: sweep re-fetch (rebuild SourceRecord from ACTIVE subs by id
      -- + subscriptionMetaPairs) — deferred; force-close on age still works.
      fetchSourcesByIds = \_ _ -> pure [],
      sweepInterval = 4 * nominalDay,
      maxOpenAge = 30 * nominalDay,
      fetchOrphanTargets = Nothing,
      classify = defaultClassify,
      syncSourceStatus = Just (EntitySync.syncSubscriptionStatus mySpec)
    }
  where
    mySpec = ReconT.ReconciliationSpec ReconT.PREPAID_SUBSCRIPTION ReconT.DSSR ReconT.SUBSCRIPTION_PURCHASE

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
  subs <- QSubPurchase.findActiveByDateRange (Id scope.merchantOperatingCityId) range.from range.to
  forM subs $ \sub -> do
    extraPairs <- subscriptionMetaPairs sub
    let sid = sub.id.getId
    pure $
      ReconT.SourceRecord
        { srcId = sid,
          srcEntityId = Just sid,
          srcPartyId = Just sub.ownerId,
          srcAmount = sub.planRideCredit,
          srcMatchKey = Just sid,
          srcComponent = Just "SUBSCRIPTION_PURCHASE",
          srcMeta =
            Just . A.object $
              [ "planFee" .= sub.planFee,
                "purchaseTimestamp" .= sub.purchaseTimestamp
              ]
                <> extraPairs,
          srcTimestamp = sub.createdAt,
          srcLifecycle = ReconT.Settled -- ACTIVE subscriptions are past the purchase step.
        }

fetchTargets ::
  ( BeamFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  HS.HashSet Text ->
  m [ReconT.TargetRecord]
fetchTargets _scope subIds = do
  entries <-
    QLedgerExtra.findByReferenceTypesAndReferenceIds
      [subscriptionCreditReferenceType]
      (HS.toList subIds)
  let firstBySub :: HM.HashMap Text LedgerEntry.LedgerEntry
      firstBySub = HM.fromListWith (\_new old -> old) [(e.referenceId, e) | e <- entries]
  pure
    [ ReconT.TargetRecord
        { tgtId = e.id.getId,
          tgtMatchKey = e.referenceId,
          tgtAmount = e.amount,
          tgtMeta = Just $ A.object ["referenceType" .= (e.referenceType :: Text)],
          tgtSettlementId = Nothing,
          tgtSettlementDate = Nothing,
          tgtSettlementMode = Nothing,
          tgtRrn = Nothing,
          tgtTransactionDate = Just e.createdAt
        }
      | (_, e) <- HM.toList firstBySub
    ]
