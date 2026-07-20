{-
  Shared enrichment helper for recon recipes whose source or target is a
  SubscriptionPurchase row. Emits a set of Aeson key-value pairs that
  callers splice into their srcMeta / tgtMeta blob.

  Fields produced (all persisted inside 'entityMeta' JSONB on the entry):

    * subscriptionAmountExclGst   — from the matching IndirectTaxTransaction's
                                    taxableValue; falls back to planFee when
                                    no tax txn exists.
    * gstOnSubscription           — IndirectTaxTransaction.totalGstAmount.
    * totalTransactionAmount      — amountExclGst + gst.
    * planName                    — Plan.name resolved via subscription.planId.
    * remainingSubscriptionBalance — result of
                                    'getSubscriptionRemainingAvailableBalance',
                                    with the settled-consumed total derived
                                    from RideSubscriptionDebit +
                                    ExpiryCreditTransfer ledger entries.

  Perf note: three extra queries per subscription (plan, indirect tax, ledger).
  At hourly chunk sizes the volume is bounded; if this ever gets tight we
  batch-load these up-front in the calling recipe.
-}
module SharedLogic.Finance.Reconciliation.SubscriptionMeta
  ( subscriptionMetaPairs,
  )
where

import Data.Aeson ((.=))
import Data.Aeson.Types (Pair)
import qualified Domain.Types.SubscriptionPurchase as DSP
import Kernel.Prelude
import Kernel.Types.CacheFlow (CacheFlow)
import Kernel.Types.Common (EsqDBFlow, MonadFlow)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.IndirectTaxTransaction as QIndirectTax
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerExtra
import SharedLogic.Finance.Prepaid
  ( expiryCreditTransferReferenceType,
    getSubscriptionRemainingAvailableBalance,
    prepaidRideDebitReferenceType,
  )
import qualified Storage.Queries.Plan as QPlan

subscriptionMetaPairs ::
  (BeamFlow m r, CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  DSP.SubscriptionPurchase ->
  m [Pair]
subscriptionMetaPairs sub = do
  mbPlan <- QPlan.findByPrimaryKey sub.planId
  taxTxns <- QIndirectTax.findByReferenceId sub.id.getId
  consumedEntries <-
    QLedgerExtra.findByReferenceIn
      [prepaidRideDebitReferenceType, expiryCreditTransferReferenceType]
      sub.id.getId
  let settledConsumed = sum (map (.amount) consumedEntries)
  mbRemaining <- getSubscriptionRemainingAvailableBalance sub settledConsumed
  let mbTaxTxn = listToMaybe taxTxns
      amountExclGst = maybe sub.planFee (.taxableValue) mbTaxTxn
      gst = maybe 0 (.totalGstAmount) mbTaxTxn
      totalInclGst = amountExclGst + gst
  pure
    [ "subscriptionAmountExclGst" .= amountExclGst,
      "gstOnSubscription" .= gst,
      "totalTransactionAmount" .= totalInclGst,
      "planName" .= fmap (.name) mbPlan,
      "remainingSubscriptionBalance" .= mbRemaining
    ]
