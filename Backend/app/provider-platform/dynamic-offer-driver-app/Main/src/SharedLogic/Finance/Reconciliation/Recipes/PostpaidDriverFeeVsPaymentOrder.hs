{-
  Postpaid Subscription :: DRIVER_FEE ↔ PAYMENT_ORDER.

  For every driver_fee created in the chunk, the winning invoice is picked
  from atlas_driver_offer_bpp.invoice (SUCCESS preferred, else latest by
  COALESCE(updated_at, created_at)), and that invoice's ID is used as the
  join key to atlas_driver_offer_bpp.payment_order (invoice.id ==
  payment_order.id in this schema).

  Grouping is GroupByTargetKey because a single payment_order can settle
  many driver_fees; the recon check verifies that
      sum(driver_fee.amount for fees sharing a PO) == payment_order.amount

  Component is derived from fee_type:
    * MANDATE_REGISTRATION       -> "PENNY_DROP"
    * RECURRING_INVOICE / other  -> "SUBSCRIPTION_INVOICE"
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Finance.Reconciliation.Recipes.PostpaidDriverFeeVsPaymentOrder
  ( recipe,
  )
where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Time (nominalDay)
import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.Invoice as DInvoice
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.Finance.Reconciliation.Recipe (Recipe (..), defaultClassify)
import qualified Lib.Finance.Reconciliation.Types as ReconT
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Storage.Queries.PaymentOrder as QPaymentOrder
import qualified Storage.Queries.DriverFee as QDriverFee
import qualified Storage.Queries.Invoice as QInvoice

recipe ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  Recipe m
recipe =
  Recipe
    { spec = ReconT.ReconciliationSpec ReconT.POSTPAID_SUBSCRIPTION ReconT.DRIVER_FEE ReconT.PAYMENT_ORDER,
      chunkPlan = ReconT.ByHour,
      -- Internal-only recon: invoice / payment_order rows land inline
      -- with the driver_fee lifecycle.
      settlementBuffer = nominalDay,
      grouping = ReconT.GroupByTargetKey,
      fetchSourceChunk = fetchSources,
      fetchTargetsById = fetchTargets,
      -- TODO: sweep re-fetch (bulk driver_fees by id + winning-invoice pick)
      -- — deferred; force-close on age still works.
      fetchSourcesByIds = \_ _ -> pure [],
      sweepInterval = 4 * nominalDay,
      maxOpenAge = 30 * nominalDay,
      fetchOrphanTargets = Nothing,
      classify = defaultClassify,
      syncSourceStatus = Nothing
    }

-- ─── Sources ───────────────────────────────────────────────────────────────

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
  -- Query 1: driver_fees created in the chunk hour.
  -- TODO(recipe author): if a matching helper doesn't exist yet, add
  -- `findByMerchantAndCreatedAtRange` to Storage.Queries.DriverFeeExtra —
  -- selectivity is on (merchant_id, created_at) so the new compound index
  -- from postpaidReports migration covers it.
  fees <- QDriverFee.findByMerchantAndCreatedAtRange scope.merchantId scope.merchantOperatingCityId range.from range.to

  -- Query 2: bulk-fetch every invoice for the chunk's fees in a single
  -- indexed WHERE driver_fee_id = ANY(?), then pick the winner in-app.
  let feeIds = map (.id) fees
  allInvoices <- QInvoice.findByDriverFeeIds feeIds
  let winners :: HM.HashMap Text DInvoice.Invoice
      winners = HM.fromListWith preferBetter [(inv.driverFeeId.getId, inv) | inv <- allInvoices]

  pure
    [ let fid = df.id.getId
          mbWinner = HM.lookup fid winners
          poId = (\inv -> inv.id.getId) <$> mbWinner
       in ReconT.SourceRecord
            { srcId = fid,
              srcEntityId = Just fid,
              srcPartyId = Just df.driverId.getId,
              srcAmount = platformCollection df,
              srcMatchKey = poId, -- invoice.id == payment_order.id
              srcComponent = Just (componentLabel df.feeType),
              srcMeta =
                Just $
                  A.object
                    [ "feeType" .= df.feeType,
                      "startTime" .= df.startTime,
                      "createdAt" .= df.createdAt,
                      "invoiceShortId" .= ((\inv -> inv.invoiceShortId) <$> mbWinner)
                    ],
              srcTimestamp = df.createdAt,
              srcLifecycle = ReconT.Settled -- driver_fee rows in the chunk are already realised.
            }
      | df <- fees
    ]

-- ─── Targets ───────────────────────────────────────────────────────────────

fetchTargets ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  ReconT.MerchantScope ->
  HS.HashSet Text ->
  m [ReconT.TargetRecord]
fetchTargets _scope poIds = do
  -- Single bulk fetch instead of one findById per payment_order.
  pos <- QPaymentOrder.findAllByIds (map Id (HS.toList poIds))
  pure
    [ ReconT.TargetRecord
        { tgtId = po.id.getId,
          tgtMatchKey = po.id.getId,
          tgtAmount = po.amount,
          tgtMeta =
            Just $
              A.object
                [ "status" .= (show po.status :: Text),
                  "shortId" .= po.shortId.getShortId,
                  "createdAt" .= po.createdAt
                ],
          tgtSettlementId = Nothing,
          tgtSettlementDate = Nothing,
          tgtSettlementMode = Nothing,
          tgtRrn = Nothing,
          tgtTransactionDate = Just po.createdAt
        }
      | po <- pos
    ]

-- ─── Pure helpers ─────────────────────────────────────────────────────────

-- | Prefer status = SUCCESS; else the latest by updated_at.
--   Semantic matches the CTE tiebreak we use on the postpaid Revenue Reports
--   backend (server/src/services/postgres/postpaidReports.ts). Consumed by
--   HM.fromListWith to keep the winning invoice per driver_fee.
preferBetter :: DInvoice.Invoice -> DInvoice.Invoice -> DInvoice.Invoice
preferBetter new old
  | isSuccess new && not (isSuccess old) = new
  | not (isSuccess new) && isSuccess old = old
  | recencyOf new > recencyOf old = new
  | otherwise = old
  where
    isSuccess inv = (show inv.invoiceStatus :: Text) == "SUCCESS"
    recencyOf inv = inv.updatedAt

platformCollection :: DDF.DriverFee -> HighPrecMoney
platformCollection df =
  df.platformFee.fee + df.platformFee.cgst + df.platformFee.sgst + fromMaybe 0 df.cancellationPenaltyAmount

componentLabel :: DDF.FeeType -> Text
componentLabel ft = case (show ft :: Text) of
  "MANDATE_REGISTRATION" -> "PENNY_DROP"
  _ -> "SUBSCRIPTION_INVOICE"
