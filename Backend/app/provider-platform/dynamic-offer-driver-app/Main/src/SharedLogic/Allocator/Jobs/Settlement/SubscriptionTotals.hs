module SharedLogic.Allocator.Jobs.Settlement.SubscriptionTotals
  ( SubscriptionTotals (..),
    fetchSubscriptionTotals,
    fetchPgReportsInBatches,
  )
where

import qualified Data.List as DL
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.PgPaymentSettlementReport as Dom
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Beam.IndirectTaxTransaction as BeamITT
import qualified Lib.Finance.Storage.Queries.PgPaymentSettlementReport as QPgReport
import qualified Storage.Beam.SubscriptionPurchase as BeamSP
import qualified Storage.Queries.SubscriptionPurchase as QSP

batchSize :: Int
batchSize = 1000

-- ---------------------------------------------------------------------------
-- Subscription purchase totals (aggregated)
-- ---------------------------------------------------------------------------

data SubscriptionTotals = SubscriptionTotals
  { grossAmount :: HighPrecMoney,
    cgst :: HighPrecMoney,
    sgst :: HighPrecMoney,
    igst :: HighPrecMoney,
    netAmount :: HighPrecMoney
  }

emptyTotals :: SubscriptionTotals
emptyTotals = SubscriptionTotals 0 0 0 0 0

fetchSubscriptionTotals ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  UTCTime ->
  m SubscriptionTotals
fetchSubscriptionTotals merchantOpCityId fromTime toTime = go 0 emptyTotals
  where
    go offset acc = do
      rows <- QSP.findActiveWithTaxByDateRange merchantOpCityId fromTime toTime batchSize offset
      if null rows
        then pure acc
        else do
          let newAcc = DL.foldl' addRow acc rows
          if length rows < batchSize
            then pure newAcc
            else go (offset + batchSize) newAcc

    addRow :: SubscriptionTotals -> (BeamSP.SubscriptionPurchase, Maybe BeamITT.IndirectTaxTransaction) -> SubscriptionTotals
    addRow acc (sp, mbItt) =
      case mbItt of
        Just itt ->
          SubscriptionTotals
            { grossAmount = acc.grossAmount + sp.planFee,
              cgst = acc.cgst + itt.cgstAmount,
              sgst = acc.sgst + itt.sgstAmount,
              igst = acc.igst + itt.igstAmount,
              netAmount = acc.netAmount + itt.taxableValue
            }
        Nothing ->
          acc{grossAmount = acc.grossAmount + sp.planFee,
              netAmount = acc.netAmount + sp.planFee
             }

-- ---------------------------------------------------------------------------
-- PG settlement reports (batched fetch)
-- ---------------------------------------------------------------------------

fetchPgReportsInBatches ::
  (BeamFlow m r) =>
  Text ->
  UTCTime ->
  UTCTime ->
  m [Dom.PgPaymentSettlementReport]
fetchPgReportsInBatches mId fromTime toTime = go 0 []
  where
    go offset acc = do
      batch <- QPgReport.findByMerchantIdAndDateRangeWithLimitOffset mId fromTime toTime batchSize offset
      if null batch
        then pure acc
        else do
          let newAcc = acc <> batch
          if length batch < batchSize
            then pure newAcc
            else go (offset + batchSize) newAcc
