module Domain.Action.Beckn.ReceiverRecon
  ( ReceiverReconRequest (..),
    ReceiverReconOrder (..),
    SettlementDetailParsed (..),
    ingestReceiverRecon,
    reconcileIngestedOrders,
  )
where

import qualified Data.HashSet as HS
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as RSO
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement as RUS
import qualified Lib.Finance.Reconciliation.Runner as ReconRunner
import qualified Lib.Finance.Reconciliation.Types as ReconT
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrder as QRSO
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrderExtra as QRSOExtra
import qualified Lib.Finance.Storage.Queries.ReconUtrSettlement as QRUS
import qualified Lib.Finance.Storage.Queries.ReconUtrSettlementExtra as QRUSExtra
import qualified SharedLogic.Finance.Reconciliation.Recipes.RsfBapClaimVsPlatformRide as RsfRecipe

data ReceiverReconRequest = ReceiverReconRequest
  { bapId :: Text,
    bapUri :: Text,
    messageId :: Text,
    reconTransactionId :: Text,
    deadline :: UTCTime,
    orders :: [ReceiverReconOrder]
  }

data ReceiverReconOrder = ReceiverReconOrder
  { orderId :: Text,
    orderTransactionId :: Text,
    invoiceNo :: Maybe Text,
    orderState :: Text,
    claimedGrossAmount :: HighPrecMoney,
    claimedSettlementAmount :: HighPrecMoney,
    paymentStatus :: Text,
    settlementId :: Text,
    settlementReferenceNo :: Text,
    reasonCode :: Text,
    wireReconStatus :: Text,
    wireOrderReconStatus :: Text,
    bffType :: Maybe Text,
    bffAmount :: Maybe HighPrecMoney,
    withholdingTaxGst :: Maybe HighPrecMoney,
    withholdingTaxTds :: Maybe HighPrecMoney,
    deductionByCollector :: Maybe HighPrecMoney,
    rawJson :: Text,
    settlementDetails :: [SettlementDetailParsed]
  }

data SettlementDetailParsed = SettlementDetailParsed
  { utr :: Text,
    amount :: HighPrecMoney,
    status :: Text,
    sdSettlementType :: Text,
    sdSettlementDate :: UTCTime
  }

ingestReceiverRecon ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ReceiverReconRequest ->
  Flow ()
ingestReceiverRecon merchantId merchantOpCityId req =
  -- serialized check with no race condition on multi batched orders
  acquireAll lockPrefix orderIds ingestBody
  where
    -- sorted orders will avoid deadlock situation between two batches [o1, o2] and [o2, o1]
    orderIds = sort . HS.toList . HS.fromList $ map (.orderId) req.orders
    lockPrefix = "RsfIngestLock:" <> merchantId.getId <> "|"

    acquireAll _ [] action = action
    acquireAll prefix (oid : rest) action =
      Hedis.withLockRedis (prefix <> oid) 60 $
        acquireAll prefix rest action

    ingestBody = do
      now <- getCurrentTime
      let merchantIdText = Just merchantId.getId
          merchantOpCityIdText = Just merchantOpCityId.getId

      existingRows <- QRSOExtra.findByOrderIds orderIds
      let existingKeys = HS.fromList [(r.orderId, r.settlementReferenceNo) | r <- existingRows]
          incomingPairs =
            [ (order, detail)
              | order <- req.orders,
                detail <- order.settlementDetails
            ]
          newPairs =
            snd $
              foldl'
                ( \(seen, acc) pair@(order, detail) ->
                    let key = (order.orderId, detail.utr)
                     in if HS.member key seen
                          then (seen, acc)
                          else (HS.insert key seen, acc <> [pair])
                )
                (existingKeys, [])
                incomingPairs
          skippedCount = length incomingPairs - length newPairs

      when (skippedCount > 0) $
        logWarning $
          "RSF ingest: skipped "
            <> show skippedCount
            <> " already-claimed (order,UTR) pair(s), messageId="
            <> req.messageId

      let utrGroups = groupDetailsByUtr newPairs
      utrIdMap <- fmap Map.fromList . forM (Map.toList utrGroups) $ \(utrVal, pairs) -> do
        utrId <- generateGUID
        QRUSExtra.upsertByUtr $
          RUS.ReconUtrSettlement
            { id = utrId,
              merchantId = merchantIdText,
              merchantOperatingCityId = merchantOpCityIdText,
              utr = utrVal,
              bapId = req.bapId,
              bapUri = req.bapUri,
              claimedTotalAmount = sum (map ((.amount) . snd) pairs),
              totalOrders = length pairs,
              bankVerifiedAmount = Nothing,
              resolvedAt = Nothing,
              resolvedBy = Nothing,
              createdAt = now,
              updatedAt = now
            }
        actualId <- maybe utrId (.id) <$> QRUS.findByUtr utrVal
        pure (utrVal, actualId)

      orderRows <- forM newPairs $ \(order, detail) -> do
        rowId <- generateGUID
        pure
          RSO.ReconSettlementOrder
            { id = rowId,
              merchantId = merchantIdText,
              merchantOperatingCityId = merchantOpCityIdText,
              utrSettlementId = Map.lookup detail.utr utrIdMap,
              sourceType = Just RSO.BAP_CLAIMED,
              orderId = order.orderId,
              messageId = req.messageId,
              reconTransactionId = req.reconTransactionId,
              orderTransactionId = order.orderTransactionId,
              invoiceNo = order.invoiceNo,
              orderState = order.orderState,
              settlementId = order.settlementId,
              settlementReferenceNo = detail.utr,
              reasonCode = order.reasonCode,
              claimedGrossAmount = order.claimedGrossAmount,
              claimedSettlementAmount = detail.amount,
              paymentStatus = order.paymentStatus,
              settlementType = detail.sdSettlementType,
              settlementDate = detail.sdSettlementDate,
              bffType = order.bffType,
              bffAmount = order.bffAmount,
              withholdingTaxGst = order.withholdingTaxGst,
              withholdingTaxTds = order.withholdingTaxTds,
              deductionByCollector = order.deductionByCollector,
              receivedAt = now,
              rideId = Nothing,
              driverId = Nothing,
              platformGrossFare = Nothing,
              platformNetReceivable = Nothing,
              platformOrderTimestamp = Nothing,
              allocatedBankCash = Nothing,
              reconciliationStatus = Nothing,
              wireReconStatus = order.wireReconStatus,
              wireOrderReconStatus = order.wireOrderReconStatus,
              settlementClearedAt = Nothing,
              manuallyConfirmedAt = Nothing,
              manuallyConfirmedBy = Nothing,
              manualConfirmationReason = Nothing,
              ourReconStatus = RSO.PENDING,
              diffAmount = Nothing,
              remarks = Nothing,
              rawJson = order.rawJson,
              createdAt = now,
              updatedAt = now
            }

      QRSO.createMany orderRows

      logInfo $
        "RSF ingest complete: messageId="
          <> req.messageId
          <> " rows="
          <> show (length orderRows)
          <> " orders="
          <> show (length req.orders)

reconcileIngestedOrders ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ReceiverReconRequest ->
  Flow ()
reconcileIngestedOrders merchantId merchantOpCityId req = do
  let scope = ReconT.MerchantScope merchantId.getId merchantOpCityId.getId
      sourceIds = map (ReconT.SourceId . (.orderId)) req.orders
  unless (null sourceIds) $
    ReconRunner.reconcileSources RsfRecipe.recipe scope sourceIds
  logInfo $
    "RSF recon complete: messageId="
      <> req.messageId
      <> " orders="
      <> show (length req.orders)

groupDetailsByUtr :: [(ReceiverReconOrder, SettlementDetailParsed)] -> Map.Map Text [(ReceiverReconOrder, SettlementDetailParsed)]
groupDetailsByUtr = foldl' (\acc pair -> Map.insertWith (<>) ((.utr) . snd $ pair) [pair] acc) Map.empty
