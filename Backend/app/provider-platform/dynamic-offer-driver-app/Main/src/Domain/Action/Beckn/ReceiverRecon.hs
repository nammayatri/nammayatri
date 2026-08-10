module Domain.Action.Beckn.ReceiverRecon
  ( ReceiverReconRequest (..),
    ReceiverReconOrder (..),
    SettlementDetailParsed (..),
    handleReceiverRecon,
  )
where

import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime (..))
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import Kernel.Types.Common ()
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as RSO
import qualified Lib.Finance.Domain.Types.ReconUtrSettlement as RUS
import qualified Lib.Finance.Reconciliation.Runner as ReconRunner
import qualified Lib.Finance.Reconciliation.Types as ReconT
import qualified Lib.Finance.Storage.Queries.ReconSettlementOrder as QRSO
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
    settlementType :: Text,
    settlementDate :: UTCTime,
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

handleReceiverRecon ::
  Id DM.Merchant ->
  ReceiverReconRequest ->
  Flow ()
handleReceiverRecon merchantId req = do
  now <- getCurrentTime
  let merchantIdText = Just merchantId.getId

  -- Step 1: Group by UTR and upsert ReconUtrSettlement
  let allDetailPairs = concatMap (\o -> map (o,) o.settlementDetails) req.orders
      utrGroups = groupDetailsByUtr allDetailPairs

  utrIdMap <- fmap Map.fromList . forM (Map.toList utrGroups) $ \(utrVal, pairs) -> do
    let claimedTotal = sum $ map ((.amount) . snd) pairs
        pairCount = length pairs
    utrId <- generateGUID
    let utrSettlement =
          RUS.ReconUtrSettlement
            { id = utrId,
              merchantId = merchantIdText,
              merchantOperatingCityId = Nothing,
              utr = utrVal,
              bapId = req.bapId,
              bapUri = req.bapUri,
              claimedTotalAmount = claimedTotal,
              totalOrders = pairCount,
              bankVerifiedAmount = Nothing,
              resolutionStatus = RUS.RES_PENDING,
              sendStatus = RUS.SEND_PENDING,
              deadline = req.deadline,
              resolvedAt = Nothing,
              resolvedBy = Nothing,
              sentAt = Nothing,
              sendAttempts = 0,
              deadlineBreachedNotifiedAt = Nothing,
              createdAt = now,
              updatedAt = now
            }
    QRUSExtra.upsertByUtr utrSettlement
    existingUtr <- QRUS.findByUtr utrVal
    let actualId = maybe utrId (.id) existingUtr
    pure (utrVal, actualId)

  -- Step 2: Create RSO rows (raw claims only — no ride resolution)
  orderRows <- fmap concat . forM (zip [0 ..] req.orders) $ \(orderIdx, order) -> do
    let details =
          if null order.settlementDetails
            then [fallbackDetail]
            else order.settlementDetails

    forM (zip [0 ..] details) $ \(detailIdx, detail) -> do
      let thisUtrSettlementId = fromMaybe (Id "") $ Map.lookup detail.utr utrIdMap
      rowId <- generateGUID
      pure
        RSO.ReconSettlementOrder
          { id = rowId,
            merchantId = merchantIdText,
            merchantOperatingCityId = Nothing,
            utrSettlementId = Just thisUtrSettlementId,
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
            allocatedBankCash = Nothing,
            reconciliationStatus = Nothing,
            wireReconStatus = order.wireReconStatus,
            wireOrderReconStatus = order.wireOrderReconStatus,
            orderSequence = orderIdx,
            settlementDetailIndex = detailIdx,
            settlementClearedAt = Nothing,
            correctionForOrderRowId = Nothing,
            refundStatus = Nothing,
            manuallyConfirmedAt = Nothing,
            manuallyConfirmedBy = Nothing,
            manualConfirmationReason = Nothing,
            ourReconStatus = RSO.PENDING,
            diffAmount = Nothing,
            remarks = Nothing,
            payoutEligible = Nothing,
            refundReference = Nothing,
            refundedAt = Nothing,
            rawJson = order.rawJson,
            createdAt = now,
            updatedAt = now
          }

  QRSO.createMany orderRows

  -- Step 3: Inline reconciliation via Recipe framework
  let scope = ReconT.MerchantScope merchantId.getId merchantId.getId
      sourceIds = map (ReconT.SourceId . (.orderId)) req.orders
  unless (null sourceIds) $
    ReconRunner.reconcileSources RsfRecipe.recipe scope sourceIds

  logInfo $
    "RSF ingest + recon complete: messageId="
      <> req.messageId
      <> " rows="
      <> show (length orderRows)
      <> " orders="
      <> show (length req.orders)

groupDetailsByUtr :: [(ReceiverReconOrder, SettlementDetailParsed)] -> Map.Map Text [(ReceiverReconOrder, SettlementDetailParsed)]
groupDetailsByUtr = foldl' (\acc pair -> Map.insertWith (<>) ((.utr) . snd $ pair) [pair] acc) Map.empty

fallbackDetail :: SettlementDetailParsed
fallbackDetail =
  SettlementDetailParsed
    { utr = "",
      amount = 0,
      status = "",
      sdSettlementType = "",
      sdSettlementDate = UTCTime (toEnum 0) 0
    }
