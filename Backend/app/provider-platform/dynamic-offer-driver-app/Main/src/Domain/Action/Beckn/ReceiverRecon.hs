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
import Data.Time.Clock (UTCTime (..))
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

ingestReceiverRecon ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ReceiverReconRequest ->
  Flow ()
ingestReceiverRecon merchantId merchantOpCityId req =
  -- Serializes the idempotency check below (read-then-decide, not atomic on
  -- its own) against a second receiver_recon call touching the same order
  -- landing before the first one has committed its rows.
  acquireAll lockPrefix orderIds ingestBody
  where
    -- Sorted + deduped: a canonical acquisition order across all concurrent
    -- callers rules out a circular wait -- without this, two messages that
    -- share two orders but list them in opposite order could each hold one
    -- lock while waiting on the other, deadlocking until TTL expiry.
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

      -- Idempotency: a (orderId, UTR) pair already recorded by an earlier message
      -- (or repeated within this one) is a no-op resend -- e.g. a BAP correction
      -- resends the full settlement_details array with one new (often negative)
      -- UTR added alongside unchanged old ones. Re-creating a row for the
      -- unchanged ones would double the UTR's claimed total via upsertByUtr's
      -- accumulation, and would let a later recompute reach back into an
      -- already-SENT claim.
      existingRows <- QRSOExtra.findByOrderIds orderIds
      let existingKeys = HS.fromList [(r.orderId, r.settlementReferenceNo) | r <- existingRows]

          orderDetailsWithIdx :: [(Int, ReceiverReconOrder, Int, SettlementDetailParsed)]
          orderDetailsWithIdx =
            [ (orderIdx, order, detailIdx, detail)
              | (orderIdx, order) <- zip [0 ..] req.orders,
                let details = if null order.settlementDetails then [fallbackDetail] else order.settlementDetails,
                (detailIdx, detail) <- zip [0 ..] details
            ]

          (_, newOrderDetailsRev) =
            foldl'
              ( \(seen, acc) item@(_, order, _, detail) ->
                  let key = (order.orderId, detail.utr)
                   in if HS.member key seen
                        then (seen, acc)
                        else (HS.insert key seen, item : acc)
              )
              (existingKeys, [])
              orderDetailsWithIdx
          newOrderDetails = reverse newOrderDetailsRev
          skippedCount = length orderDetailsWithIdx - length newOrderDetails

      when (skippedCount > 0) $
        logWarning $
          "RSF ingest: skipped "
            <> show skippedCount
            <> " already-claimed (order,UTR) pair(s), messageId="
            <> req.messageId

      -- Step 1: Group by UTR and upsert ReconUtrSettlement (new claims only)
      let allDetailPairs = [(order, detail) | (_, order, _, detail) <- newOrderDetails]
          utrGroups = groupDetailsByUtr allDetailPairs

      utrIdMap <- fmap Map.fromList . forM (Map.toList utrGroups) $ \(utrVal, pairs) -> do
        let claimedTotal = sum $ map ((.amount) . snd) pairs
            pairCount = length pairs
        utrId <- generateGUID
        let utrSettlement =
              RUS.ReconUtrSettlement
                { id = utrId,
                  merchantId = merchantIdText,
                  merchantOperatingCityId = merchantOpCityIdText,
                  utr = utrVal,
                  bapId = req.bapId,
                  bapUri = req.bapUri,
                  claimedTotalAmount = claimedTotal,
                  totalOrders = pairCount,
                  bankVerifiedAmount = Nothing,
                  resolutionStatus = RUS.RES_PENDING,
                  resolvedAt = Nothing,
                  resolvedBy = Nothing,
                  createdAt = now,
                  updatedAt = now
                }
        QRUSExtra.upsertByUtr utrSettlement
        existingUtr <- QRUS.findByUtr utrVal
        let actualId = maybe utrId (.id) existingUtr
        pure (utrVal, actualId)

      -- Step 2: Create RSO rows (raw claims only — no ride resolution). Only
      -- the newly-seen (order, UTR) pairs from the idempotency filter above --
      -- an already-claimed pair is left completely untouched.
      orderRows <- forM newOrderDetails $ \(orderIdx, order, detailIdx, detail) -> do
        let thisUtrSettlementId = fromMaybe (Id "") $ Map.lookup detail.utr utrIdMap
        rowId <- generateGUID
        pure
          RSO.ReconSettlementOrder
            { id = rowId,
              merchantId = merchantIdText,
              merchantOperatingCityId = merchantOpCityIdText,
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
              -- Stable only within this message, not globally across every
              -- message that ever contributes a row to the same UTR -- a
              -- later correction adding a new order to an *existing multi-
              -- order* UTR gets its sequence numbers fresh from 0, which can
              -- collide with sequence numbers that UTR's earlier rows already
              -- used. Accepted for now: none of the current flows add a new
              -- order to an already-multi-order UTR in a later message (the
              -- correction flows use fresh single-order UTRs; the waterfall
              -- UTR's orders all arrive in one message). Revisit if that
              -- combination becomes a real scenario.
              orderSequence = orderIdx,
              settlementDetailIndex = detailIdx,
              settlementClearedAt = Nothing,
              manuallyConfirmedAt = Nothing,
              manuallyConfirmedBy = Nothing,
              manualConfirmationReason = Nothing,
              ourReconStatus = RSO.PENDING,
              diffAmount = Nothing,
              remarks = Nothing,
              payoutEligible = Nothing,
              rawJson = order.rawJson,
              createdAt = now,
              updatedAt = now
            }

      QRSO.createMany orderRows

      logInfo $
        "RSF ingest + recon complete: messageId="
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

fallbackDetail :: SettlementDetailParsed
fallbackDetail =
  SettlementDetailParsed
    { utr = "",
      amount = 0,
      status = "",
      sdSettlementType = "",
      sdSettlementDate = UTCTime (toEnum 0) 0
    }
