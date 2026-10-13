{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Domain.Action.Beckn.ReceiverRecon
  ( ReceiverReconRequest (..),
    ReceiverReconOrder (..),
    SettlementDetailParsed (..),
    runReceiverReconPass,
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as L
import qualified Lib.Finance.Reconciliation.Runner as ReconRunner
import qualified Lib.Finance.Reconciliation.Types as ReconT
import qualified Lib.Finance.Storage.Queries.RsfOrderState as QOrderState
import qualified Lib.Finance.Storage.Queries.RsfReconLedgerEntry as QLedger
import qualified Lib.Finance.Storage.Queries.RsfUtrState as QUtrState
import qualified SharedLogic.Finance.Reconciliation.Recipes.RsfBapClaimVsPlatformRide as RsfRecipe
import qualified SharedLogic.Finance.Reconciliation.Recipes.RsfUtrClaimVsBankReceipt as RsfUtrRecipe
import qualified SharedLogic.RSFLedger as RSFLedger

data ReceiverReconRequest = ReceiverReconRequest
  { bapId :: Text,
    bapUri :: Text,
    messageId :: Text,
    reconTransactionId :: Text,
    ttl :: Maybe Text,
    contextTimestamp :: UTCTime,
    rawJson :: Text,
    orders :: [ReceiverReconOrder]
  }

data ReceiverReconOrder = ReceiverReconOrder
  { orderId :: Text,
    orderTransactionId :: Text,
    invoiceNo :: Maybe Text,
    collectorAppId :: Maybe Text,
    orderState :: Text,
    claimedGrossAmount :: HighPrecMoney,
    paymentStatus :: Text,
    transactionStatus :: Maybe Text,
    settlementId :: Text,
    reasonCode :: Text,
    bffType :: Maybe Text,
    bffAmount :: Maybe HighPrecMoney,
    withholdingTaxGst :: Maybe HighPrecMoney,
    withholdingTaxTds :: Maybe HighPrecMoney,
    deductionByCollector :: Maybe HighPrecMoney,
    settlementDetails :: [SettlementDetailParsed]
  }

data SettlementDetailParsed = SettlementDetailParsed
  { utr :: Text,
    amount :: HighPrecMoney,
    settlementTimestamp :: UTCTime
  }

-- Phase 2 (async, steps 4-9): persist first, then let each stage settle claim_status.
--   4. MESSAGE_RECEIVED envelope row
--   5. stage A per order (payload only): payment is PAID, Σ legs == payment.params.amount
--   6. one BAP_CLAIM per settlement leg -- PENDING if stage A passed, else straight to its rejection
--   7. stage B: recon-framework pass (fare, then finder fee) settles only the failures
--   8. stage C: closed-UTR check, then the survivors become ACCEPTED
--   9. MESSAGE_PROCESSED -- the last write; Phase 4 will not send without it
-- Rides come in from Phase 1 (E4 already resolved every order id).
runReceiverReconPass ::
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  ReceiverReconRequest ->
  Map.Map Text DRide.Ride ->
  Flow ()
runReceiverReconPass merchantId merchantOpCityId req rideByOrderId =
  Hedis.withLockRedis ("RsfIngest:" <> merchantId.getId <> ":" <> req.messageId) 300 $ do
    alreadyReceived <- QLedger.findMessageReceived merchantId.getId req.messageId
    if isJust alreadyReceived
      then logWarning $ "RSF ingest: messageId=" <> req.messageId <> " already in the ledger, skipping"
      else do
        now <- getCurrentTime
        let mid = merchantId.getId
            mocId = Just merchantOpCityId.getId
            orderIds = nub $ map (.orderId) req.orders

        -- step 4
        envelope <- RSFLedger.baseEntry mid mocId L.MESSAGE_RECEIVED L.BAP_CLAIMED 0 req.contextTimestamp
        QLedger.create
          envelope
            { L.messageId = Just req.messageId,
              L.contextTransactionId = Just req.reconTransactionId,
              L.bapId = Just req.bapId,
              L.bapUri = Just req.bapUri,
              L.ttl = req.ttl,
              L.rawJson = Just req.rawJson
            }

        -- steps 5 + 6. A leg already ACCEPTED with the same amount is a re-send: no-op, never re-judged.
        priorEntries <- QLedger.findAllByMerchantAndOrderIds mid orderIds
        let priorAccepted = RSFLedger.acceptedClaims priorEntries
            isResend order leg = ((.amount) <$> Map.lookup (order.orderId, leg.utr) priorAccepted) == Just leg.amount
            legsToWrite =
              [ (order, leg, stageA)
                | order <- req.orders,
                  let stageA = runStageA order,
                  leg <- order.settlementDetails,
                  isJust stageA || not (isResend order leg)
              ]
        claimRows <- forM (zip [0 :: Int ..] legsToWrite) $ \(idx, (order, leg, stageA)) -> do
          row <- RSFLedger.baseEntry mid mocId L.BAP_CLAIM L.BAP_CLAIMED leg.amount leg.settlementTimestamp
          let mbRide = Map.lookup order.orderId rideByOrderId
          pure
            row{L.messageId = Just req.messageId,
                L.orderId = Just order.orderId,
                L.utr = Just leg.utr,
                L.orderPaymentAmount = Just order.claimedGrossAmount,
                L.bffType = order.bffType,
                L.bffAmount = order.bffAmount,
                L.withholdingTaxGst = order.withholdingTaxGst,
                L.withholdingTaxTds = order.withholdingTaxTds,
                L.deductionByCollector = order.deductionByCollector,
                L.settlementId = Just order.settlementId,
                L.orderTransactionId = Just order.orderTransactionId,
                L.invoiceNo = order.invoiceNo,
                L.collectorAppId = order.collectorAppId,
                L.orderState = Just order.orderState,
                L.settlementReasonCode = Just order.reasonCode,
                L.rideId = (.id.getId) <$> mbRide,
                L.driverId = (.driverId.getId) <$> mbRide,
                L.claimStatus = Just (maybe L.PENDING fst stageA),
                L.rejectionDiff = snd <$> stageA,
                -- microsecond offsets keep payload (claim) order recoverable from createdAt
                L.createdAt = addUTCTime (fromIntegral idx / 1000000) now -- Need Attention
               }
        QLedger.createMany claimRows
        -- the two projections get their (status NULL) row the first time an order / UTR is seen;
        -- the dashboard pages over them, Phase 4 stamps them
        QOrderState.createMissing mid (Just req.bapId) now orderIds
        QUtrState.createMissing mid (Just req.bapId) now (mapMaybe (.utr) claimRows)

        -- step 7. One lock for the whole message (held above), not one per order.
        let pendingOrderIds = nub [oid | r <- claimRows, r.claimStatus == Just L.PENDING, Just oid <- [r.orderId]]
        unless (null pendingOrderIds) $
          ReconRunner.processChunkByIds RsfRecipe.recipe (ReconT.MerchantScope mid merchantOpCityId.getId) pendingOrderIds

        -- step 8
        unjudged <- runStageC mid merchantOpCityId.getId req.messageId rideByOrderId

        -- step 9. A ride still in flight leaves its claim PENDING and the message visibly incomplete.
        if unjudged > 0
          then logError $ "RSF ingest: messageId=" <> req.messageId <> " left " <> show unjudged <> " claim(s) PENDING (ride not terminal), MESSAGE_PROCESSED not written"
          else do
            processed <- RSFLedger.baseEntry mid mocId L.MESSAGE_PROCESSED L.SYSTEM_JOB 0 now
            QLedger.create processed {L.messageId = Just req.messageId}
        logInfo $ "RSF ingest complete: messageId=" <> req.messageId <> " claims=" <> show (length claimRows) <> " orders=" <> show (length req.orders)

-- Stage A (checks 1 and 2): payload only, no I/O. Nothing = passed.
-- rejectionDiff for check 2 is signed: payment.params.amount − Σ legs.
runStageA :: ReceiverReconOrder -> Maybe (L.RsfClaimStatus, HighPrecMoney)
runStageA order
  | not (isPaid order.paymentStatus && maybe True isPaid order.transactionStatus) = Just (L.REJECTED_NOT_PAID, 0)
  | legSum /= order.claimedGrossAmount = Just (L.REJECTED_DETAIL_SUM, order.claimedGrossAmount - legSum)
  | otherwise = Nothing
  where
    isPaid = (== "PAID") . T.toUpper
    legSum = sum $ map (.amount) order.settlementDetails

-- Stage C (check 5): a UTR already bank-verified by us must still have the money to cover the
-- new legs this message puts on it -- Σ new legs <= received − allocated. That
-- comparison is a recon-framework pass (RsfUtrClaimVsBankReceipt); its write-back rejects the
-- UTR's PENDING claims as REJECTED_UTR_IMBALANCE. Whatever is still PENDING afterwards becomes
-- ACCEPTED -- written exactly once, here. Money is handed out later by autoAllocate.
-- Returns how many claims stayed PENDING (ride in flight).
runStageC :: Text -> Text -> Text -> Map.Map Text DRide.Ride -> Flow Int
runStageC mid mocId messageId rideByOrderId = do
  let pendingClaims entries = [e | e <- entries, e.entryType == L.BAP_CLAIM, e.claimStatus == Just L.PENDING]
      hasFare e = isJust $ e.orderId >>= (`Map.lookup` rideByOrderId) >>= RSFLedger.rideFare
  pending <- pendingClaims <$> QLedger.findAllByMerchantAndMessageId mid messageId
  let (judgeable, inFlight) = (filter hasFare pending, filter (not . hasFare) pending)
  -- a UTR is "already in the system" once finance has bank-verified it (it has a BANK_RECEIPT);
  -- the rsf_utr_state row alone is not enough, it is written at ingest for every UTR
  receipts <- QLedger.findAllByMerchantEntryTypesAndUtrs mid [L.BANK_RECEIPT] (nub $ mapMaybe (.utr) judgeable)
  let verifiedUtrs = nub (mapMaybe (.utr) receipts)
  unless (null verifiedUtrs) $
    ReconRunner.processChunkByIds (RsfUtrRecipe.recipe messageId) (ReconT.MerchantScope mid mocId) verifiedUtrs
  survivors <- filter hasFare . pendingClaims <$> QLedger.findAllByMerchantAndMessageId mid messageId
  forM_ survivors $ \e -> QLedger.updateClaimStatus e.id L.ACCEPTED Nothing
  pure (length inFlight)
